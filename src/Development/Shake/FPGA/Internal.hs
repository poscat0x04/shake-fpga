module Development.Shake.FPGA.Internal
  ( -- * Types
    HDL (..),
    Target (..),
    TargetRef,

    -- * Build Configuration
    BuildConfig (..),
    readBuildConfig,
    CompiledBuildConfig (..),
    compile,

    -- * Shake rules from configuration
    rulesFor,
    rulesFromFile,
    shakeFromFile,

    -- * Query types used for @addOracle@
    StrProp (..),
    StrPropQuery (..),
    BoolProp (..),
    BoolPropQuery (..),
    HDLQuery (..),
    ToolChain (..),
    ToolChainQuery (..),

    -- * Utilities
    lookupToolChain,
  )
where

import Clash.Driver.Manifest (Manifest (..), ManifestPort (..), PortDirection (..), readManifest)
import Clash.Main (defaultMain)
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    defaultOptions,
    genericParseJSON,
  )
import Data.Aeson.TH (deriveFromJSON)
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.List (intercalate, nub, partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.String.Interpolate (__i)
import Data.Text (pack, unpack)
import Data.Text qualified as T
import Data.Yaml (decodeFileEither)
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
import Development.Shake.FPGA.DirStructure
import Development.Shake.FPGA.Utils (Components, DBool (..), buildDir, shakeOpts)
import Development.Shake.FilePath (takeExtension, (<.>), (</>))
import Development.Shake.Util (parseMakefile)
import GHC.Generics (Generic)
import System.Directory (findExecutable, makeAbsolute)

data HDL
  = Verilog
  | SystemVerilog
  | VHDL
  deriving (Eq, Show, Generic, Hashable, Binary, NFData)

-- | A single clash target to eventually compile into a bitstream file
--   for an FPGA. Targets are top-level haskell definitions in a module.
--   hence, they are uniquely identified by the module name and the
--   identifier name (topEntity).
data Target
  = Target
  { targetModule :: String,
    targetTopEntity :: String,
    -- | The FPGA hardware part to target
    targetPart :: String,
    -- | The supplied constraint file
    targetXDC :: String,
    -- | Alias for the target, will be used to generate phony rules if set
    targetAlias :: Maybe String,
    -- | Whether to start the Vivado GUI when synthesizing
    targetGUI :: DBool 'False,
    targetLinkComponents :: Components
  }
  deriving (Eq, Show, Generic)

type TargetRef = (String, String)

-- | Configuration for a list of targets to build
data BuildConfig
  = BuildConfig
  { targets :: [Target],
    hdl :: HDL
  }
  deriving (Eq, Show, Generic)

compile :: BuildConfig -> CompiledBuildConfig
compile BuildConfig {..} =
  CompiledBuildConfig
    { targetsMap = M.fromList [((targetModule, targetTopEntity), t) | t@Target {..} <- targets],
      targets = [(targetModule, targetTopEntity) | Target {..} <- targets],
      ..
    }

data CompiledBuildConfig
  = CompiledBuildConfig
  { targetsMap :: Map TargetRef Target,
    targets :: [TargetRef],
    hdl :: HDL
  }
  deriving (Eq, Show, Generic)

rulesFor :: CompiledBuildConfig -> Rules ()
rulesFor CompiledBuildConfig {..} = do
  -- So that shake can rebuild when properties change
  _ <- addOracle $ \(StrPropQuery p tref) -> do
    let Target {..} = fromJust $ M.lookup tref targetsMap
    pure $ case p of
      Part -> targetPart
      XDC -> targetXDC
  _ <- addOracle $ \(BoolPropQuery p tref) -> do
    let Target {..} = fromJust $ M.lookup tref targetsMap
    pure $ case p of
      GUI -> coerce targetGUI
  _ <- addOracle $ \HDLQuery -> pure hdl
  _ <- addOracle $ \ToolChainQuery -> lookupToolChain ToolChainQuery
  _ <- addOracle $ \VerilatorFlagsQuery -> do
    mPkgConfig <- liftIO $ findExecutable "pkg-config"
    case mPkgConfig of
      Nothing -> fail "pkg-config not found"
      Just pkgConfig -> do
        StdoutTrim out <- cmd pkgConfig "--cflags" "verilator"
        pure out

  let mods = nub $ map fst targets

  forM_ mods $ \modName ->
    rulesForModule modName

  forM_ targets $ \t ->
    rulesForTarget t $ do
      Target {..} <- M.lookup t targetsMap
      targetAlias
  where
    clash :: [String] -> Action ()
    clash = liftIO . defaultMain . (["-isrc"] <>)

    rulesForModule :: String -> Rules ()
    rulesForModule modName = do
      depMakefile modName %> \out -> do
        alwaysRerun
        withTempFile $ \temp -> do
          clash ["-M", "-dep-suffix", "", "-dep-makefile", temp, modName]
          copyFileChanged temp out

    rulesForTarget :: TargetRef -> Maybe String -> Rules ()
    rulesForTarget tref@(modName, topEntity) mAlias = do
      let DirStructure {..} = dirsOf tref
      let BuildOutputLayout {..} = buildOutputsOf tref
      manifestFile %> \_out -> do
        -- To correctly specify dependencies
        need [depMakefile modName]
        mkFile <- liftIO $ readFile $ depMakefile modName
        let deps = parseMakefile mkFile
        let hsSrcs = [src | (_, srcs) <- deps, src <- srcs, takeExtension src == ".hs"]
        need hsSrcs

        _hdl <- askOracle HDLQuery
        -- synthesize using clash
        withTempDir $ \temp -> do
          clash $
            clashFlagOf hdl
              <> ["-outputdir", temp]
              <> ["-fclash-compile-ultra"]
              <> ["-main-is", topEntity, modName]
          liftIO $ removeFiles clashDir ["//*"]
          let prodDir = temp </> modName <.> topEntity
          products <- liftIO $ getDirectoryFilesIO prodDir ["//*"]
          forM_ products $ \file -> do
            let src = prodDir </> file
            let dst = clashDir </> file
            copyFile' src dst

      tclScript %> \out -> do
        need [manifestFile]

        -- Determine the HDL sources from clash-manifest.json
        _hdl <- askOracle HDLQuery
        Just Manifest {..} <- liftIO $ readManifest manifestFile
        hdlSrcs <-
          mapM makeAbsolute' $
            [ clashDir </> src
              | (src, _) <- fileNames,
                takeExtension src == "." <> extOf hdl
            ]

        part <- askOracle $ StrPropQuery Part tref
        xdcFile <- askOracle (StrPropQuery XDC tref) >>= makeAbsolute'
        gui <- askOracle $ BoolPropQuery GUI tref

        let file =
              [__i|
          set_param general.maxThreads 8
          #{if gui then "start_gui" else ""}

          #{readCommandOf hdl} #{unwords hdlSrcs}
          read_xdc #{xdcFile}

          if {[file exists post_synth.dcp]} {
            read_checkpoint -incremental -auto_incremental post_synth.dcp
          }
          synth_design -top #{topComponent} -part #{part}
          write_checkpoint -force post_synth.dcp
          report_timing_summary -file post_synth_timing_summary.txt -rpx post_synth_timing_summary.rpx
          report_power -file post_synth_power.txt -rpx post_synth_power.rpx

          if {[file exists post_place.dcp]} {
            read_checkpoint -incremental -auto_incremental post_place.dcp
          }
          opt_design
          place_design
          phys_opt_design
          write_checkpoint -force post_place.dcp
          report_timing_summary \\
            -file post_place_timing_summary.txt \\
            -rpx post_place_timing_summary.rpx

          if {[file exists post_route.dcp]} {
            read_checkpoint -incremental -auto_incremental post_route.dcp
          }
          route_design
          write_checkpoint -force post_route.dcp
          report_timing_summary \\
            -file post_route_timing_summary.txt \\
            -rpx post_route_timing_summary.rpx
          report_timing \\
            -sort_by group \\
            -max_paths 100 \\
            -path_type summary \\
            -file post_route_timing.txt \\
            -rpx post_route_timing.rpx
          report_clock_utilization -file clock_util.txt
          report_utilization -file post_route_util.txt
          report_power -file post_route_power.txt

          write_bitstream -force out.bit
        |]
        liftIO $ writeFile out file

      bitStreamFile %> \_out -> do
        need [tclScript]
        script <- makeAbsolute' tclScript
        cmd_ (Cwd vivadoDir) "vivado" "-mode" "batch" "-source" script

      [libVmodelA, libverilatedA] &%> \_out -> do
        need [manifestFile]
        Just Manifest {..} <- liftIO $ readManifest manifestFile

        when (any (\ManifestPort {..} -> mpDirection == InOut) ports) $
          fail "inout ports are not supported on the boundary"

        ToolChain {..} <- askOracle ToolChainQuery
        let top = unpack topComponent

        cmd_
          "verilator --cc --build --prefix Vmodel"
          -- use all threads for compilation
          "-j 0"
          -- generate FST waveform
          "--trace-fst --trace-structs"
          -- optimization flags for verilator codegen
          -- see: https://verilator.org/guide/latest/simulating.html#benchmarking-optimization
          "-O3 --x-assign fast --x-initial fast --noassert"
          -- c++ compiler flags
          ["--MAKEFLAGS", [__i|CXX=#{cxx} CC=#{cc} OPT_FAST="-O3"|]]
          "--CFLAGS -fPIC"
          -- clash currently generates verilog 2001 and systemverilog 2012
          -- see: https://clash-lang.readthedocs.io/en/latest/developing-hardware/flags.html
          "+1364-2001ext+v"
          "+1800-2012ext+sv"
          "-Mdir"
          verilatorDir
          "-y"
          clashDir
          "--top-module"
          top
          top

      vmodelCH %> \out -> do
        need [manifestFile]
        Just Manifest {..} <- liftIO $ readManifest manifestFile

        let (_, inNonClockPorts, outPorts) = classifyPorts ports

        inputMembers <- fmap unlines $ forM inNonClockPorts $ \p@ManifestPort {..} -> do
          rep <- guessPortRep p
          pure [__i|#{cTypeOf rep} #{mpName};|]
        outputMembers <- fmap unlines $ forM outPorts $ \p@ManifestPort {..} -> do
          rep <- guessPortRep p
          pure [__i|#{cTypeOf rep} #{mpName};|]

        let source =
              [__i|
          \#pragma once
          \#ifdef __cplusplus
          \#include "Vmodel.h"
          \#include <memory>
          \#else
          \#include <stdint.h>
          \#endif


          struct Input {
          #{inputMembers}
          };

          struct Output {
          #{outputMembers}
          };

          \#ifdef __cplusplus
          class SimModel final {
            Vmodel* model_p;
            VerilatedContext* context_p;
          public:
            SimModel();
            ~SimModel();
            void step(const Input *inp, Output *out);
          };
          \#else
          struct SimModel;
          typedef struct SimModel SimModel;
          typedef struct Input Input;
          typedef struct Output Output;
          \#endif

          \#ifdef __cplusplus
          extern "C" {
          \#endif
            SimModel* startModel();
            void stopModel(SimModel* model);
            void stepModel(SimModel* model, Input* inp, Output* out);
          \#ifdef __cplusplus
          }
          \#endif
        |]
        liftIO $ writeFile out source

      vmodelCCPP %> \out -> do
        need [manifestFile]
        Just Manifest {..} <- liftIO $ readManifest manifestFile

        let (inClockPorts, inNonClockPorts, outPorts) = classifyPorts ports
        clock <- case inClockPorts of
          [p] -> pure p
          -- TODO: add combinatorial circuit support
          [] -> fail "no clock input found: combinatorial circuits are currently not supported"
          -- TODO: add multi domain clock input support
          _ -> fail "multiple clock inputs found: multi domain clock inputs are currently not supported"

        let setInput = unlines $ inNonClockPorts <&> \ManifestPort {..} -> [__i|model_p->#{mpName} = inp->#{mpName};|]
        let readOutput = unlines $ outPorts <&> \ManifestPort {..} -> [__i|out->#{mpName} = model_p->#{mpName};|]

        let source =
              [__i|
          \#include "Vmodel-c.h"

          SimModel::SimModel() {
            context_p = new VerilatedContext();
            model_p = new Vmodel(context_p, "model");
          }

          SimModel::~SimModel() {
            delete model_p;
            delete context_p;
          };

          void SimModel::step(const Input *inp, Output *out) {
            // set input
          #{setInput}
            // tick clock
            model_p->#{mpName clock} = true;
            model_p->eval();
            context_p->timeInc(1);
            model_p->#{mpName clock} = false;
            model_p->eval();
            context_p->timeInc(1);
            // read output
          #{readOutput}
          }

          extern "C" {
            SimModel* startModel() {
              return new SimModel();
            }
            void stopModel(SimModel* model) {
              delete model;
            }
            void stepModel(SimModel* model, Input* inp, Output* out) {
              model->step(inp, out);
            }
          }
        |]
        liftIO $ writeFile out source

      vmodelCO %> \out -> do
        need [vmodelCCPP, vmodelCH, libVmodelA]
        ToolChain {..} <- askOracle ToolChainQuery
        incFlags <- askOracle VerilatorFlagsQuery
        cmd_ cxx incFlags "-c" vmodelCCPP "-o" out

      libVmodelCA %> \out -> do
        need [vmodelCO, libVmodelA, libverilatedA]
        ToolChain {..} <- askOracle ToolChainQuery
        let mirScript =
              [__i|
          create #{out}
          addlib #{libVmodelA}
          addlib #{libverilatedA}
          addmod #{vmodelCO}
          save
          end
        |]
        cmd_ "ar -M" (StdinBS mirScript)

      fullVmodelCO %> \out -> do
        need [libVmodelCA]
        ToolChain {..} <- askOracle ToolChainQuery
        cmd_ ld "-r -o" out "--whole-archive" libVmodelCA

      verilatedHSC %> \out -> do
        need [manifestFile, vmodelCH]
        Just Manifest {..} <- liftIO $ readManifest manifestFile

        let (_, inNonClockPorts, outPorts) = classifyPorts ports
        inputFields <- fmap (intercalate ",\n") $ forM inNonClockPorts $ \ManifestPort {..} -> do
          rep <- guessPortRep ManifestPort {..}
          pure [__i|#{T.toLower mpName} :: #{haskellTypeOf rep}|]
        let inputPeek =
              unwords $
                inNonClockPorts <&> \ManifestPort {..} ->
                  [__i|<*> (\#peek Input, #{mpName}) ptr|]
        let inputPoke =
              unwords $
                inNonClockPorts <&> \ManifestPort {..} ->
                  [__i|(\#poke Input, #{mpName}) ptr #{T.toLower mpName};|]

        outputFields <- fmap (intercalate ",\n") $ forM outPorts $ \ManifestPort {..} -> do
          rep <- guessPortRep ManifestPort {..}
          pure [__i|#{T.toLower mpName} :: #{haskellTypeOf rep}|]
        let outputPeek =
              unwords $
                outPorts <&> \ManifestPort {..} ->
                  [__i|<*> (\#peek Output, #{mpName}) ptr|]
        let outputPoke =
              unwords $
                outPorts <&> \ManifestPort {..} ->
                  [__i|(\#poke Output, #{mpName}) ptr #{T.toLower mpName};|]

        let source =
              [__i|
          {-\# LANGUAGE ForeignFunctionInterface \#-}
          {-\# LANGUAGE RecordWildCards \#-}
          {-\# LANGUAGE DeriveGeneric \#-}
          module Verilated where

          import Prelude
          import GHC.Generics
          import Data.Word
          import Data.Int
          import Foreign.Storable
          import Foreign.Ptr

          data Input = Input
            {
          #{inputFields}
            }
            deriving (Show, Eq, Generic)

          data Output = Output
            {
          #{outputFields}
            }
            deriving (Show, Eq, Generic)

          data SimModel

          foreign import ccall unsafe "startModel" c_startModel :: IO (Ptr SimModel)
          foreign import ccall unsafe "stopModel" c_stopModel :: Ptr SimModel -> IO ()
          foreign import ccall unsafe "stepModel" c_stepModel :: Ptr SimModel -> Ptr Input -> Ptr Output -> IO ()

          \#include "Vmodel-c.h"

          instance Storable Input where
            sizeOf _ = \#size Input
            alignment _ = \#alignment Input
            peek ptr = (pure Input) #{inputPeek}
            poke ptr Input{..} = do
              { #{inputPoke} pure ()
              }

          instance Storable Output where
            sizeOf _ = \#size Output
            alignment _ = \#alignment Output
            peek ptr = (pure Output) #{outputPeek}
            poke ptr Output{..} = do
              { #{outputPoke} pure ()
              }
        |]
        liftIO $ writeFile out source

      -- phony rules when aliases are defined
      forM_ mAlias $ \alias -> do
        let tgt n = alias <> ":" <> n
        let bitT = tgt "bit"
        let verilateT = tgt "verilate"
        let clashT = tgt "clash"
        phony bitT $ need [bitStreamFile]
        phony verilateT $ need [libVmodelA]
        phony clashT $ need [manifestFile]
        phony alias $
          need [bitT, verilateT, clashT]
      where
        clashFlagOf :: HDL -> [String]
        clashFlagOf = \case
          Verilog -> ["--verilog"]
          SystemVerilog -> ["--systemverilog"]
          VHDL -> ["--vhdl"]

        extOf :: HDL -> String
        extOf = \case
          Verilog -> "v"
          SystemVerilog -> "sv"
          VHDL -> "vhdl"

        readCommandOf :: HDL -> String
        readCommandOf = \case
          Verilog -> "read_verilog"
          SystemVerilog -> "read_verilog -sv"
          VHDL -> "read_vhdl"

        makeAbsolute' :: FilePath -> Action FilePath
        makeAbsolute' = liftIO . makeAbsolute

        classifyPorts :: [ManifestPort] -> ([ManifestPort], [ManifestPort], [ManifestPort])
        classifyPorts ps = (inClockPorts, inNonClockPorts, outPorts)
          where
            (inClockPorts, inNonClockPorts) = partition mpIsClock inPorts
            inPorts = filter (\ManifestPort {..} -> mpDirection == In) ps
            outPorts = filter (\ManifestPort {..} -> mpDirection == Out) ps

        guessPortRep :: ManifestPort -> Action Rep
        guessPortRep ManifestPort {..} = do
          let signed = pack "signed" `T.isInfixOf` mpTypeName
          width <- guessWidth mpWidth
          pure Rep {..}
          where
            guessWidth w
              | w <= 8 = pure W8
              | w <= 16 = pure W16
              | w <= 32 = pure W32
              | w <= 64 = pure W64
              | otherwise = fail $ "unsupported width of port " <> unpack mpName <> ": " <> show w

        -- signess doesn't matter anyways since we're doing FFI
        cTypeOf :: Rep -> String
        cTypeOf Rep {..} = case width of
          W8 -> "uint8_t"
          W16 -> "uint16_t"
          W32 -> "uint32_t"
          W64 -> "uint64_t"

        haskellTypeOf :: Rep -> String
        haskellTypeOf Rep {..} = case (width, signed) of
          (W8, False) -> "Word8"
          (W16, False) -> "Word16"
          (W32, False) -> "Word32"
          (W64, False) -> "Word64"
          (W8, True) -> "Int8"
          (W16, True) -> "Int16"
          (W32, True) -> "Int32"
          (W64, True) -> "Int64"

data StrProp
  = Part
  | XDC
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

data StrPropQuery
  = StrPropQuery
  { prop :: StrProp,
    target :: TargetRef
  }
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult StrPropQuery = String

data BoolProp
  = GUI
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

data BoolPropQuery
  = BoolPropQuery
  { prop :: BoolProp,
    target :: TargetRef
  }
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult BoolPropQuery = Bool

data HDLQuery = HDLQuery
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult HDLQuery = HDL

data ToolChainQuery
  = ToolChainQuery
  deriving (Eq, Show, Typeable, Generic, Hashable, Binary, NFData)

data ToolChain
  = ToolChain
  { cc :: FilePath,
    cxx :: FilePath,
    ld :: FilePath
  }
  deriving (Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult ToolChainQuery = ToolChain

data VerilatorFlagsQuery
  = VerilatorFlagsQuery
  deriving (Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult VerilatorFlagsQuery = String

data Width
  = W8
  | W16
  | W32
  | W64
  deriving (Show, Eq)

data Rep
  = Rep
  { width :: Width,
    signed :: Bool
  }
  deriving (Show, Eq)

lookupToolChain :: ToolChainQuery -> Action ToolChain
lookupToolChain _ = do
  mbLld <- findExecutable' "ld.lld"
  mbGold <- findExecutable' "ld.gold"
  mbLd <- findExecutable' "ld"
  mbClang <- findExecutable' "clang"
  mbCc <- findExecutable' "cc"
  mbClangxx <- findExecutable' "clang++"
  mbCxx <- findExecutable' "c++"
  ld <- (mbLld <|> mbGold <|> mbLd) `abort` "ld not found"
  cc <- (mbClang <|> mbCc) `abort` "cc not found"
  cxx <- (mbClangxx <|> mbCxx) `abort` "c++ not found"
  pure ToolChain {..}
  where
    findExecutable' = liftIO . findExecutable

    abort :: Maybe a -> String -> Action a
    abort m msg = do
      case m of
        Just x -> pure x
        Nothing -> fail msg

instance FromJSON HDL where
  omittedField = Just SystemVerilog
  parseJSON = genericParseJSON defaultOptions

$( deriveFromJSON
     defaultOptions
       { allowOmittedFields = True,
         omitNothingFields = True,
         fieldLabelModifier = \case
           "targetModule" -> "module"
           "targetTopEntity" -> "topEntity"
           "targetPart" -> "part"
           "targetXDC" -> "xdc"
           "targetAlias" -> "alias"
           "targetGUI" -> "gui"
           "targetLinkComponents" -> "linkComponents"
           x -> x
       }
     ''Target
 )
$( deriveFromJSON
     defaultOptions
       { allowOmittedFields = True,
         omitNothingFields = True
       }
     ''BuildConfig
 )

readBuildConfig :: FilePath -> IO BuildConfig
readBuildConfig file = do
  e <- decodeFileEither file
  case e of
    Left err -> fail $ "Failed to parse config file: " <> show err
    Right c -> pure c

rulesFromFile :: FilePath -> Rules ()
rulesFromFile file = do
  config <- liftIO $ readBuildConfig file
  rulesFor $ compile config

shakeFromFile :: FilePath -> IO ()
shakeFromFile file = shakeArgs shakeOpts $ do
  phony "clean" $ do
    putNormal $ "Cleaning files in " <> buildDir
    removeFilesAfter buildDir ["//*"]

  rulesFromFile file
