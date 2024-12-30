module Development.Shake.FPGA.Internal
  ( -- * Types
    HDL (..),
    Target (..),
    TargetRef,

    -- * Build Configuration
    BuildConfig (..),
    CompiledBuildConfig (..),
    compile,

    -- * Shake rules from configuration
    rulesFor,
    rulesFromFile,
    shakeFromFile,

    -- * Query types used for @addOracle@
    StrProp (..),
    StrPropQuery (..),
    HDLQuery (..),
  )
where

import Clash.Driver.Manifest (Manifest (..), readManifest)
import Clash.Main (defaultMain)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Aeson
  ( FromJSON (..),
    Options (..),
    defaultOptions,
    genericParseJSON,
  )
import Data.Aeson.TH (deriveFromJSON)
import Data.Data (Typeable)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.String.Interpolate (__i)
import Data.Text qualified as T
import Data.Yaml (decodeFileEither)
import Development.Shake
import Development.Shake.Classes (Binary, Hashable, NFData)
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
    targetAlias :: Maybe String
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
    ld :: FilePath,
    ar :: FilePath
  }
  deriving (Show, Eq, Typeable, Generic, Hashable, Binary, NFData)

type instance RuleResult ToolChainQuery = ToolChain

lookupToolChain :: ToolChainQuery -> Action ToolChain
lookupToolChain _ = do
  mbLd <- findExecutable' "ld"
  mbAr <- findExecutable' "ar"
  mbClang <- findExecutable' "clang"
  mbCc <- findExecutable' "cc"
  mbClangxx <- findExecutable' "clang++"
  mbCxx <- findExecutable' "c++"
  ld <- mbLd `abort` "ld not found"
  ar <- mbAr `abort` "ar not found"
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

rulesFor :: CompiledBuildConfig -> Rules ()
rulesFor CompiledBuildConfig {..} = do
  -- So that shake can rebuild when properties change
  _ <- addOracle $ \(StrPropQuery p tref) -> do
    let Target {..} = fromJust $ M.lookup tref targetsMap
    pure $ case p of
      Part -> targetPart
      XDC -> targetXDC
  _ <- addOracle $ \HDLQuery -> pure hdl
  _ <- addOracle $ \ToolChainQuery -> lookupToolChain ToolChainQuery

  let mods = nub $ map fst targets

  forM_ mods $ \modName ->
    rulesForModule modName

  forM_ targets $ \t ->
    rulesForTarget t
  where
    buildDir = "_build"
    clashDir = buildDir </> "clash"

    clash :: [String] -> Action ()
    clash args = liftIO $ defaultMain args'
      where
        args' = ["-outputdir", clashDir, "-isrc"] <> args

    depMkFile modName = clashDir </> modName <.> "mk"

    rulesForModule :: String -> Rules ()
    rulesForModule modName = do
      depMkFile modName %> \out -> do
        alwaysRerun
        withTempFile $ \temp -> do
          clash ["-M", "-dep-suffix", "", "-dep-makefile", temp, modName]
          copyFileChanged temp out

    rulesForTarget :: TargetRef -> Rules ()
    rulesForTarget tref@(modName, topEntity) = do
      manifestFile %> \_out -> do
        -- To correctly specify dependencies
        need [depMkFile modName]
        mkFile <- liftIO $ readFile $ depMkFile modName
        let deps = parseMakefile mkFile
        let hsSrcs = [src | (_, srcs) <- deps, src <- srcs, takeExtension src == ".hs"]
        need hsSrcs

        _hdl <- askOracle HDLQuery
        -- synthesize using clash
        clash $
          clashFlagOf hdl
            <> ["-fclash-compile-ultra"]
            <> ["-main-is", topEntity, modName]

      tclScript %> \out -> do
        need [manifestFile]

        -- Determine the HDL sources from clash-manifest.json
        _hdl <- askOracle HDLQuery
        Just Manifest {..} <- liftIO $ readManifest manifestFile
        hdlSrcs <-
          mapM makeAbsolute' $
            [ targetClashDir </> src
              | (src, _) <- fileNames,
                takeExtension src == "." <> extOf hdl
            ]

        part <- askOracle $ StrPropQuery Part tref
        xdcFile <- askOracle (StrPropQuery XDC tref) >>= makeAbsolute'

        let file =
              [__i|
          #{readCommandOf hdl} #{unwords hdlSrcs}
          read_xdc #{xdcFile}

          synth_design -top #{topComponent} -part #{part}
          write_checkpoint -force post_synth.dcp
          report_timing_summary -file post_synth_timing_summary.txt -rpx post_synth_timing_summary.rpx
          report_power -file post_synth_power.txt -rpx post_synth_power.rpx

          opt_design
          place_design
          phys_opt_design
          write_checkpoint -force post_place.dcp
          report_timing_summary \\
            -file post_place_timing_summary.txt \\
            -rpx post_place_timing_summary.rpx

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

      bitstreamFile %> \_out -> do
        need [tclScript]
        script <- makeAbsolute' tclScript
        cmd_ (Cwd vivadoDir) "vivado" "-mode" "batch" "-source" script

      libVmodel %> \_out -> do
        need [manifestFile]
        Just Manifest {..} <- liftIO $ readManifest manifestFile
        ToolChain {..} <- askOracle ToolChainQuery
        let top = T.unpack topComponent
        let makeFlags =
              unwords
                [ "CXX=" <> cxx,
                  "CC=" <> cc,
                  "OPT_FAST=\"-O3\"",
                  "OPT_GLOBAL=\"-march=native\""
                ]
        cmd_
          "verilator --cc --build --prefix Vmodel"
          -- use all threads for compilation
          "-j 0"
          -- generate FST waveform
          "--trace-fst --trace-structs"
          -- optimization flags
          -- see: https://verilator.org/guide/latest/simulating.html#benchmarking-optimization
          -- verilator codegen
          "-O3 --x-assign fast --x-initial fast --noassert"
          -- c++ compiler flags
          ["--MAKEFLAGS", makeFlags]
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

      libverilated %> \_out -> do
        need [libVmodel]
      where
        targetClashDir = clashDir </> modName <.> topEntity
        manifestFile = targetClashDir </> "clash-manifest.json"

        tclScript = buildDir </> modName <.> topEntity <.> "tcl"

        vivadoDir = buildDir </> "vivado" </> modName </> topEntity
        bitstreamFile = vivadoDir </> "out.bit"

        verilatorDir = buildDir </> "verilate" </> modName </> topEntity
        libverilated = verilatorDir </> "libverilated.a"
        libVmodel = verilatorDir </> "libVmodel.a"

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

rulesFromFile :: FilePath -> Rules ()
rulesFromFile file = do
  config <- liftIO $ do
    e <- decodeFileEither file
    case e of
      Left err -> fail $ "Failed to parse config file: " <> show err
      Right c -> pure c
  rulesFor $ compile config

shakeFromFile :: FilePath -> IO ()
shakeFromFile file = shakeArgs shakeOptions {shakeFiles = buildDir} $ do
  phony "clean" $ do
    putNormal $ "Cleaning files in " <> buildDir
    removeFilesAfter buildDir ["//*"]

  rulesFromFile file
  where
    buildDir = "_build"
