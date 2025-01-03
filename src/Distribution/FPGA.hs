{-# LANGUAGE OverloadedStrings #-}

module Distribution.FPGA
  ( injectFPGAHooks,
    simpleUserHooksWithFPGA,
    defaultMainWithFPGA,
    LinkOpt (..),
    LinkOpts,
    linkOptsOf,
    injectLinkOpt,
    injectLinkOpts,
  )
where

import Control.Monad (forM, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Development.Shake (shake, want)
import Development.Shake.FPGA.DirStructure
  ( BuildOutputLayout (..),
    DirStructure (..),
    buildOutputsOf,
    dirsOf,
  )
import Development.Shake.FPGA.Internal
  ( BuildConfig (..),
    Target (..),
    buildAllLinkedTargets,
    compile,
    readBuildConfig,
    rulesFor,
  )
import Development.Shake.FPGA.Utils
  ( Components (..),
    HasComponentName (..),
    shakeOpts,
  )
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.Compat.Lens ((%~), (&))
import Distribution.ModuleName (ModuleName)
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo (localPkgDescr), lookupComponent, showComponentName)
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.Lens
  ( HasBuildInfo (..),
    PackageDescription,
    allLibraries,
    benchmarks,
    executables,
    foreignLibs,
    testSuites,
  )
import Distribution.Utils.Path
  ( FileOrDir (..),
    Include,
    Pkg,
    Source,
    SymbolicPath,
    unsafeMakeSymbolicPath,
  )
import GHC.Generics (Generic)

type Linkage = (ComponentName, Target)

-- | Target is
data LinkOpt = LinkOpt
  { includePath :: SymbolicPath Pkg (Dir Include),
    objects :: [FilePath],
    libraries :: [String],
    libSearchPaths :: [FilePath],
    hsSourcePath :: SymbolicPath Pkg (Dir Source),
    hsModules :: [ModuleName]
  }
  deriving (Show, Eq, Generic)

type LinkOpts = Map ComponentName LinkOpt

linkOptsOf :: BuildConfig -> IO LinkOpts
linkOptsOf BuildConfig {..} = do
  alists <-
    sequenceA
      [ fmap (cn,) io
        | t@Target {..} <- targets,
          let io = linkOptOf t,
          cn <- unComponents targetLinkComponents
      ]
  pure $ M.fromList alists

linkOptOf :: Target -> IO LinkOpt
linkOptOf Target {..} = do
  let DirStructure {..} = dirsOf (targetModule, targetTopEntity)
      BuildOutputLayout {..} = buildOutputsOf (targetModule, targetTopEntity)
  fullVmodelCOAbs <- makeAbsolute fullVmodelCO
  pure
    LinkOpt
      { includePath = unsafeMakeSymbolicPath verilatorDir,
        libraries = ["stdc++", "atomic", "z"],
        libSearchPaths = [],
        hsSourcePath = unsafeMakeSymbolicPath hsDir,
        hsModules = ["Verilated"],
        objects = [fullVmodelCOAbs]
      }

injectLinkOptsToPD :: LinkOpts -> PackageDescription -> PackageDescription
injectLinkOptsToPD opts pd' =
  pd'
    & executables . traverse %~ injectLinkOpts' opts
    & allLibraries %~ injectLinkOpts' opts
    & foreignLibs . traverse %~ injectLinkOpts' opts
    & testSuites . traverse %~ injectLinkOpts' opts
    & benchmarks . traverse %~ injectLinkOpts' opts
  where
    injectLinkOpts' opts a = injectLinkOpts opts (componentName a) a

injectLinkOpts :: (HasBuildInfo a) => LinkOpts -> ComponentName -> a -> a
injectLinkOpts opts cn a =
  case M.lookup cn opts of
    Just opt -> injectLinkOpt opt a
    Nothing -> a

injectLinkOpt :: (HasBuildInfo a) => LinkOpt -> a -> a
injectLinkOpt LinkOpt {..} a =
  a
    & buildInfo . options %~ fmap (objects <>)
    & buildInfo . includeDirs %~ (includePath :)
    & buildInfo . extraLibs %~ (libraries <>)
    & buildInfo . hsSourceDirs %~ (hsSourcePath :)
    & buildInfo . otherModules %~ (hsModules <>)
    & buildInfo . options %~ fmap (libFlags libraries <>)
  where
    libFlags :: (Functor f) => f String -> f String
    libFlags = fmap ("-l" <>)

buildThenInject :: PackageDescription -> IO PackageDescription
buildThenInject pd = do
  bc <- buildAllLinkedTargets
  opts <- linkOptsOf bc
  pure $ injectLinkOptsToPD opts pd

injectFPGAHooks :: UserHooks -> UserHooks
injectFPGAHooks UserHooks {..} = uh'
  where
    uh' =
      UserHooks
        { buildHook = newBuildHook,
          replHook = newReplHook,
          ..
        }
    newBuildHook pd lbi uh bf = do
      pd' <- buildThenInject pd
      let lbi' = lbi {localPkgDescr = pd'}
      buildHook pd' lbi' uh bf
    newReplHook pd lbi uh rf args = do
      pd' <- buildThenInject pd
      let lbi' = lbi {localPkgDescr = pd'}
      replHook pd' lbi' uh rf args

simpleUserHooksWithFPGA :: UserHooks
simpleUserHooksWithFPGA = injectFPGAHooks simpleUserHooks

defaultMainWithFPGA :: IO ()
defaultMainWithFPGA = defaultMainWithHooks simpleUserHooksWithFPGA
