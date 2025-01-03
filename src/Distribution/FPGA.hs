{-# LANGUAGE OverloadedStrings #-}

module Distribution.FPGA
  ( injectFPGAHooks,
    simpleUserHooksWithFPGA,
    defaultMainWithFPGA,
  )
where

import Control.Monad (forM, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Development.Shake
import Development.Shake.FPGA.DirStructure
  ( BuildOutputLayout (..),
    DirStructure (..),
    buildOutputsOf,
    dirsOf,
  )
import Development.Shake.FPGA.Internal
  ( BuildConfig (..),
    Target (..),
    compile,
    readBuildConfig,
    rulesFor,
  )
import Development.Shake.FPGA.Utils
  ( Components (..),
    HasComponentName (..),
    shakeOpts,
  )
import Distribution.Compat.Lens
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

linkagesOf :: BuildConfig -> [Linkage]
linkagesOf BuildConfig {..} =
  [ (cn, t)
    | t@Target {..} <- targets,
      cn <- unComponents targetLinkComponents
  ]

injectLinkages :: PackageDescription -> [Linkage] -> IO PackageDescription
injectLinkages pd linkages = do
  alist <- forM linkages $ \(cn, t) -> do
    ensureComponent cn
    pure (cn, resolveLinkOpt t)
  let linkOpts = M.fromList alist
  pure $ injectLinkOpts linkOpts pd
  where
    injectLinkOpts :: LinkOpts -> PackageDescription -> PackageDescription
    injectLinkOpts opts pd' =
      pd'
        & executables . traverse %~ injectLinkOptTo
        & allLibraries %~ injectLinkOptTo
        & foreignLibs . traverse %~ injectLinkOptTo
        & testSuites . traverse %~ injectLinkOptTo
        & benchmarks . traverse %~ injectLinkOptTo
      where
        injectLinkOptTo :: (HasComponentName a, HasBuildInfo a) => a -> a
        injectLinkOptTo a =
          case M.lookup (componentName a) opts of
            Just LinkOpt {..} ->
              a
                & buildInfo
                  %~ ( \bi ->
                         bi
                           & options %~ fmap (\flag -> objects <> libFlags libraries <> flag)
                           & includeDirs %~ (includePath :)
                           & hsSourceDirs %~ (hsSourcePath :)
                           & otherModules %~ (hsModules <>)
                     )
            Nothing -> a

        libFlags = fmap ("-l" <>)

    ensureComponent :: ComponentName -> IO ()
    ensureComponent cn =
      when (isNothing (lookupComponent pd cn)) $
        fail $
          "Unknown component: " <> showComponentName cn

    resolveLinkOpt :: Target -> LinkOpt
    resolveLinkOpt Target {..} =
      let DirStructure {..} = dirsOf (targetModule, targetTopEntity)
          BuildOutputLayout {..} = buildOutputsOf (targetModule, targetTopEntity)
       in LinkOpt
            { includePath = unsafeMakeSymbolicPath verilatorDir,
              libraries = ["stdc++", "atomic", "z"],
              libSearchPaths = [],
              hsSourcePath = unsafeMakeSymbolicPath hsDir,
              hsModules = ["Verilated"],
              objects = [fullVmodelCO]
            }

buildThenInject :: PackageDescription -> IO PackageDescription
buildThenInject pd = do
  bc <- readBuildConfig "shake-fpga.yaml"
  let shakeTargets =
        [ target
          | Target {..} <- targets bc,
            not $ null $ unComponents targetLinkComponents,
            let BuildOutputLayout {..} = buildOutputsOf (targetModule, targetTopEntity),
            target <- [fullVmodelCO, verilatedHSC]
        ]
  shake shakeOpts $
    rulesFor (compile bc) >> want shakeTargets
  injectLinkages pd $ linkagesOf bc

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
