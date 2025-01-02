module Distribution.FPGA () where

import Control.Monad (foldM, forM, when)
import Data.Coerce (coerce)
import Data.Functor.Identity
import Data.Hashable (Hashable)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isNothing)
import Development.Shake.FPGA.Internal
import Development.Shake.FPGA.Utils
import Distribution.Compat.Lens
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (lookupComponent, showComponentName)
import Distribution.Types.Component (Component (..))
import Distribution.Types.ComponentName (ComponentName (..))
import Distribution.Types.Lens

type Linkage = (ComponentName, Target)

-- | Target is
type LinkOpt = String

type LinkOpts = Map ComponentName LinkOpt

linkagesOf :: CompiledBuildConfig -> [Linkage]
linkagesOf = _

injectLinkages :: PackageDescription -> [Linkage] -> IO PackageDescription
injectLinkages pd linkages = do
  alist <- forM linkages $ \(cn, t) -> do
    ensureComponent cn
    opt <- resolveLinkOpt t
    pure (cn, opt)
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
            Just opt ->
              a
                & buildInfo
                  %~ ( \bi ->
                         bi
                           & options %~ _
                           & ldOptions %~ _
                           & includeDirs %~ _
                           & extraLibDirs %~ _
                           & extraLibs %~ (\l -> l <> ["stdc++", "atomic", "z"])
                     )
            Nothing -> a

    ensureComponent :: ComponentName -> IO ()
    ensureComponent cn =
      when (isNothing (lookupComponent pd cn)) $
        fail $
          "Unknown component: " <> showComponentName cn

    resolveLinkOpt :: Target -> IO LinkOpt
    resolveLinkOpt Target {..} = _

class HasComponentName a where
  componentName :: a -> ComponentName

instance HasComponentName Executable where
  componentName = CExeName . view exeName

instance HasComponentName Library where
  componentName = CLibName . view libName

instance HasComponentName ForeignLib where
  componentName = CFLibName . view foreignLibName

instance HasComponentName TestSuite where
  componentName = CTestName . view testName

instance HasComponentName Benchmark where
  componentName = CBenchName . view benchmarkName
