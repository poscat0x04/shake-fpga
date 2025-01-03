module Distribution.FPGA.Hooks where

import Clash.Data.UniqMap (empty)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce
import Data.Functor (($>))
import Data.Map.Strict qualified as M
import Development.Shake.FPGA.Internal
import Distribution.FPGA
import Distribution.Simple.SetupHooks
import GHC.IO (unsafePerformIO)

linkOpts :: LinkOpts
linkOpts = unsafePerformIO $ readBuildConfig "shake-fpga.yaml" >>= linkOptsOf
{-# NOINLINE linkOpts #-}

fpgaHooks :: SetupHooks
fpgaHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { preConfComponentHook = Just pccHook,
            postConfPackageHook = Just pcpHook
          }
    }

pccHook :: PreConfComponentHook
pccHook PreConfComponentInputs {..} = do
  let cn = componentName component
  pure $
    PreConfComponentOutputs $
      coerce (injectLinkOpts @Component) linkOpts cn (emptyComponentDiff cn)

pcpHook :: PostConfPackageHook
pcpHook i = liftIO buildAllLinkedTargets $> ()
