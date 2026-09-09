module Distribution.FPGA.Hooks where

import Data.Coerce
import Data.Functor (($>))
import Development.Shake.FPGA.Internal
import Distribution.FPGA
import Distribution.Simple.SetupHooks

fpgaHooks :: SetupHooks
fpgaHooks =
  noSetupHooks
    { configureHooks =
        noConfigureHooks
          { postConfPackageHook = Just pcpHook,
            preConfComponentHook = Just pccHook
          }
    }

pccHook :: PreConfComponentHook
pccHook PreConfComponentInputs {..} = do
  linkOpts <- readBuildConfig "shake-fpga.yaml" >>= linkOptsOf
  let cn = componentName component
  pure $
    PreConfComponentOutputs $
      coerce (injectLinkOpts @Component) linkOpts cn (emptyComponentDiff cn)

pcpHook :: PostConfPackageHook
pcpHook _i = buildAllLinkedTargets $> ()
