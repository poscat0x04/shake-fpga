module Development.Shake.FPGA.Utils
  ( DBool (..),
    Components (..),
    HasComponentName (..),
    buildDir,
    shakeOpts,
  )
where

import Control.Monad (forM)
import Data.Aeson (FromJSON (..))
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.String.Interpolate (__i)
import Development.Shake
import Distribution.Compat.Lens (view)
import Distribution.PackageDescription (ComponentName (..))
import Distribution.Parsec (eitherParsec)
import Distribution.Types.Lens
import GHC.Generics (Generic)

class KnownBool (k :: Bool) where
  boolVal :: Proxy k -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False

-- | Boolean with default value
newtype DBool (k :: Bool)
  = DBool Bool
  deriving (Show, Eq, Generic)

instance (KnownBool k) => FromJSON (DBool k) where
  omittedField = Just $ coerce $ boolVal (Proxy @k)
  parseJSON v = do
    (b :: Bool) <- parseJSON v
    pure $ coerce b

-- | List of cabal components
newtype Components = Components {unComponents :: [ComponentName]}
  deriving (Show, Eq, Generic)

instance FromJSON Components where
  omittedField = Just $ Components []
  parseJSON v = do
    sl <- parseJSON v
    cs <- forM sl $ \s ->
      either
        (\e -> fail [__i|Invalid component name #{e}|])
        pure
        $ eitherParsec s
    pure $ Components cs

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

buildDir :: String
buildDir = "_build"

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions {shakeFiles = buildDir, shakeThreads = 0}
