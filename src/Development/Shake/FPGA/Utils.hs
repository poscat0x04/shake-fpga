module Development.Shake.FPGA.Utils (DBool (..)) where

import Data.Aeson (FromJSON (..))
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import GHC.Generics (Generic)

class KnownBool (k :: Bool) where
  boolVal :: Proxy k -> Bool

instance KnownBool 'True where
  boolVal _ = True

instance KnownBool 'False where
  boolVal _ = False

newtype DBool (k :: Bool)
  = DBool Bool
  deriving (Show, Eq, Generic)

instance (KnownBool k) => FromJSON (DBool k) where
  omittedField = Just $ coerce $ boolVal (Proxy @k)
  parseJSON v = do
    (b :: Bool) <- parseJSON v
    pure $ coerce b
