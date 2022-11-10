-- | Unique Identifier used across Drasil.
module UID (
    UID
  , HasUID(uid)
  , mkUid
) where

-- | The most basic item: having a unique identifier key, here a UID.
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use.
  uid :: c -> UID

-- | A 'UID' is a "unique identifier" for things that we will put into our
-- database of information. We use a newtype wrapper to make sure we are only
-- using 'UID's where desired.
newtype UID = UID String
  deriving (Eq, Ord)

instance Show UID where
  show (UID u) = u

-- | Smart constructor for making a 'UID' from a 'String'.
mkUid :: String -> UID
mkUid = UID
