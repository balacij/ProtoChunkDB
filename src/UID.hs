-- | Unique Identifier used across Drasil.
module UID (
    UID
  , HasUID(uid)
  , mkUid, (+++), (+++.), (+++!)
  , showUID
  , sortByUID
) where

import Data.List (sortBy)

-- | The most basic item: having a unique identifier key, here a UID.
class HasUID c where
  -- | Provides a /unique/ id for internal Drasil use.
  uid :: c -> UID

-- | A @UID@ is a 'unique identifier' for things that we will put into our database
-- of information. We use a newtype wrapper to make sure we are only using
-- 'UID's where desired.
newtype UID = UID String
  deriving (Eq, Ord)

instance Show UID where
  show (UID u) = u

-- | Smart constructor for making a 'UID' from a 'String'.
mkUid :: String -> UID
mkUid = UID
  -- '►' `elem` s = error $ "► not allowed in UID " ++ show s -- FIXME: Need to implement other constructors before we can use this.
  -- null s       = error "UID must be non-zero length" -- FIXME: See Drasil.DocumentLanguage.TraceabilityGraph (uses an empty UID)
  -- otherwise    = UID s

-- | For when we need to modify a UID. We first take the base chunk's UID and then append a suffix to it.
(+++) :: HasUID a => a -> String -> UID
a +++ suff
  | null suff       = error "Suffix must be non-zero length"
  | otherwise       = UID $ s ++ suff
  -- otherwise       = UID $ s ++ '►':suff --FIXME: Implement this properly.
    where
        UID s = uid a

-- | For when we need to append something to a UID.
(+++.) :: UID -> String -> UID
a +++. suff
  | null suff       = error $ "Suffix must be non-zero length for UID " ++ show a
  | otherwise       = UID $ s ++ suff
    where UID s = a

(+++!) :: (HasUID a, HasUID b) => a -> b -> UID
a +++! b 
  | null (showUID a) || null (showUID b) = error $ showUID a ++ " and " ++ showUID b ++ " UIDs must be non-zero length"
  | otherwise = UID (showUID a ++ showUID b)

-- TODO: This looks like it shouldn't exist.
-- | Grabs the UID from something that has a UID and displays it as a String.
showUID :: HasUID a => a -> String
showUID = show . uid

sortByUID :: HasUID a => [a] -> [a]
sortByUID = sortBy (\l r -> compare (uid l) (uid r))
