{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module ChunkDB
  ( ChunkDB,
    empty,
    mkChunkDB,
    find,
    findOrErr,
    findRefs,
    findRefsOrErr,
    findAll,
    insert,
    insert',
    insertAll,
    insertAll',
    insertAllOrIgnore,
    union,
    registered,
    isRegistered,
    refbyTable, -- FIXME: This function should be re-examined. Some functions can probably be moved here!
  )
where

import Data.List (nub, (\\))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, typeOf, typeRep)
import Chunk (Chunk, HasChunkRefs (chunkRefs), chunkType, mkChunk, unChunk, IsChunk)
import UID (HasUID (..), UID)

type ReferredBy = [UID]

type ChunkByUID = M.Map UID (Chunk, ReferredBy)

type ChunksByTypeRep = M.Map TypeRep [Chunk]

newtype ChunkDB = ChunkDB (ChunkByUID, ChunksByTypeRep)

empty :: ChunkDB
empty = ChunkDB (M.empty, M.empty)

{-
TODO: Discuss important design decisions. These will become rather critical to Drasil projects that rely on ChunkDBs.

1. UID uniqueness and immutability after placement
2. `union`
3. No `ChunkDB`s inside of `ChunkDB`s!
4. Try to avoid manually written UID references.

TODO: Near-future:
1. Should all Chunks, on insertion, be checked to ensure that the knowledge they depend on already exists in the ChunkDB?

-}

find :: Typeable a => UID -> ChunkDB -> Maybe a
find u (ChunkDB (tc, _)) = do
  (c', _) <- M.lookup u tc
  unChunk c'

-- TODO: We should remove these "OrErr" functions because they print without context of where it was expected...
findOrErr :: Typeable a => UID -> ChunkDB -> a
findOrErr u = fromMaybe (error $ "Failed to find chunk " ++ show u) . find u

-- Is this TypeRep really needed? Well, for now, it's a hack to shorten our lists a bit and pre-cache our type lists by their typerep.
-- Justified,... but not optimal. It would be nice if we could have the chunks pre-unChunked or if we could avoid the TypeRep altogether!
-- On the bright side, we get order lists (by insertion order) for cheap!
findAll :: Typeable a => TypeRep -> ChunkDB -> [a]
findAll tr (ChunkDB (_, trm)) = maybe [] (mapMaybe unChunk) (M.lookup tr trm)

{-
consumeAllWithTyCon :: Typeable a => TyCon -> (forall b. Typeable (a b) => a b -> c) -> ChunkDB -> [c]
consumeAllWithTyCon tc f c@(ChunkDB (_, trm)) = r -- foldr (\a b -> b ++ mapMaybe (fmap f . unChunk) (findAll a c)) [] trKeys
  where
    trKeys = filter ((==) tc . typeRepTyCon) (M.keys trm)

    f' :: (Typeable a, Typeable b, Typeable (a b)) => Chunk -> Maybe (a b)
    f' = unChunk1

    r = foldr (\a b -> b ++ mapMaybe (fmap f . unChunk1) (findAll a c)) [] trKeys

---------
This function is seemingly impossible (or, at least, possible but very tricky!
I haven't figured out an elegant solution that doesn't involve _some_ sort of
enumeration).

So, here is a big question arising:

Do we want type parameters to be allowed for Chunks?
  - If we do, any sort of "bulk operation that works on the type constructor level,
    for any type parameters" is really difficult to perform _after_ a typecast because
    we need to be able to find a monomorphic type for the input (which is the hard part)
    as shown above.

  - If we don't, this should become much easier. However, we would lose out on the
    Haskell-level type errors.

    Is that a problem? In a sense, yes, for obvious reasons.
    However, in the greater scheme of things, if "Drasil in Drasil" is the goal, then
    "Drasil" as it stands is currently bootstrapped in Haskell, meaning the existing
    code likely won't remain forever. In which case, this might not be a "bad" thing
    at all because we'd likely have a different host language (e.g., Drasil..-lang?).

    In any case, if we did remove type parameters in chunks, then we would need to
    perform type checking at the level of Drasil (instead of leaning on the Haskell
    static type system, we'd be using the "Drasil-compiler runtime"). This sounds
    like it would work fine, but it might be a bit tedious.

-}

findRefs :: UID -> ChunkDB -> Maybe [UID]
findRefs u (ChunkDB (tc, _)) = do
  (_, refs) <- M.lookup u tc
  Just refs

findRefsOrErr :: UID -> ChunkDB -> [UID]
findRefsOrErr u = fromMaybe (error $ "Failed to find references for unknown chunk " ++ show u) . find u

insert' :: IsChunk a => a -> ChunkDB -> ChunkDB
insert' = flip insert

insert :: IsChunk a => ChunkDB -> a -> ChunkDB
insert (ChunkDB (cu, ctr)) c
  -- TODO: Enable this once I've removed th e type parameters for QDefinition!
  --  | not (null (splitTyConApp (typeOf c) ^. _2)) = error "Chunks are not allowed to have type parameters."
  | typeOf c == typeRep (Proxy @ChunkDB) =
    error "Insertion of ChunkDBs in ChunkDBs is disallowed; please perform unions with them instead."
  | M.member (uid c) cu =
    error $ "Attempting to register a chunk with an already registered UID; `" ++ show (uid c) ++ "`"
  | otherwise = ChunkDB (finalCu, ctr')
  where
    c' :: Chunk
    c' = mkChunk c

    cu' :: ChunkByUID
    cu' = M.insert (uid c) (c', []) cu -- insert our chunk, it is not currently referred by anything.
    insertRefExpectingExistence :: UID -> ChunkByUID -> ChunkByUID
    insertRefExpectingExistence u cbu =
      if isJust prev
        then cbu'
        else error $ "Referred knowledge is missing for `" ++ show (uid c) ++ "`; needs `" ++ show u ++ "`"
      where
        (prev, cbu') = M.insertLookupWithKey (\_ _ (rcc, rcref) -> (rcc, u : rcref)) u (undefined, []) cbu

    finalCu :: ChunkByUID
    finalCu = foldr insertRefExpectingExistence cu' $ nub (chunkRefs c) \\ [uid c]

    ctr' :: ChunksByTypeRep
    ctr' = M.alter (Just . maybe [c'] (++ [c'])) (typeOf c) ctr

insertAll :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAll cdb l = foldr (flip insert) cdb (reverse l) -- note: the "reverse" is here to make insertions slightly more readable -- I don't want to use foldl (it seems many have complaints about it)

insertAll' :: IsChunk a => [a] -> ChunkDB -> ChunkDB
insertAll' = flip insertAll

insertAllOrIgnore :: IsChunk a => ChunkDB -> [a] -> ChunkDB
insertAllOrIgnore cdb = foldr (\next old -> if isRegistered (uid next) cdb then old else insert old next) cdb

union :: ChunkDB -> ChunkDB -> ChunkDB
union (ChunkDB (lum, ltrm)) (ChunkDB (rum, rtrm)) = ChunkDB (um, trm)
  where
    um :: ChunkByUID
    um = M.unionWith (\(conflict, _) _ -> error $ "Unioned ChunkDBs contains at least one UID collision; `" ++ show (uid conflict) ++ "`!") lum rum

    -- NOTE: This is an important design decision, we are joining with preference for assuming that the LEFT comes first (in other words, is the "earlier" discovered information)
    trm :: ChunksByTypeRep
    trm = M.unionWith (++) ltrm rtrm

mkChunkDB :: [Chunk] -> ChunkDB
mkChunkDB cs = ChunkDB (cbu, csbtr)
  where
    cbu :: ChunkByUID -- TODO: build a proper reference list, post-facto
    cbu =
      M.fromListWithKey
        ( \k (r1, _) (r2, _) ->
            error $
              "At least 2 chunks provided contain the same UID, `"
                ++ show k
                ++ "`, with types: "
                ++ show (chunkType r1)
                ++ " and "
                ++ show (chunkType r2)
        )
        $ map (\c -> (uid c, (c, []))) cs

    trs :: [TypeRep]
    trs = nub $ map chunkType cs

    trcs :: [(TypeRep, [Chunk])]
    trcs = map (\tr -> (tr, filter ((==) tr . chunkType) cs)) trs

    csbtr :: ChunksByTypeRep
    csbtr = M.fromList trcs

registered :: ChunkDB -> [UID]
registered (ChunkDB (x, _)) = M.keys x

isRegistered :: UID -> ChunkDB -> Bool
isRegistered u (ChunkDB (x, _)) = M.member u x

{- FIXME: TO BE REWRITTEN -}
refbyTable :: ChunkDB -> M.Map UID [UID]
refbyTable (ChunkDB (x, _)) = M.map snd x