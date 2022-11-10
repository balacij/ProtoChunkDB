module Main where

import qualified Control.Exception as E

import Chunk (HasChunkRefs (..))
import ChunkDB (mkChunkDB, findOrErr, ChunkDB, registered, union, typesRegistered, numRegistered)
import UID (HasUID (..), UID, mkUid)
import TypedUIDRef (mkRef, typedFindOrErr)

data Something = Something
  { _t1Uid :: UID,
    _t1Refs :: [UID]
  }
instance HasUID Something where uid = _t1Uid
instance HasChunkRefs Something where chunkRefs = _t1Refs

data SomethingElse = SomethingElse
  { _t2Uid :: UID,
    _t2Refs :: [UID]
  }
instance HasUID SomethingElse where uid = _t2Uid
instance HasChunkRefs SomethingElse where chunkRefs = _t2Refs

main :: IO ()
main = do
  let
    t11 = Something (mkUid "t11") []
    t12 = Something (mkUid "t12") [uid t11]
    t13 = Something (mkUid "t13") [uid t12]
    t14 = Something (mkUid "t14") []
    t21 = SomethingElse (mkUid "t21") []
    t22 = SomethingElse (mkUid "t22") [uid t21]
    t23 = SomethingElse (mkUid "t23") [uid t22]
    t24 = SomethingElse (mkUid "t24") []

    tUid = mkRef t12
    
    cdb1 :: ChunkDB
    cdb1 = mkChunkDB [t11, t12, t13]

    cdb2 :: ChunkDB
    cdb2 = mkChunkDB [t21, t22, t23, t24]

    hdlr :: E.ErrorCall -> IO ()
    hdlr = print

    run f = E.catch f hdlr


  putStrLn "[ Grab a chunk from the ChunkDB, with explicit information ]"
  run $ print $ uid (findOrErr (mkUid "t11") cdb1 :: Something)

  putStrLn "\n[ Grab a chunk from the ChunkDB with type information from a typed reference (intended to be used!) ]"
  run $ print $ uid $ typedFindOrErr tUid cdb1

  putStrLn "\n[ ERROR EXPECTED: Look for a non-existant chunk ]"
  run $ print $ uid $ typedFindOrErr tUid cdb2

  putStrLn "\n[ ERROR EXPECTED: Place a chunk in a ChunkDB that doesn't contain the referenced chunks ]"
  run $ print $ uid $ typedFindOrErr tUid (mkChunkDB [t12]) -- since we didn't place t11 in the DB, it errors! "t12" is denied entry.

  putStrLn "\n[ Same run as ^, but with the required referenced chunks ]"
  run $ print $ uid $ typedFindOrErr tUid (mkChunkDB [t11, t12])

  putStrLn "\n[ Count of Chunks in CDB1 ]"
  run $ print $ numRegistered cdb1

  putStrLn "\n[ Show the list of registered chunks by UID ]"
  run $ print $ registered cdb1

  putStrLn "\n[ Show the list of typed registered ]"
  run $ print $ typesRegistered cdb1

  putStrLn "\n[ Union of two ChunkDBs ]"
  run $ print $ registered (cdb1 `union` mkChunkDB [t14])

  putStrLn "\n[ Uids of the union of the two ChunkDBs ]"
  run $ print $ registered (cdb1 `union` cdb2)

  putStrLn "\n[ Types of the union of the two ChunkDBs ]"
  run $ print $ typesRegistered (cdb1 `union` cdb2)

  putStrLn "\n[ Count of the union of the two ChunkDBs ]"
  run $ print $ numRegistered (cdb1 `union` cdb2)

  putStrLn "\n[ ERROR EXPECTED: Union of the same ChunkDB (UID Conflicts!) ]"
  run $ print $ registered (cdb1 `union` cdb1)
