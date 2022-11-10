{-# LANGUAGE ExistentialQuantification #-}
module Main where

import Prelude hiding (lookup)
import Data.Map.Strict (Map, lookup)

type UID = String


type ChunkDB = Map UID Chunk

data Chunk = forall a. Chunk a

data QuantityDict = QD {
  _uid :: UID,
  a :: String
}

retrieveQD :: UID -> ChunkDB -> Maybe QuantityDict
retrieveQD u cdb = do
  (Chunk expectedQd) <- lookup u cdb
  pure expectedQd

main :: IO ()
main = do
  putStrLn "Hello world!"
