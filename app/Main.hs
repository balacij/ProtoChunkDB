{-# LANGUAGE ExistentialQuantification #-}
module Main where

import qualified Data.Map.Strict as M

type UID = String


type ChunkDB = M.Map UID Chunk

data Chunk = forall a. Chunk a

data QuantityDict = QD {
  _uid :: UID,
  a :: String
}

retrieveQD :: UID -> ChunkDB -> Maybe QuantityDict
retrieveQD u cdb = do
  (Chunk expectedQd) <- M.lookup u cdb
  pure expectedQd

main :: IO ()
main = do
  putStrLn "Hello world!"
