{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

module Chunk
  ( Chunk,
    IsChunk,
    HasChunkRefs (..),
    mkChunk,
    unChunk,
    chunkType,
  )
where

import Data.Typeable (Proxy (Proxy), TypeRep, Typeable, cast, typeOf, typeRep)
import UID (HasUID (..), UID)

class HasChunkRefs a where
  chunkRefs :: a -> [UID]

type IsChunk a = (HasUID a, HasChunkRefs a, Typeable a)

data Chunk = forall a. IsChunk a => Chunk a

instance Eq Chunk where
  l == r = uid l == uid r

instance HasUID Chunk where
  uid (Chunk t) = uid t

mkChunk :: (HasUID a, HasChunkRefs a, Typeable a) => a -> Chunk
mkChunk a
  | typeOf a == typeRep (Proxy @Chunk) = error "Cannot place a Chunk inside of a Chunk"
  | otherwise = Chunk a

unChunk :: Typeable a => Chunk -> Maybe a
unChunk (Chunk c) = cast c

chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c
