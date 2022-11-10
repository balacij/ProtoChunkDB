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

-- | All chunks should expose what chunks they reference/rely on, so that we can
-- test 'ChunkDB's to make sure they carry enough information to
-- 'satisfy'/'create' a chunk.
class HasChunkRefs a where
  chunkRefs :: a -> [UID]

-- | Constraint for anything that may be considered a valid chunk type.
type IsChunk a = (HasUID a, HasChunkRefs a, Typeable a)

-- | Any _thing_ that satisfies the IsChunk constraint list.
data Chunk = forall a. IsChunk a => Chunk a

instance Eq Chunk where
  l == r = uid l == uid r

instance HasUID Chunk where
  uid (Chunk t) = uid t

-- | Smart constructor for 'Chunk', ensuring that 'Chunk's are never placed
-- within 'Chunk's.
mkChunk :: IsChunk a => a -> Chunk
mkChunk a
  | typeOf a == typeRep (Proxy @Chunk) = error "Cannot place a Chunk inside of a Chunk"
  | otherwise = Chunk a

-- | "Open" chunks, casting them to a specific type.
unChunk :: Typeable a => Chunk -> Maybe a
unChunk (Chunk c) = cast c

-- | Ask a 'Chunk' for it's type representation.
chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c
