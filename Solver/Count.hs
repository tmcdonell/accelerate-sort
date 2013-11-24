{-# LANGUAGE ScopedTypeVariables #-}

module Solver.Count
  where

import Prelude                                  as P
import Data.Array.Accelerate                    as A
--import Data.Array.Accelerate.Interpreter
--import Data.Array.Accelerate.CUDA


class (Elt e, IsIntegral e) => Count e where
  countBins   :: e {- dummy -} -> Int

instance Count Word8 where
  countBins _ = 256

instance Count Word16 where
  countBins _ = 65536


histogram :: forall e. Count e => Acc (Vector e) -> Acc (Vector Int32)
histogram arr =
  let bins      = constant $ countBins (undefined :: e)
      zeros     = fill (index1 bins) (constant 0)
      ones      = fill (shape arr)   (constant 1)
  in
  permute (+) zeros (\ix -> index1 . A.fromIntegral $ arr!ix) ones


sort :: Count e => Acc (Vector e) -> Acc (Vector e)
sort arr =
  let
      -- CUDA only supports atomic operations for 32-bit and 64-bit types, which
      -- are required in the implementation of 'permute'.
      --
      zeros     = fill (shape arr)   (0 :: Exp Int32)
      ones      = fill (shape count) (1 :: Exp Int32)

      count     = A.scanl1 (+) (histogram arr)
      flags     = permute (+) zeros (\ix -> let i = A.fromIntegral (count ! ix)
                                            in  i >=* size arr ? ( ignore, index1 i )) ones
  in
  A.scanl1 (+) (A.map A.fromIntegral flags)

