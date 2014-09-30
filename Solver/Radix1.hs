{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
--
-- Radix sort for a subclass of element types
--

module Solver.Radix1
  where

import Prelude                                                  as P
import Data.Bits
import Data.Array.Accelerate                                    as A


--
-- Radix sort ------------------------------------------------------------------
--
-- This is rather slow. Speeding up the reference implementation by using, say,
-- vector-algorithms, does not significantly change the runtime. Thus, we stick
-- to the simple list-based representation for the time being.
--

class Elt e => Radix e where
  passes :: e {- dummy -} -> Int
  radix  :: Exp Int -> Exp e -> Exp Int

#define signed(ty)                                                             \
instance Radix ty where ;                                                      \
  passes = finiteBitSize ;                                                     \
  radix  = radixOfSigned ;

#define unsigned(ty)                                                           \
instance Radix ty where ;                                                      \
  passes = finiteBitSize ;                                                     \
  radix  = radixOfUnsigned ;

signed(Int)
signed(Int8)
signed(Int16)
signed(Int32)
signed(Int64)
unsigned(Word)
unsigned(Word8)
unsigned(Word16)
unsigned(Word32)
unsigned(Word64)

radixOfSigned :: forall e. (Radix e, IsIntegral e) => Exp Int -> Exp e -> Exp Int
radixOfSigned i e = i ==* (passes' - 1) ? (radix' (e `xor` minBound), radix' e)
   where
     radix' x = A.fromIntegral $ (x `A.shiftR` i) .&. 1
     passes'  = constant (passes (undefined :: e))

radixOfUnsigned :: (Radix e, IsIntegral e) => Exp Int -> Exp e -> Exp Int
radixOfUnsigned i e = A.fromIntegral $ (e `A.shiftR` i) .&. 1


-- A simple (parallel) radix sort implementation [1].
--
-- [1] G. E. Blelloch. "Prefix sums and their applications." Technical Report
--     CMU-CS-90-190. Carnegie Mellon University. 1990.
--
radixsort :: Radix a => Acc (Vector a) -> Acc (Vector a)
radixsort = radixsortBy id

radixsortBy :: forall a r. (Elt a, Radix r) => (Exp a -> Exp r) -> Acc (Vector a) -> Acc (Vector a)
radixsortBy rdx arr = foldr1 (>->) (P.map radixPass [0..p-1]) arr
  where
    p = passes (undefined :: r)
    --
    deal f x      = let (a,b)   = unlift x in (f ==* 0) ? (a,b)
    radixPass k v = let k'      = unit (constant k)
                        flags   = A.map (radix (the k') . rdx) v
                        idown   = prescanl (+) 0 . A.map (xor 1)        $ flags
                        iup     = A.map (size v - 1 -) . prescanr (+) 0 $ flags
                        index   = A.zipWith deal flags (A.zip idown iup)
                    in
                    permute const v (\ix -> index1 (index!ix)) v

