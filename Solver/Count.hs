{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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


-- There is a problem in the above in that we lose the relation to the original
-- element, thus we construct an entirely new array, not by determining how to
-- permute the old elements into a sorted position.
--

{--
 -- http://opendatastructures.org/ods-java/11_2_Counting_Sort_Radix_So.html

   int[] countingSort(int[] a, int k) {
        int c[] = new int[k];
        for (int i = 0; i < a.length; i++)
            c[a[i]]++;
        for (int i = 1; i < k; i++)
            c[i] += c[i-1];
        int b[] = new int[a.length];
        for (int i = a.length-1; i >= 0; i--)
            b[--c[a[i]]] = a[i];                        -- (1)
        return b;
    }
--}


as :: Acc (Vector Word8)
as = use $ fromList (Z:.20) [7,2,9,0,1,2,0,9,7,4,4,6,9,1,0,9,3,2,5,9]

c  = histogram as               -- [3,2,3,1,2,1,1,2,0,5]
c' = A.scanl1 (+) c             -- [3,5,8,9,11,12,13,15,15,20]

--flags :: Acc (Vector Word8)
flags = permute (+) zeros (\ix -> let i = A.fromIntegral (c' ! ix)
                                  in  i >=* size as ? (ignore, index1 i)) ones -- c'
  where
    zeros       = A.fill (shape as) 0
    ones        = A.fill (shape c') 1   :: Acc (Vector Word8)

-- if scanning the input array ('as') right-to-left, 'bucket' represents the
-- index that each element will be put in the result array. However! Once we
-- use an index, it needs to be decremented somehow so that the next element of
-- the same value goes into the bucket before it (comment 1)!
--
-- [14,7,19,2,4,7,2,19,14,10,10,12,19,4,2,19,8,7,11,19]
--
-- whereas we need this to be:
--
-- [ ... mess ... ]
--
-- bucket = A.generate (shape as)
--                     (\ix -> (c' ! index1 (A.fromIntegral $ as ! ix)) - 1)
--
-- bucket' = backpermute (shape as)
--                       (\ix -> index1 (A.fromIntegral $ as ! ix))
--                       (A.map (subtract 1) c')

