-- matrix
module Matrix where

import Common
import Vector
import Helper
import BaseEnum

import Data.Word
import Debug.Trace

data Matrix = MList {mdata :: NonEmptyList Scalar,
                     mstride :: Word}

instance Eq Matrix where
    a == b = (mdata a) == (mdata b)

mfill :: Scalar -> Word -> Word -> Matrix
mfill s rowNb colNb = let (rnb, cnb) = (word2Int rowNb, word2Int colNb)
                          (m:ms) = replicate (rnb * cnb) s
                      in MList {mdata = fromList2NL m ms,
                                mstride = colNb}

mzero :: Word -> Word -> Matrix
mzero = mfill 0.0

fromVectors :: NonEmptyList Vector -> Matrix
fromVectors vvs =
    let (v:vs) = nl2List vvs
        sizes = [(vsize v_) == (vsize v) | v_ <- v:vs]
        allSameLength = foldl1 (==) sizes
    in if not allSameLength
       then traceStack "All vectors must have same length" (mzero 1 1)
       else -- foldfn :: (a -> b -> a)
           let foldfn ac p = let a = vec2List p in ac ++ a
               (m:ms) = foldl foldfn [] (v:vs)
           in MList {mdata = fromList2NL m ms, 
                     mstride = (int2Word . vsize) v}

msize :: Matrix -> Word
msize = (int2Word . (lengthNL . mdata))

mget :: Matrix -> Word -> Word -> Scalar
mget mat rowIndex colIndex =
    let i = word2Int rowIndex
        j = word2Int colIndex
    in getNL (mdata mat) (i * j + j) 

mColNb :: Matrix -> Word
mColNb = mstride

mRowNb :: Matrix -> Word
mRowNb a = 
    let s = msize a
        scalarCol = word2Scalar (mColNb a)
        scalarRow = (word2Scalar s) / scalarCol
    in scalar2Word scalarRow
