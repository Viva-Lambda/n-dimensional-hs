-- matrix
module Matrix where

import Common
import Vector
import Helper
import BaseEnum

import Data.Word
import Data.List
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


mgetColumn :: Matrix -> Word -> Vector
mgetColumn mat cindex =
    if cindex >= (mstride mat)
    then let msg1 = "given column index is larger than stride " ++ show (mstride mat)
             msg2 = msg1 ++ " index " ++ show cindex 
         in traceStack msg2 zeroV 3
    else let md = mdata mat
             col_nb = word2Int $ mColNb mat
             row_nb = word2Int $ mRowNb mat
             cinx = word2Int cindex
             cindices = [r * col_nb + cinx | r <- [0..row_nb]]
             mapf index = getNL md index
             (c:cs) = map mapf cindices
         in fromList2Vec c cs

mgetRow :: Matrix -> Word -> Vector
mgetRow mat rindex =
    let rnb = mRowNb mat
    in if rindex >= rnb
       then let msg1 = "given row index is larger than row number " ++ show rnb
                msg2 = msg1 ++ " index " ++ show rindex 
            in traceStack msg2 zeroV 3
       else let md = mdata mat
                col_nb = word2Int $ mColNb mat
                rinx = word2Int rindex
                rindices = [rinx * col_nb + c | c <- [0..col_nb]]
                mapf index = getNL md index
                (c:cs) = map mapf rindices
            in fromList2Vec c cs

mSetRow :: Matrix -> Word -> Vector -> Matrix
mSetRow mat rowIndex newRow =
    if rowIndex >= (mRowNb mat)
    then traceStack "given row index is larger than number of rows of matrix" (mzero 1 1)
    else let col_nb = word2Int $ mColNb mat
             rinx = word2Int rowIndex
             rindices = [(rinx * col_nb + c, c) | c <- [0..col_nb]]
             md = mdata mat
             getIf index = let (is_in, indx) = let (reals, cs) = unzip rindices
                                                   val = elemIndex index reals
                                               in case val of
                                                    Just ind -> (True,
                                                                 cs !! ind)
                                                    Nothing -> (False, index) 
                           in if is_in
                              then vget newRow indx
                              else getNL md indx
             (c:cs) = map getIf [0..((lengthNL md) - 1)]
         in MList {mdata = fromList2NL c cs, mstride = mstride mat}


                               


