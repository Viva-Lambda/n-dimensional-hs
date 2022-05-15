-- helper types
module Helper where

import Debug.Trace
import Data.List
import GHC.Float hiding (clamp)

import BaseEnum

data NonEmptyList a = NList a [a]

headNL :: NonEmptyList a -> a
headNL (NList a _) = a

tailNL :: NonEmptyList a -> [a]
tailNL (NList _ a) = a

initNL :: NonEmptyList a -> [a]
initNL (NList a b) = if null b
                     then [a]
                     else [a] ++ (init b)

lastNL :: NonEmptyList a -> a
lastNL (NList a b) = if null b
                     then a
                     else last b

lengthNL :: NonEmptyList a -> Int
lengthNL (NList a b) = 1 + length b

nl2List :: NonEmptyList a -> [a]
nl2List (NList a b) = [a] ++ b

fromList2NL :: a -> [a] -> NonEmptyList a
fromList2NL a b = NList a b

getNL :: NonEmptyList a -> Int -> a
getNL a index = if (index >= lengthNL a) || (index < 0)
                then let m = "IndexError :: index: " ++ show index ++ " out of bounds "
                         m2 = "number of elements " ++ show (lengthNL a)
                     in traceStack (m ++ m2) (headNL a)
                else (nl2List a) !! index

mapNL :: (a -> b) -> NonEmptyList a -> NonEmptyList b
mapNL f n = let (m:ms) = map f (nl2List n) in fromList2NL m ms

foldlNL :: (a -> b -> a) -> a -> NonEmptyList b -> a
foldlNL f acc n = let ms = nl2List n in foldl f acc ms

zipNL :: NonEmptyList a -> NonEmptyList b -> NonEmptyList (a, b)
zipNL a b = let (m:ms) = zip (nl2List a) (nl2List b) in fromList2NL m ms

sortNL :: Ord a => NonEmptyList a -> NonEmptyList a
sortNL a = let (m:ms) = sort $! nl2List a in fromList2NL m ms

minNL :: Ord a => NonEmptyList a -> a
minNL a = let (m:ms) = nl2List a in minimum (m:ms) 

maxNL :: Ord a => NonEmptyList a -> a
maxNL a = let (m:ms) = nl2List a in maximum (m:ms) 

minmaxByNL :: (a -> a -> Ordering) -> Bool -> NonEmptyList a -> a
minmaxByNL f b m = let ms = nl2List m in if b
                                         then maximumBy f ms
                                         else minimumBy f ms

maximumByNL :: (a -> a -> Ordering) -> NonEmptyList a -> a
maximumByNL f m = minmaxByNL f True m

minimumByNL :: (a -> a -> Ordering) -> NonEmptyList a -> a
minimumByNL f m = minmaxByNL f False m

reverseNL :: NonEmptyList a -> NonEmptyList a
reverseNL a = let (m:ms) = reverse $ nl2List a in fromList2NL m ms

elemNL :: Eq a => a -> NonEmptyList a -> Bool
elemNL m b = let ms = nl2List b in m `elem` ms

findNL :: (a -> Bool) -> NonEmptyList a -> Maybe a
findNL f m = let ms = nl2List m in find f ms

partitionNL :: (a -> Bool) -> NonEmptyList a -> ([a], [a])
partitionNL f m = let ms = nl2List m in partition f ms


instance Eq a => Eq (NonEmptyList a) where
    a == b = (nl2List a) == (nl2List b)

instance Show a => Show (NonEmptyList a) where
    show a = show (nl2List a)

infty :: Scalar
infty = (read "Infinity") :: Scalar

m_pi :: Scalar
m_pi = 3.141592653589793238


clamp :: Ord a => a -> a -> a -> a
clamp x min max = if x < min
                  then min
                  else if x > max
                       then max
                       else x

-- interpolate a value in one range to another range
interp :: (Scalar, Scalar) -> (Scalar, Scalar) -> Scalar -> Scalar
interp (inputStart, inputEnd) (outputStart, outputEnd) value =
    let idiff = (value - inputStart) / (inputEnd - inputStart)
        odiff = outputEnd - outputStart
    in idiff * odiff + outputStart

eqReduce :: Eq a => [a] -> ((a -> Bool) -> [a] -> Bool) -> Bool
eqReduce lst f = case lst of
                    [] -> True
                    (x:xs) -> f (== x) (x:xs)

allEqual :: Eq a => [a] -> Bool
allEqual lst = eqReduce lst all


anyEqual :: Eq a => [a] -> Bool
anyEqual lst = eqReduce lst any

-- enumerate
enumerate :: [a] -> [(Int, a)]
enumerate a = zip [0..((length a)-1)] a

-- take between
takeBetween :: Int -> Int -> [a] -> [a]
takeBetween mnv mxv lst =
    let (mn, mx) = if mnv < mxv
                   then (mnv, mxv)  
                   else (mxv, mnv)
    in if mn < 0
       then traceStack "minimum value is smaller than zero in takeBetween" []
       else if mx > (length lst)
            then let lstlen = "list size " ++ show (length lst)
                     mxstr = "maximum value " ++ show mx
                     msg = "maximum value is bigger than list size " 
                 in traceStack (msg ++ lstlen ++ mxstr) []
            else let enums = enumerate lst
                     pred (i, a) = i >= mn && i <= mx
                     subseq = filter pred enums
                     (nms, els) = unzip subseq
                 in els


