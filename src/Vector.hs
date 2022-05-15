-- vector
module Vector where

import Helper
import Common
import BaseEnum
import Debug.Trace

data Vector = VList (NonEmptyList Scalar)

instance Eq Vector where
    (VList a) == (VList b) = (nl2List a) == (nl2List b)

fromList2Vec :: Scalar -> [Scalar] -> Vector
fromList2Vec a b = VList (fromList2NL a b)

vec2List :: Vector -> [Scalar]
vec2List (VList a) = nl2List a

instance Show Vector where
    show a =
        let msg1 = "<Vector " ++ "size " ++ show (vsize a)
            msg2 = msg1 ++ " data " ++ (unwords $ map show (vec2List a))
            msg3 = msg2 ++ " >"
        in msg3

singularV :: Int -> Scalar -> Vector
singularV size v = fromList2Vec v (replicate (size - 1) v)

zeroV :: Int -> Vector
zeroV size = singularV size 0.0

zeroLikeVector :: Vector -> Vector
zeroLikeVector a = zeroV (vsize a)

inftyV :: Int -> Vector
inftyV size = singularV size infty

negInftyV :: Int -> Vector
negInftyV size = singularV size (-infty)

vsize :: Vector -> Int
vsize (VList v) = lengthNL v

vget :: Vector -> Int -> Scalar
vget (VList v) index = getNL v index

nearZeroVec :: Vector -> Bool
nearZeroVec (VList vs) =
    let nzero = 1e-10
        foldfn (x:[]) = x
        foldfn (x:xs) = x && foldfn xs
    in foldfn $ map ((< nzero) . abs) (nl2List vs)

instance BinaryOp Vector where
    scalar_op opname fnc v =
        let (b:bs) = map fnc (vec2List v) in fromList2Vec b bs

    element_wise_op opname fnc v1 v2 =
        if (vsize v1) /= (vsize v2)
        then traceStack (sizeError v1 v2 opname) v1
        else let (b:bs) = zipWith fnc (vec2List v1) (vec2List v2)
             in fromList2Vec b bs
        where sizeError a1 a2 name =
                let msg = "vector sizes: " ++ (show $ vsize a1)
                    msg2 = " and " ++ (show $ vsize a2)
                    msg3 = " are incorrect for operation " ++ name
                in msg ++ msg2 ++ msg3
    div v e =
        let es = vec2List e
        in if 0.0 `elem` es
           then traceStack ("vector contains zero in a division operation") v
           else element_wise_op "divide" (/) v e

