-- common operations for multi dimensional constructs
module Common where

import Debug.Trace

-- 
import BaseEnum

type OpName = String -- for debugging purposes

class Eq a => BinaryOp a where
    scalar_op :: OpName -> (Scalar -> Scalar) -> a -> a
    element_wise_op :: OpName -> (Scalar -> Scalar -> Scalar) -> a -> a -> a
    plus :: a -> a -> a
    plus a b = element_wise_op "plus" (+) a b

    splus :: a -> Scalar -> a
    splus a b = let scalar_fn s = s + b in scalar_op "plus" scalar_fn a

    minus :: a -> a -> a
    minus a b = element_wise_op "minus" (-) a b
    sminus :: a -> Scalar -> a
    sminus a b = let scalar_fn s = s - b in scalar_op "minus" scalar_fn a

    times :: a -> a -> a
    times a b = element_wise_op "times" (*) a b
    stimes :: a -> Scalar -> a
    stimes a b = let scalar_fn s = s * b in scalar_op "times" scalar_fn a

    --
    div :: a -> a -> a
    -- div a b = element_wise_op "div" (/)
    sdiv :: a -> Scalar -> a
    sdiv a b = if b == 0.0
               then traceStack "ZeroDivisionError :: the scalar argument is 0" a
               else let scalar_fn s = s / b in scalar_op "div" scalar_fn a
