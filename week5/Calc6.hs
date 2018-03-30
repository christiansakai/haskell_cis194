{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc6 where

import Prelude hiding (map, exp)
import qualified Data.Map as M

class HasVars a where
  var :: String -> a

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data VarExprT =
    Lit Integer
  | Var String
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance HasVars VarExprT where
  var str = Var str

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var str = \map -> M.lookup str map

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = \_ -> Just a

  add mToI mToI' = \map ->
    case mToI map of
      Nothing -> Nothing
      Just a  -> 
        case mToI' map of
          Nothing -> Nothing
          Just b  -> Just $ a + b

  mul mToI mToI' = \map ->
    case mToI map of
      Nothing -> Nothing
      Just a  -> 
        case mToI' map of
          Nothing -> Nothing
          Just b  -> Just $ a * b

withVars :: [(String, Integer)]
        -> (M.Map String Integer -> Maybe Integer)
        -> Maybe Integer
withVars vars exp = exp $ M.fromList vars


test :: Maybe Integer
test = withVars [("x", 6)] $ add (lit 3) (var "x")

test' :: Maybe Integer
test' = withVars [("x", 6)] $ add (lit 3) (var "y")

test'' :: Maybe Integer
test'' = withVars [("x", 6), ("y", 3)] $ 
  mul (var "x") (add (var "y") (var "x"))


