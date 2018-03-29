{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Parser (parseExp)
import qualified StackVM as SVM 

data ExprT = 
    Lit Integer
  | Add ExprT ExprT
  | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit int)          = int
eval (Add expr1 expr2)  = eval expr1 + eval expr2
eval (Mul expr1 expr2)  = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr str = 
  case parseExp Lit Add Mul str of
    Just expr -> Just $ eval expr
    Nothing   -> Nothing


class Expr a where

  lit :: Integer -> a

  add :: a -> a -> a

  mul :: a -> a -> a
          

instance Expr ExprT where
  lit = Lit

  add = Add

  mul = Mul


reify :: ExprT -> ExprT
reify = id


instance Expr Integer where
  lit a = a

  add a b = a + b

  mul a b = a * b


instance Expr Bool where
  lit a 
    | a <= 0    = False
    | otherwise = True

  add a b = a || b

  mul a b = a && b


newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax

  add (MinMax a) (MinMax b) = 
    if a > b then MinMax a else MinMax b

  mul (MinMax a) (MinMax b) = 
    if a < b then MinMax a else MinMax b


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7

  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7

  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7


-- | Exercise 5
instance Expr SVM.Program where
  -- lit :: Integer -> Program
  lit a = [SVM.PushI a]

  -- add :: Program -> Program -> Program
  add ps ps' = ps ++ ps'

  -- mul :: Program -> Program -> Program
  mul ps ps' = ps ++ ps'


compile :: String -> Maybe SVM.Program
compile str = parseExp lit add mul str

testRunStringOnVM :: String -> Either String SVM.StackVal
testRunStringOnVM str =
  case compile str of
    Just program ->
      SVM.stackVM program

    Nothing      ->
      Left "Compilation failed"


-- | Exercise 6
