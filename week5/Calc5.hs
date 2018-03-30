{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc5 where

import Parser (parseExp)
import StackVM 

class Expr a where

  lit :: Integer -> a

  add :: a -> a -> a

  mul :: a -> a -> a
 
instance Expr Program where
  -- lit :: Integer -> Program
  lit a = [PushI a]

  -- add :: Program -> Program -> Program
  add ps ps' = ps ++ ps'

  -- mul :: Program -> Program -> Program
  mul ps ps' = ps ++ ps'


compile :: String -> Maybe Program
compile str = parseExp lit add mul str

testRunStringOnVM :: String -> Either String StackVal
testRunStringOnVM str =
  case compile str of
    Just program ->
      stackVM program

    Nothing      ->
      Left "Compilation failed"
