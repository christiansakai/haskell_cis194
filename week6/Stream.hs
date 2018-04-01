{-# OPTIONS_GHC -Wall #-}

module Stream where

data Stream el = 
    Empty
  | Cons el (Stream el)

streamToList :: Stream a -> [a]
streamToList Empty            = []
streamToList (Cons el stream) = el : streamToList stream

instance Show a => Show (Stream a) where
  -- show :: Stream a -> String
  show Empty            = ""
  show (Cons el Empty)  = show el
  show (Cons el stream) = show el ++ ", " ++ show stream

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap _ Empty            = Empty
streamMap f (Cons el stream) = Cons (f el) (streamMap f stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a 
interleaveStreams Empty a   = a
interleaveStreams a Empty   = a
interleaveStreams (Cons el str) (Cons el' str') =
  Cons el (Cons el' (interleaveStreams str str'))

-- ruler :: Stream Integer
-- ruler = streamFromSeed 

