{-# OPTIONS_GHC -Wall #-} 

module JoinList where

data JoinList m a = 
    Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl    = jl 
(+++) jl Empty    = jl 
(+++) jl jl' = Append (tag jl `mappend` tag jl') jl jl' 

tag :: Monoid m => JoinList m a -> m
tag Empty           = mempty
tag (Single m _)    = m 
tag (Append m _ _)  = m
