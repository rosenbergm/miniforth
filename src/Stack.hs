module Stack (Stack, empty, push, peek, pop, pop2, fromList, toList, swap, over, rot, clear) where

newtype Stack a = Stack [a] deriving (Show)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

peek :: Stack a -> Maybe a
peek (Stack []) = Nothing
peek (Stack (x : _)) = Just x

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack []) = Nothing
pop (Stack (x : xs)) = Just (x, Stack xs)

pop2 :: Stack a -> Maybe (a, a, Stack a)
pop2 (Stack []) = Nothing
pop2 (Stack [_]) = Nothing
pop2 (Stack (x1 : x2 : xs)) = Just (x1, x2, Stack xs)

fromList :: [a] -> Stack a
fromList = Stack

toList :: Stack a -> [a]
toList (Stack xs) = xs

swap :: Stack a -> Maybe (Stack a)
swap (Stack (x1 : x2 : xs)) = Just $ Stack (x2 : x1 : xs)
swap _ = Nothing

over :: Stack a -> Maybe (Stack a)
over (Stack (x1 : x2 : xs)) = Just $ Stack (x2 : x1 : x2 : xs)
over _ = Nothing

rot :: Stack a -> Maybe (Stack a)
rot (Stack (x1 : x2 : x3 : xs)) = Just $ Stack (x3 : x1 : x2 : xs)
rot _ = Nothing

clear :: Stack a -> Stack a
clear _ = Stack []
