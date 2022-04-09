{-# LANGUAGE FlexibleInstances, 
    FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
            --------Exercise 1--------
              ---The popular way---

--Computes the n-th fibonacci number
--the bad way = SLOWER
fib :: Integer -> Integer
fib 0 = 0   --base case
fib 1 = 1   --base case
fib n = fib (n - 1) + fib (n - 2)   --recursion -> f n = f n - 1 + f n - 2

--Infinite list of fibonacci
fibs1 :: [Integer]
fibs1 = map fib [0..]

            --------Exercise 2--------
                ---The zip way---

--Infinite list of fibonacci
--the good way = WAY FASTER
fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

data Stream a =  Cons a (Stream a)

--Show stream just shows the first n elements
instance Show a => Show (Stream a) where
    show stream = show (take n $ streamToList stream)
        where n = 20

listToStream :: [a] -> Stream a
listToStream (x:xs) = Cons x (listToStream xs)
listToStream [] = error "Finite stream error!"  --in case the list is finite

            --------Exercise 3--------
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x:(streamToList  xs)

            --------Exercise 4--------
streamRepeat :: a -> Stream a
streamRepeat r = Cons r (streamRepeat r)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons x stream) = Cons (func x) (streamMap func stream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed rule seed = Cons (rule seed) (streamFromSeed rule (rule seed))

            --------Exercise 5--------
nats :: Stream Integer
nats = streamFromSeed (\x -> x + 1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams s1 s2 = listToStream ((head l1):(head l2):(streamToList (interleaveStreams (listToStream $ tail l1) (listToStream $ tail l2))))  
    where l1 = streamToList s1
          l2 = streamToList s2

--Playing around with the above function I came upon this
--(interleaveStreams (streamRepeat 0) (interleaveStreams (streamRepeat 1) (interleaveStreams (streamRepeat 2) (streamRepeat 3))))

--Which in turn made me come up with this solution :D
ruler :: Stream Integer 
ruler = rulerAux 0
    where rulerAux st = interleaveStreams (streamRepeat st) (rulerAux $ st + 1) -- needed an argument to 'store' the incremental call of the 
                                                                                -- streamRepeat function (rulerAux x calls rulerAux x + 1)

            --------Exercise 6 (Optional)--------
                  ---III.The stream way---

--x = 0 + 1 * x + 0 * x ^ 2 + 0 * x ^ 3 + ...
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

--Multiplies each element of a stream by a scalar
scalarProd :: Integer -> Stream Integer -> Stream Integer 
scalarProd v (Cons x s) = Cons (v * x) (scalarProd v s)

--Now you can evaluate things like (x+1)^2,
--(x+(x+12)^3)^12 etc in ghci
instance Num (Stream Integer) where
    fromInteger x = Cons x (streamRepeat 0) -- fromInteger n = n + 0*x + 0*x^2 + 0*x^3 + . . . . :: Stream Integer
    negate s = scalarProd (-1) s            -- negates every element
    (+) (Cons x1 s1) (Cons x2 s2) = Cons (x1 + x2) (s1 + s2)    -- adds every element
    (*) (Cons x1 s1) (Cons x2 s2) = Cons (x1 * x2) ((scalarProd x1 s2) + s1 * (Cons x2 s2)) -- multiplies 2 streams

instance Fractional (Stream Integer) where
    (/) (Cons x1 s1) (Cons x2 s2) = Cons (div x1 x2) ((1 / (fromIntegral x2)) * (s1 - q * s2))  -- division between two streams
        where q = (Cons x1 s1) / (Cons x2 s2)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

            --------Exercise 7 (Optional)--------
                ---IV.The matrix power way---

type Line = [Integer]   -- For readability :D
type FibMatrix = [Line]    -- Only use the 2 x 2 case

instance Num Line where
    (+) l1 l2 = zipWith (+) l1 l2

instance Num FibMatrix where
    (+) m1 m2 = zipWith (+) m1 m2
    (*) [[a11,a12],[a21,a22]] [[b11,b12],[b21,b22]] = [[a11*b11 + a12*b21,a11*b12 + a12*b22], [a21*b11 + a22*b21, a21*b12 + a22*b22]]
    (*) _ _ = error "Not a 2x2 matrix!" --

-- Base fib matrix <=> fibMatrix ^ 1
f1 :: FibMatrix
f1 = [[1,1],[1,0]]

-- Computes the n-th fib matrix
fibMatrix :: Integer -> FibMatrix
fibMatrix n = f1 ^ n

-- Extracts the n-th fibonacci number from a fib matrix
getFib :: FibMatrix -> Integer 
getFib = head . tail . head 

-- Very fast method
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (getFib . fibMatrix) n