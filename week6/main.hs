{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0:1:zipWith (+) fibs2 (tail fibs2)

data Stream a = a :. Stream a
infixr 5 :.

streamToList :: Stream a -> [a]
streamToList (x :. xs) = x:streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = x :. streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :. xs) = f x :. streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = x :. streamFromSeed f (f x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :. xs) ys = x :. interleaveStreams ys xs

ruler :: Stream Integer
ruler = f 0
    where f n = interleaveStreams (streamRepeat n) (f (n+1))

x :: Stream Integer
x = 0 :. 1 :. streamRepeat 0

instance Num (Stream Integer) where
    (a:.as) + (b:.bs) = a+b :. as+bs
    (a:.as) * bb@(b:.bs) = a*b :. streamMap (a*) bs + as * bb
    fromInteger n = n :. streamRepeat 0
    negate = streamMap negate

instance Fractional (Stream Integer) where
    (a:.as) / (b:.bs) = q
        where q = streamMap (`div` b) (a :. as - q * bs)
    fromRational = undefined

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

type Matrix = [[Integer]]
fm :: Matrix
fm = [[1, 1], [1, 0]]

instance Num Matrix where
   [[a,b],[c,d]] * [[a',b'],[c',d']] = [[a * a' + b * c', a*c' + b * d'], [b*a'+ d*c', b * b'+ d * d']]

fib4 :: Integer -> Integer
fib4 n = (fm ^ (n+1)) !! 1 !! 1

main :: IO ()
main = print ""
