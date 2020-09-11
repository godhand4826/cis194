{-# OPTIONS_GHC -Wall #-}
import Data.List

fun1 ::[Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
              deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr f Leaf
    where h Leaf = -1
          h (Node n _ _ _) = n
          f v Leaf = Node 0 Leaf v Leaf
          f v (Node _ l a r) = if h l <= h r
            then Node (max (h l') (h r) + 1) l' a r
            else Node (max (h l) (h r') + 1) l a r'
                where l' = f v l
                      r' = f v r

xor :: [Bool] -> Bool
xor = foldl f False
    where f True True = False
          f False False = False
          f _ _ = True

map' :: (a -> b)->[a]->[b]
map' f = foldr (\x y -> f x:y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z = foldr (flip f) z . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ [1..n] \\ [i+j+2*i*j | i<-[1..n], j<-[1..n], i<=j, i+j+2*i*j <= n]

main :: IO()
main = do
    print $ foldTree "ABCDEFGHIJ"
    print $ xor [False, True, False]
    print $ not $ xor [False, True, False, False, True]
