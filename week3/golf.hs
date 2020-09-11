{-# OPTIONS_GHC -Wall #-}
module Golf where
import Data.List

skips :: [a] -> [[a]]
skips xs = map (`f` xs) [0.. length xs - 1]
    where f n = map snd . filter ((==0) . (`mod` (n+1)) . (+1) . fst) . zip [0..]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:r@(y:z:_))
    | x < y && y > z = y : localMaxima r
    | otherwise = localMaxima r
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = (unlines . transpose . map f $ c) ++ "==========\n0123456789\n"
    where c = count xs
          m = maximum c
          f n = replicate (m-n) ' ' ++ replicate n '*'

count ::[Integer] -> [Int]
count xs = map (\n -> length $ filter (==n) xs) [0..9]

main :: IO ()
main = do
    print [ skips "ABCD" == ["ABCD", "BD", "C", "D"]
        , skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
        , skips [1] == ([[1]] :: [[Int]])
        , skips [True,False] == [[True,False], [False]]
        , skips [] == ([] :: [[Int]]) ]
    print [ localMaxima [2,9,5,6,1] == [9,6]
        , localMaxima [2,3,4,1,5] == [4]
        , localMaxima [1,2,3,4,5] == ([]::[Integer]) ]
    print [
        histogram [1,1,1,5] == " *        \n *        \n *   *    \n==========\n0123456789\n",
        histogram [3,5] == "   * *    \n==========\n0123456789\n" ]
