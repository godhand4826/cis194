{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where
import Data.Char

newtype Score = Score Int
                deriving (Show, Eq, Num, Ord)

getScore :: Score -> Int
getScore (Score n) = n

instance Monoid Score where
    mempty = 0
    mappend = (+)

score :: Char -> Score
score c
    | c' `elem` "aeilnorstu" = 1
    | c' `elem` "dg" = 2
    | c' `elem` "bcmp" = 3
    | c' `elem` "fhvwy" = 4
    | c' `elem` "k" = 5
    | c' `elem` "jx" = 8
    | c' `elem` "qz" = 10
    | otherwise = 0
    where c' = toLower c

scoreString :: String -> Score
scoreString = sum . map score