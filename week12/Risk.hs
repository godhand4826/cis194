{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Debug.Trace
import Control.Monad.Random
import Data.List
import Data.Ord

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield as bs) = if as < 2 || bs == 0
        then return bf
        else do
        aUnit <- replicateM (if as > 3 then 3 else as - 1) die
        bUnit <- replicateM (min 2 bs) die
        let match = zipWith (>) (sortOn Down aUnit) (sortOn Down bUnit)
        let aWin = length $ filter id match
        let bWin = length match - aWin
        return $ Battlefield (as - bWin) (bs - aWin)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield as bs) =
    if as < 2 || bs <= 0
    then return bf
    else battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
    bs <- replicateM 1000 (invade bf)
    let wins = length $ filter (\x -> defenders x == 0) bs
    return $ fromIntegral wins / 1000

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield as bs)
    | as < 2 || bs <= 0 = if bs == 0 then 1 else 0
    | otherwise =
        exactSuccessProb (Battlefield as (bs - match)) * z +
        exactSuccessProb (Battlefield (as -1) (bs - match + 1)) * o +
        exactSuccessProb (Battlefield (as -2) bs) * t
        where au = if as > 3 then 3 else as - 1
              bu = min 2 bs
              match = min au bu
              (z,o,t) = f (au, bu)

-- (atk, def) -> (zero, one, two)
f :: (Int, Int) -> (Double, Double, Double)
f (a, b) | a <= 0 || a > 3 || b <= 0 || b > 2 = error "atk , def shoud be positive"
f (1, 1) = (15/26, 21/36, 0)
f (1, 2) = (55/216, 161/216, 0)
f (2, 1) = (125/216, 91/216, 0)
f (2, 2) = (295/1296, 420/ 1296, 581/1296)
f (3, 1) = (750/1296, 546/1296, 0)
f (3, 2) = (2890/7776, 2611/7776, 2275/7776)

main = do
    let b = Battlefield 5 5
    d <- evalRandIO $ successProb b
    print d
    print $ exactSuccessProb b
