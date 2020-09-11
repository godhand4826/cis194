import Control.Monad
import Data.List

toDigits :: Integer -> [Integer]
toDigits n | n > 0 = map (read . (:[])) $ show n
toDigits _ = []

toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . f . reverse
    where f (x:y:ys) = x: 2 * y : f ys
          f xs = xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . doubleEveryOther . toDigits

main = print
        [ toDigits 1234 == [1,2,3,4]
        , toDigitsRev 1234 == [4,3,2,1]
        , toDigits 0 == []
        , toDigits (-17) == []
        , doubleEveryOther [8,7,6,5] == [16,7,12,5]
        , doubleEveryOther [1,2,3] == [1,4,3]
        , sumDigits [16,7,12,5] == 22
        , validate 4012888888881881 == True
        , validate 4012888888881882 == False ]
