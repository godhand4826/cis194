import Data.Array
import Data.List

knapsack01 :: [Double]   -- values
           -> [Integer]  -- nonnegative weights
           -> Integer    -- knapsack size
           -> Double     -- max possible value
knapsack01 vs ws maxW = f!(numItems-1, maxW)
    where numItems = length vs
          indices = ((-1, 0), (numItems-1, maxW))
          f = foldl' g (listArray indices (repeat 0)) (range indices)
              where g m iter = case iter of
                        (-1, _) -> m
                        (i, w) | ws!!i > w -> m // [((i,w), m!(i-1, w))]
                        (i, w) -> m // [((i,w), max (m!(i-1, w)) (m!(i-1, w - (ws !! i)) + vs !! i))]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
main = print example
