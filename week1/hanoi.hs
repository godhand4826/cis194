type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src goal tmp
    | n <= 0 = []
    | n == 1 = [(src, goal)]
    | otherwise = hanoi (n - 1) src tmp goal ++ [(src, goal)] ++ hanoi (n - 1) tmp goal src

main = print [ hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]]
