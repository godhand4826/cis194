{-# LANGUAGE FlexibleInstances #-}
import Data.List
import Data.Monoid
import Buffer
import Editor
import Scrabble
import Sized
import StringBuffer

data JoinList m a = Empty
                | Single m a
                | Append m (JoinList m a) (JoinList m a)
                deriving (Eq, Show)

y2=Append (Size 4)
        (Append (Size 3)
            (Single (Size 1) 'y')
            (Append (Size 2) 
                (Single (Size 1) 'e')
                (Single (Size 1) 'a')))
        (Single (Size 1) 'h')

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ Empty = a
Empty +++ b = b
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ i (Append _ l r) = if i < lsize
                          then indexJ i l
                          else indexJ (i - lsize) r
                            where lsize = getSize . size . tag $ l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n j | (getSize . size . tag) j <= n = Empty
dropJ n (Append _ l r) = if n <= lsize
                         then dropJ n l +++ r
                         else dropJ (n - lsize) r
                            where lsize = getSize . size . tag $ l
dropJ _ j = j

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n j | (getSize . size . tag) j <= n = j
takeJ n (Append _ l r) = if n <= lsize
                         then takeJ n l
                         else l +++ takeJ (n - lsize) r
                            where lsize = getSize . size . tag $ l
takeJ _ _ = Empty

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jlToList
    fromString = asJoinList . lines
    line = indexJ
    replaceLine n s jl = takeJ n jl +++ asJoinList [s] +++ dropJ (n+1) jl
    numLines = getSize . size . tag
    value = getScore . fst . tag

asJoinList :: [String] -> JoinList (Score, Size) String
asJoinList [] = Empty
asJoinList [s] = Single (scoreString s, 1)  s
asJoinList ls = asJoinList l +++ asJoinList r
        where (l, r) = splitAt (length ls `div` 2) ls

main = runEditor editor $ asJoinList [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]