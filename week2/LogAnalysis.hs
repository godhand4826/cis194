{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log
import Text.Read(readMaybe)
import Data.Maybe

parseMessage :: String -> LogMessage
parseMessage l = let ws = words l in
                case ws of
                ("I":t:xs) | isJust (readMaybe t :: Maybe Int)
                    -> LogMessage Info time msg
                        where Just time = readMaybe t
                              msg = unwords xs
                ("W":t:xs) | isJust (readMaybe t :: Maybe Int)
                    -> LogMessage Warning time msg
                        where Just time = readMaybe t
                              msg = unwords xs
                ("E":c:t:xs) | isJust (readMaybe c :: Maybe Int) && isJust (readMaybe t :: Maybe Int)
                    -> LogMessage (Error code) time msg
                        where Just code = readMaybe c
                              Just time = readMaybe t
                              msg = unwords xs
                _ -> Unknown l

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ t _) (Node l m@(LogMessage _ t2 _) r)
    | t > t2  = Node l m (insert logMsg r)
    | otherwise = Node (insert logMsg l) m r
insert _ t = t

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = map msg $ filter relevant logs
    where relevant x = case x of
                        (LogMessage (Error c) _ _) | c >= 50 -> True
                        _ -> False
          msg (LogMessage _ _ s) = s
          msg _ = ""

main :: IO()
main = do
    print
        [ parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
        , parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
        , parseMessage "This is not in the right format" == Unknown "This is not in the right format"]
    s <- testWhatWentWrong parse whatWentWrong "sample.log"
    print [s == [ "Way too many pickles" , "Bad pickle-flange interaction detected" , "Flange failed!" ] ]
