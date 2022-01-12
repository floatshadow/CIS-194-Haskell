{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
                   case wordList of
                   ("I":time:msg) -> LogMessage Info (read time :: TimeStamp) (unwords msg)
                   ("W":time:msg) -> LogMessage Warning (read time :: TimeStamp) (unwords msg)
                   ("E":level:time:msg) -> LogMessage (Error (read level :: Int)) (read time :: TimeStamp) (unwords msg)
                   _ -> Unknown (unwords wordList)


parse :: String -> [LogMessage]
parse = map parseMessage . lines



insert :: LogMessage -> MessageTree -> MessageTree
insert logmsg Leaf = Node Leaf logmsg Leaf
insert logmsg@(LogMessage _ time _) (Node left nodemsg@(LogMessage _ _time _) right)
  | time > _time = Node left nodemsg (insert logmsg right)
  | otherwise  = Node (insert logmsg left) nodemsg right
{- otherwise case -}
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (msg:msgList) = insert msg (build msgList)
{- suggested by hlint: foldr insert Leaf msgList -}


inOrder :: MessageTree -> [LogMessage]
inOrder (Node Leaf nodemsg Leaf) = [nodemsg]
inOrder (Node left nodemsg right) = (inOrder left) ++ [nodemsg] ++ (inOrder right)
inOrder Leaf = []


{-whatWentWrong :: [LogMessage] -> [String]
whatWentWrong (msg@(LogMessage (Error level) _ str):msgList)
  | level >= 50 = str:whatWentWrong msgList
  | otherwise = whatWentWrong msgList
whatWentWrong (msg@LogMessage {}:msgList) = whatWentWrong msgList
whatWentWrong [] = []
-}

getString :: [LogMessage] -> [String]
getString (LogMessage _ _ str :msgList) = str:(getString msgList)
getString _ = []


check :: Int -> LogMessage -> Bool
check threshold (LogMessage (Error level) _ _ )
  | threshold <= level = True 
  | otherwise = False 
check _ _ = False 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = getString. inOrder . build . filter (check 50)




