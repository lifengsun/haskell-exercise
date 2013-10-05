{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Ex.1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : t : msg) -> LogMessage Info (read t :: TimeStamp) (unwords msg)
  ("W" : t : msg) -> LogMessage Warning (read t :: TimeStamp) (unwords msg)
  ("E" : err : t : msg) -> LogMessage (Error (read err :: Int))
                           (read t :: TimeStamp) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Ex.2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg'@(LogMessage _ t' _) (Node left msg@(LogMessage _ t _) right) =
  if t' < t
  then Node (insert msg' left) msg right
  else if t' > t
       then Node left msg (insert msg' right)
       else Node left msg' right
insert _ (Node _ (Unknown _) _) = error "Unknown LogMessage in tree."

-- Ex.3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Ex.4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [msg] ++ (inOrder right)

-- Ex.5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter f . inOrder . build
  where f (LogMessage (Error err) _ _) = err >= 50
        f _ = False
        getMsg (LogMessage _ _ msg) = msg
        getMsg _ = error "none Error LogMessage"

-- Ex.6
-- mustard exhausted
