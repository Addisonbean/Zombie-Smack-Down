module EventLoop
  ( eventLoop
  , GameState (..)
  ) where

import Data.Maybe (fromMaybe)

data Command = Command
  { name :: String
  , arguments :: [String]
  } deriving (Show)

strEq :: String -> String -> Bool
strEq "" "" = True
strEq [_] "" = False
strEq "" [_] = False
strEq (x:xs) (y:ys)
  | x == y = strEq xs ys
  | otherwise = False

parseInput :: String -> Command
parseInput = parseParts . words
  where
    parseParts [] = Command { name = "", arguments = [] }
    parseParts (x:xs) = Command { name = x, arguments = xs }


data Event
  = Quit
  | SayHi
  | DoNothing
  | InvalidCommand

data GameState = GameState

execCmd :: GameState -> Command -> Event
execCmd _ (Command "quit" _) = Quit
execCmd _ (Command "exit" _) = Quit
execCmd _ (Command "hi" _) = SayHi
execCmd _ (Command "" []) = DoNothing
execCmd _ _ = InvalidCommand

execEvent :: GameState -> Event -> IO (Maybe GameState)
execEvent _ Quit = return Nothing
execEvent s SayHi = putStrLn "Hi" >> return (Just s)
execEvent s DoNothing = return $ Just s
execEvent s InvalidCommand = putStrLn "Invalid command" >> return (Just s)

eventLoop :: GameState -> IO ()
eventLoop state = do
  cmd <- parseInput <$> getLine
  
  newState <- execEvent state $ execCmd state cmd

  fromMaybe (pure ()) (eventLoop <$> newState)
