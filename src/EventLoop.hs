module EventLoop
  ( eventLoop
  , initialGame
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import System.Random

import Game
import Utility

data Command = Command
  { name :: String
  , arguments :: [String]
  } deriving (Show)

parseInput :: String -> Command
parseInput = parseParts . words
  where
    parseParts [] = Command { name = "", arguments = [] }
    parseParts (x:xs) = Command { name = x, arguments = xs }

eventLoop :: GameState ()
eventLoop = do
  g <- get
  case status g of
    Start -> liftIO (putStrLn "Welcome to Zombie Smackdown!") >> modify startGame >> eventLoop
    Exited -> return ()
    _ -> liftIO getLine >>= execCmd >> eventLoop
