module EventLoop
  ( eventLoop
  , initialGame
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import System.IO
import System.Random

import Game
import Command

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

eventLoop :: GameState ()
eventLoop = do
  g <- get
  case status g of
    Start -> liftIO (putStrLn "Welcome to Zombie Smackdown!") >> modify startGame >> eventLoop
    Exited -> return ()
    _ -> liftIO (prompt "> ") >>= maybe (liftIO $ putStrLn "invalid input") execCmd . parseInput >> eventLoop
