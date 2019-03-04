module EventLoop
  ( start
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

start :: GameState ()
start = eventLoop

eventLoop :: GameState ()
eventLoop = do
  s <- gets status
  case s of
    Start -> liftIO (putStrLn "Welcome to Zombie Smackdown!") >> initGame >> eventLoop
    Exited -> return ()
    Win -> liftIO $ putStrLn "Congrats!"
    _ -> liftIO (prompt "> ") >>= maybe (liftIO $ putStrLn "invalid input") execCmd . parseInput >> eventLoop
