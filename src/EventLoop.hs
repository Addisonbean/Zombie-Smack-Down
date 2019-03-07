module EventLoop
  ( start
  ) where

import Control.Monad.State (gets, liftIO)
import System.IO (hFlush, stdout)

import Command (parseInput)
import Game (initGame, status, execCmd, GameState, GameStatus(..))

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
    GameOver -> liftIO $ putStrLn "Game over..."
    _ -> liftIO (prompt "> ") >>= maybe (liftIO $ putStrLn "invalid input") execCmd . parseInput >> eventLoop
