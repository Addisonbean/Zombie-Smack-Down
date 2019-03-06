module Lib
  ( gameLoop
  ) where

import Control.Monad.Trans.Random (evalRandT)
import Control.Monad.Trans.State (evalStateT)
import System.Random (getStdGen)

import EventLoop (start)
import Game (initialGame)

gameLoop :: IO ()
gameLoop = getStdGen >>= evalRandT (evalStateT start initialGame) >> return ()
