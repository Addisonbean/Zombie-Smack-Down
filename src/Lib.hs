module Lib
  ( gameLoop
  ) where

import Control.Monad.Trans.State
import Control.Monad.Trans.Random
import System.Random

import EventLoop
import Game

gameLoop :: IO ()
-- gameLoop = getStdGen >>= evalStateT eventLoop . initialGame >>= return
gameLoop = do
  g <- getStdGen
  evalRandT (evalStateT start initialGame) g
  return ()
