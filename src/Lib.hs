module Lib
  ( gameLoop
  ) where

import Control.Monad.Trans.State
import System.Random

import EventLoop
import Game

gameLoop :: IO ()
gameLoop = getStdGen >>= runStateT eventLoop . initialGame >> return ()
