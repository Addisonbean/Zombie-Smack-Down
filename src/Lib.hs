module Lib
  ( gameLoop
  ) where

import EventLoop (eventLoop, initialGame)
import System.Random
import Control.Monad.Trans.State


gameLoop :: IO ()
gameLoop = getStdGen >>= runStateT eventLoop . initialGame >> return ()
