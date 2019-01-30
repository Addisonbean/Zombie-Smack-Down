module Lib
  ( gameLoop
  ) where

import EventLoop (eventLoop, GameState(..))

gameLoop :: IO ()
gameLoop = eventLoop GameState
