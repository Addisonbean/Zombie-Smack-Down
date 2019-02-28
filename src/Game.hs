{-# LANGUAGE TemplateHaskell #-}

module Game
  ( GameState
  , initialGame
  , startGame
  , GameStatus(..)
  , status
  , execCmd
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import System.Random

import Zombie

type GameState = StateT Game IO

data GameStatus
  = Start
  | Playing
  | Exited
  | GameOver
  deriving (Show, Eq)

data Game = Game
  { _waves :: [[Zombie]]
  , _currentZombie :: Zombie
  , _playerHealth :: Int
  , _randomGen :: StdGen
  , _gameStatus :: GameStatus
  } deriving (Show)
makeLenses ''Game

initialGame:: StdGen -> Game
initialGame g = Game
  { _waves = waves''
  , _currentZombie = z
  , _playerHealth = 20
  , _randomGen = g1
  , _gameStatus = Start
  } where
      ((z:w1):waves', g1) = runState genWaves g
      waves'' = w1:waves'

startGame :: Game -> Game
startGame = set gameStatus Playing

execCmd :: String -> GameState ()
execCmd "hi" = liftIO $ putStrLn "Hey!"
execCmd "exit" = modify (set gameStatus Exited)
execCmd "punch" = modify (over currentZombie punchZombie)
execCmd "status" = get >>= liftIO . putStrLn . show . view currentZombie
execCmd _ = return ()

status :: Game -> GameStatus
status = view gameStatus
