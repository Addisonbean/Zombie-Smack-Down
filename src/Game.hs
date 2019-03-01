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

import Command
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

printStatus :: Game -> IO ()
printStatus g = do
  putStrLn $ "Health: " ++ show (g ^. playerHealth)
  putStrLn $ "Zombie health: " ++ show (g ^. currentZombie . health)

execCmd :: Command -> GameState ()
execCmd Punch = do
  oldHealth <- (fmap (view (currentZombie . health)) get)
  modify $ over currentZombie punchZombie
  newHealth <- (fmap (view (currentZombie . health)) get)
  liftIO . putStrLn $ zombieStatus (oldHealth - newHealth)
    where
      zombieStatus dmg = "The zombie took " ++ show dmg ++ " damage!"
execCmd Exit = modify (set gameStatus Exited)
execCmd Status = liftIO . printStatus =<< get
execCmd EmptyCommand = return ()

status :: Game -> GameStatus
status = view gameStatus
