{-# LANGUAGE TemplateHaskell #-}

module Game
  ( GameState
  , initialGame
  , initGame
  , GameStatus(..)
  , status
  , execCmd
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Random
import Control.Monad.Random
import System.Random

import Command
import Zombie

type GameState = StateT Game (RandT StdGen IO)

data GameStatus
  = Start
  | Playing
  | Exited
  | GameOver
  | Win
  deriving (Show, Eq)

data Game = Game
  { _waves :: [(ZombieType, Int)]
  , _currentZombie :: Zombie
  , _playerHealth :: Int
  , _gameStatus :: GameStatus
  } deriving (Show)
makeLenses ''Game

initialGame :: Game
initialGame = Game
  { _waves = waveTypes
  , _currentZombie = blankZombie
  , _playerHealth = 20
  , _gameStatus = Start
  }

nextWave :: GameState ()
nextWave = do
  modify $ over waves tail
  done <- fmap null $ gets (view waves)
  if done
     then modify (set gameStatus Win)
     else nextZombie

nextZombie :: GameState ()
nextZombie = do
  (zombieType, remaining) <- fmap head $ gets (view waves)
  if remaining == 0
     then nextWave
     else do
       z <- lift $ makeZombie zombieType
       modify $ set currentZombie z
       modify $ over (waves . _head . _2) (subtract 1)

initGame :: GameState ()
initGame = modify (set gameStatus Playing) >> nextZombie

printStatus :: Game -> IO ()
printStatus g = do
  putStrLn $ "Health: " ++ show (g ^. playerHealth)
  putStrLn $ "Zombie: " ++ g ^. currentZombie . zombieType
  putStrLn $ "Zombie health: " ++ show (g ^. currentZombie . health)

ko :: GameState ()
ko = do
  liftIO $ putStrLn "KO!"
  ws <- gets $ view waves
  nextZombie

attackZombie :: (Int, Int) -> GameState Int
attackZombie range = do
  dmg <- getRandomR range
  modify $ over currentZombie (damageZombie dmg)
  return dmg

attackPlayer :: GameState Int
attackPlayer = do
  z <- gets $ view currentZombie
  dmg <- lift $ zombieDoAttack z
  modify $ over playerHealth (subtract dmg)
  return dmg

combat :: GameState ()
combat = do
  dmg <- attackZombie (1, 2)
  liftIO $ putStrLn ("The zombie took " ++ show dmg ++ " damage!")

  z <- gets $ view currentZombie
  if not $ isZombieAlive z
     then ko
     else do
       hurt <- attackPlayer
       liftIO $ putStrLn ("You took " ++ show hurt ++ " damage!")

execCmd :: Command -> GameState ()
execCmd Punch = combat
execCmd Exit = modify (set gameStatus Exited)
execCmd Status = liftIO . printStatus =<< get
execCmd EmptyCommand = return ()

status :: Game -> GameStatus
status = view gameStatus
