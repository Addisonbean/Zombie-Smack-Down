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
  | Win
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

-- use State? (StateT [[Zombie]] (Maybe Zombie))?
nextZombie :: [[Zombie]] -> Maybe (Zombie, [[Zombie]])
nextZombie [] = Nothing
nextZombie ([]:ws) = nextZombie ws
nextZombie (w:ws) = Just (z, zs:ws)
  where
    ([z], zs) = splitAt 1 w

ko :: GameState ()
ko = do
  liftIO $ putStrLn "KO!"
  ws <- gets $ view waves
  case nextZombie ws of
    Just (newZombie, newWaves) -> do
      modify $ set currentZombie newZombie
      modify $ set waves newWaves
    Nothing -> modify $ set gameStatus Win

-- TODO: use RandT in the StateT
attackZombie :: (Int, Int) -> GameState Int
attackZombie range = do
  g <- gets $ view randomGen
  let (dmg, g1) = randomR range g
  modify (set randomGen g1)
  modify $ over currentZombie (damageZombie dmg)
  return dmg

execCmd :: Command -> GameState ()
execCmd Punch = do
  dmg <- attackZombie (1, 2)
  liftIO $ putStrLn (zombieStatus dmg)

  z <- gets $ view currentZombie
  when (not $ isZombieAlive z) ko
    where
      zombieStatus dmg = "The zombie took " ++ show dmg ++ " damage!"
execCmd Exit = modify (set gameStatus Exited)
execCmd Status = liftIO . printStatus =<< get
execCmd EmptyCommand = return ()

status :: Game -> GameStatus
status = view gameStatus
