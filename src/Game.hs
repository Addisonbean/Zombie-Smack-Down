{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Game
  ( GameState
  , initialGame
  , initGame
  , GameStatus(..)
  , status
  , execCmd
  ) where

import Control.Arrow (second)
import Control.Lens (over, set, view, (^.), makeLenses, _2, _head)
import Control.Monad.Random (getRandomR, lift)
import Control.Monad.State (get, gets, liftIO, modify, when, state)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Random (RandT)
import System.Random (StdGen)

import Command (Command(..))
import Zombie
  ( ZombieType
  , blankZombie
  , Zombie
  , isZombieAlive
  , zombieDoAttack
  , zombieType
  , makeZombie
  , damageZombie
  , waveTypes
  , health
  , xpGiven
  )

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
  , _xp :: Int
  } deriving (Show)
makeLenses ''Game

initialGame :: Game
initialGame = Game
  { _waves = waveTypes
  , _currentZombie = blankZombie
  , _playerHealth = 20
  , _gameStatus = Start
  , _xp = 5
  }

type Attack = (Int, Int)

punch :: Attack
punch = (4, 5)

kick :: Attack
kick = (3, 6)

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
  putStrLn $ "Xp: " ++ show (g ^. xp)
  putStrLn $ "Zombie: " ++ (g ^. currentZombie . zombieType)
  putStrLn $ "Zombie health: " ++ show (g ^. currentZombie . health)

giveXp :: Int -> Game -> Game
giveXp = over xp . (+)

ko :: GameState ()
ko = do
  liftIO $ putStrLn "KO!"

  xp <- gets $ view (currentZombie . xpGiven)
  modify $ giveXp xp
  liftIO $ putStrLn ("+" ++ show xp ++ " xp!")

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

playerAlive :: Game -> Bool
playerAlive = (> 0) . view playerHealth

combat :: Attack -> GameState ()
combat attack = do
  dmg <- attackZombie attack
  liftIO $ putStrLn ("The zombie took " ++ show dmg ++ " damage!")

  z <- gets $ view currentZombie
  if not $ isZombieAlive z
     then ko
     else do
       hurt <- attackPlayer
       liftIO $ putStrLn ("You took " ++ show hurt ++ " damage!")

       alive <- gets playerAlive
       when (not alive) gameOver

gameOver :: GameState ()
gameOver = modify $ set gameStatus GameOver

heal :: Int -> Game -> (Bool, Game)
heal x g
  | view xp g < x = (False, g)
  | otherwise = (True, update g)
    where
      update = over xp (subtract x) . over playerHealth (+x)

doHeal :: Int -> GameState ()
doHeal x = do
  success <- state $ heal x
  if success
     then liftIO $ putStrLn ("+" ++ show x ++ " health!")
     else liftIO $ putStrLn "Not enough xp"

execCmd :: Command -> GameState ()
execCmd Punch = combat punch
execCmd Kick = combat kick
execCmd (Heal x) = doHeal x
execCmd Exit = modify (set gameStatus Exited)
execCmd Status = liftIO . printStatus =<< get
execCmd EmptyCommand = return ()

status :: Game -> GameStatus
status = view gameStatus
