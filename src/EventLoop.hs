module EventLoop
  ( eventLoop
  , initialGame
  ) where

import Data.Maybe (fromMaybe)
import System.Random
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import Utility (strEq)

data Command = Command
  { name :: String
  , arguments :: [String]
  } deriving (Show)

parseInput :: String -> Command
parseInput = parseParts . words
  where
    parseParts [] = Command { name = "", arguments = [] }
    parseParts (x:xs) = Command { name = x, arguments = xs }

data Zombie = Zombie
  { zombieType :: String
  , health :: Int
  , power :: (Int, Int)
  } deriving (Show)

data ZombieType = ZombieType
  { typeName :: String
  , healthRange :: (Int, Int)
  , powerRange :: (Int, Int)
  } deriving (Show)

basicZombieType :: ZombieType
basicZombieType = ZombieType
  { typeName = "basic"
  , healthRange = (5, 8)
  , powerRange = (3, 4)
  }

toughZombieType :: ZombieType
toughZombieType = ZombieType
  { typeName = "tough"
  , healthRange = (7, 10)
  , powerRange = (4, 6)
  }

makeZombie :: ZombieType -> State StdGen Zombie
makeZombie t = do
  h <- state (randomR (healthRange t))
  return Zombie { zombieType = typeName t, health = h, power = powerRange t }

makeWaves :: [ZombieType] -> State StdGen [[Zombie]]
makeWaves [] = return []
makeWaves (t:ts) = do
  z1 <- makeZombie t
  z2 <- makeZombie t
  z3 <- makeZombie t
  zs <- makeWaves ts
  return ([z1, z2, z3]:zs)

type GameState = StateT Game IO

data GameStatus
  = Started
  | Exited
  | GameOver
  deriving (Show, Eq)

data Game = Game
  { waves :: [[Zombie]]
  , playerHealth :: Int
  , randomGen :: StdGen
  , gameStatus :: GameStatus
  } deriving (Show)

initialGame:: StdGen -> Game
initialGame g = Game { waves = evalState (makeWaves waves') g, playerHealth = 20, randomGen = g, gameStatus = Started }
  where waves' = [basicZombieType, toughZombieType]

execCmd :: String -> StateT Game IO ()
execCmd "hi" = liftIO $ putStrLn "Hey!"
execCmd "exit" = state $ \g -> ((), g { gameStatus = Exited })
execCmd _ = return ()

eventLoop :: GameState ()
eventLoop = do
  execCmd =<< liftIO getLine
  g <- get
  if gameStatus g == Exited
     then return ()
     else eventLoop
