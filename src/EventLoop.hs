{-# LANGUAGE TemplateHaskell #-}

module EventLoop
  ( eventLoop
  , initialGame
  ) where

import System.Random
import Control.Monad.State
import Control.Monad.Trans.State (StateT)
import Control.Lens
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
  { _zombieType :: String
  , _health :: Int
  , _power :: (Int, Int)
  } deriving (Show)
makeLenses ''Zombie

punchZombie :: Zombie -> Zombie
punchZombie = over health $ subtract 1

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
  return Zombie { _zombieType = typeName t, _health = h, _power = powerRange t }

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
      types = [basicZombieType, toughZombieType]
      ((z:w1):waves', g1) = runState (makeWaves types) g
      waves'' = w1:waves'

execCmd :: String -> GameState ()
execCmd "hi" = liftIO $ putStrLn "Hey!"
execCmd "exit" = modify (set gameStatus Exited)
execCmd "punch" = modify (over currentZombie punchZombie)
execCmd "status" = get >>= liftIO . putStrLn . show . view currentZombie
execCmd _ = return ()

eventLoop :: GameState ()
eventLoop = do
  g <- get
  case _gameStatus g of
    Start -> liftIO (putStrLn "Welcome to Zombie Smackdown!") >> modify (set gameStatus Playing) >> eventLoop
    Exited -> return ()
    _ -> liftIO getLine >>= execCmd >> eventLoop
