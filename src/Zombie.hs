{-# LANGUAGE TemplateHaskell #-}

module Zombie
  ( Zombie
  , ZombieType(..)
  , blankZombie
  , damageZombie
  , makeZombie
  , waveTypes
  , zombieType
  , health
  , power
  , isZombieAlive
  ) where

import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Random
import Control.Monad.Random
import System.Random

data Zombie = Zombie
  { _zombieType :: String
  , _health :: Int
  , _power :: (Int, Int)
  } deriving (Show)
makeLenses ''Zombie

blankZombie :: Zombie
blankZombie = Zombie
  { _zombieType = ""
  , _health = 0
  , _power = (0, 0)
  }

damageZombie :: Int -> Zombie -> Zombie
damageZombie = over health . subtract

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

makeZombie :: (Monad m) => ZombieType -> RandT StdGen m Zombie
makeZombie t = do
  h <- getRandomR (healthRange t)
  return Zombie { _zombieType = typeName t, _health = h, _power = powerRange t }

isZombieAlive :: Zombie -> Bool
isZombieAlive = (> 0) . view health

waveTypes :: [(ZombieType, Int)]
waveTypes = zip [basicZombieType, toughZombieType] $ repeat 3
