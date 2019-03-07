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
  , zombieDoAttack
  , xpGiven
  ) where

import Control.Lens (over, view, makeLenses)
import Control.Monad.Trans.Random (RandT)
import Control.Monad.Random (getRandomR)
import System.Random (StdGen)

data Zombie = Zombie
  { _zombieType :: String
  , _health :: Int
  , _power :: (Int, Int)
  , _xpGiven :: Int
  } deriving (Show)
makeLenses ''Zombie

blankZombie :: Zombie
blankZombie = Zombie
  { _zombieType = ""
  , _health = 0
  , _power = (0, 0)
  }

data ZombieType = ZombieType
  { typeName :: String
  , healthRange :: (Int, Int)
  , powerRange :: (Int, Int)
  , xpEarned :: Int
  } deriving (Show)

basicZombieType :: ZombieType
basicZombieType = ZombieType
  { typeName = "basic"
  , healthRange = (5, 8)
  , powerRange = (3, 4)
  , xpEarned = 3
  }

toughZombieType :: ZombieType
toughZombieType = ZombieType
  { typeName = "tough"
  , healthRange = (7, 10)
  , powerRange = (4, 6)
  , xpEarned = 5
  }

makeZombie :: (Monad m) => ZombieType -> RandT StdGen m Zombie
makeZombie t = do
  h <- getRandomR (healthRange t)
  return Zombie { _zombieType = typeName t, _health = h, _power = powerRange t, _xpGiven = xpEarned t }

waveTypes :: [(ZombieType, Int)]
waveTypes = zip [basicZombieType, toughZombieType] $ repeat 3

isZombieAlive :: Zombie -> Bool
isZombieAlive = (> 0) . view health

damageZombie :: Int -> Zombie -> Zombie
damageZombie = over health . subtract

zombieDoAttack :: (Monad m) => Zombie -> RandT StdGen m Int
zombieDoAttack = getRandomR . view power
