{-# LANGUAGE TemplateHaskell #-}

module Zombie
  ( Zombie
  , ZombieType(..)
  , damageZombie
  , genWaves
  , zombieType
  , health
  , power
  , isZombieAlive
  ) where

import Control.Lens
import Control.Monad.State
import System.Random

data Zombie = Zombie
  { _zombieType :: String
  , _health :: Int
  , _power :: (Int, Int)
  } deriving (Show)
makeLenses ''Zombie

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

makeZombie :: ZombieType -> State StdGen Zombie
makeZombie t = do
  h <- state (randomR (healthRange t))
  return Zombie { _zombieType = typeName t, _health = h, _power = powerRange t }

isZombieAlive :: Zombie -> Bool
isZombieAlive = (> 0) . view health

waveTypes :: [ZombieType]
waveTypes = [basicZombieType, toughZombieType]

-- fold? StateT? ListT?
genWaves :: State StdGen [[Zombie]]
genWaves = genWaves' waveTypes
  where
    genWaves' [] = return []
    genWaves' (t:ts) = do
      z1 <- makeZombie t
      z2 <- makeZombie t
      z3 <- makeZombie t
      zs <- genWaves' ts
      return ([z1, z2, z3]:zs)
