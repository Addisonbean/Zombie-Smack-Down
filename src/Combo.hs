module Combo
  ( Combo
  , comboCost
  , comboDamageRange
  , isAvaliable
  , kickKickPunch
  -- , punchPunchKick
  -- , kickUppercut
  -- , fistKneeFistKneeBodySlam
  ) where

data Combo = Combo
  { comboCost :: Int
  , comboDamageRange :: (Int, Int)
  , unlockedAtWave :: Int
  } deriving (Show)

kickKickPunch :: Combo
kickKickPunch = Combo { comboCost = 3, comboDamageRange = (6, 11), unlockedAtWave = 1}

isAvaliable :: Combo -> Int -> Bool
isAvaliable c wave = wave >= unlockedAtWave c
