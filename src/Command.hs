module Command
  ( parseInput
  , Command(..)
  ) where

import Text.Read (readMaybe)

import Combo (Combo, kickKickPunch)

data Command
  = Punch
  | Kick
  | Status
  | Heal Int
  | ComboAttack Combo
  | Exit
  | EmptyCommand

parseInput :: String -> Maybe Command
parseInput = parseInput' . words
  where
    parseInput' ["punch"] = Just Punch
    parseInput' ["p"] = Just Punch

    parseInput' ["kick"] = Just Kick
    parseInput' ["k"] = Just Kick

    parseInput' ["heal", x] = Just . Heal =<< readMaybe x

    parseInput' ["exit"] = Just Exit
    parseInput' ["quit"] = Just Exit

    parseInput' ["status"] = Just Status
    parseInput' ["s"] = Just Status

    -- COMBOS

    parseInput' ["kick", "kick", "punch"] = Just $ ComboAttack kickKickPunch

    -- END COMBOS

    parseInput' [] = Just EmptyCommand

    parseInput' _ = Nothing
