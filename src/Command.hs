module Command
  ( parseInput
  , Command(..)
  ) where

data Command
  = Punch
  | Kick
  | Status
  | Exit
  | EmptyCommand

parseInput :: String -> Maybe Command
parseInput = parseInput' . words
  where
    parseInput' ["punch"] = Just Punch
    parseInput' ["p"] = Just Punch

    parseInput' ["kick"] = Just Kick
    parseInput' ["k"] = Just Kick

    parseInput' ["exit"] = Just Exit
    parseInput' ["quit"] = Just Exit

    parseInput' ["status"] = Just Status
    parseInput' ["s"] = Just Status

    parseInput' [] = Just EmptyCommand

    parseInput' _ = Nothing
