module Command
  ( parseInput
  , Command(..)
  ) where

data Command
  = Punch
  | Status
  | Exit
  | EmptyCommand

parseInput :: String -> Maybe Command
parseInput = parseInput' . words
  where
    parseInput' ["punch"] = Just Punch
    parseInput' ["exit"] = Just Exit
    parseInput' ["status"] = Just Status
    parseInput' [] = Just EmptyCommand
    parseInput' _ = Nothing
