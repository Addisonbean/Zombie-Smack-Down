module Utility
  ( strEq
  ) where

strEq :: String -> String -> Bool
strEq "" "" = True
strEq [_] "" = False
strEq "" [_] = False
strEq (x:xs) (y:ys)
  | x == y = strEq xs ys
  | otherwise = False
