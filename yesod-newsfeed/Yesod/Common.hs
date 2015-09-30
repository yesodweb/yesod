module Yesod.Common
  ( removeItem
  ) where

removeItem :: Eq a => a -> [a] -> [a]
removeItem _ [] = []
removeItem r (x:xs)
  | r == x = removeItem r xs
  | otherwise = x : removeItem r xs
