module Scrapegis.Utils
    ( chunkArray
    ) where

chunkArray :: Int -> [a] -> [[a]]
chunkArray _ [] = []
chunkArray n l
  | n > 0 = (take n l) : (chunkArray n (drop n l))
  | otherwise = error "Negative n"

