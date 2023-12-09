module HaskellAssignment where

------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst [] = NoMatch
findFirst needle haystack index
  |needle haystack = Match index
  |otherwise = findFirst needle haystack (index + 1)

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
