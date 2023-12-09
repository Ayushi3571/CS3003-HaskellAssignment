module HaskellAssignment where
------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst returnVal = alsoFindFirst returnVal 0 

alsoFindFirst :: Eq a => (a -> Bool) -> Int -> [a] -> Found
alsoFindFirst _ _ [] = NoMatch
alsoFindFirst needle index (x:xs)
  | needle x = Match index
  | otherwise = alsoFindFirst needle (index + 1) xs

------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome x = x == reverse x
