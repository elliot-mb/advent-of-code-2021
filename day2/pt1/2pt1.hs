import Text.Read (readMaybe)
main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (pairProduct (position (splitNL [contents] []) 0 0))
 
--turns a string into an array of strings split on \n
--char array argument is the buffer that fills as the text is read, then gets reset on each newline
splitNL :: [[Char]] -> [Char] -> [[Char]]
splitNL [] _ = []
splitNL ([]:xs) y = xs ++ [y] --once first block (file text) is empty
splitNL ((x:xs):xss) y
  | x == '\n' = splitNL ((xs:xss) ++ [y]) [] 
  | otherwise = splitNL (xs:xss) (y ++ [x]) 

--takes array of instructions and positions and outputs position pair
position :: [[Char]] -> Int -> Int -> (Int, Int)
position [] n m = (n, m) --once all instructions have been consumed
position ([]:xss) _ _ = (0, 0)
position ((x:xs):xss) n m 
  | x == 'f'  = position xss (n + getLastNum xs) m
  | x == 'd'  = position xss n (m + getLastNum xs) 
  | x == 'u'  = position xss n (m - getLastNum xs) 
  | otherwise = position xss n m

getLastNum :: [Char] -> Int
getLastNum x = read [last x] :: Int

pairProduct :: (Int, Int) -> Int 
pairProduct (n, m) = n * m

-- getForward :: [String] -> Int 
-- getForward [] = 0
-- getForward ((x:xs):xss)
--   | x == 'f' = (readMaybe ("\"" ++ [last (init xs)] ++ "\"") :: Int) + getForward xss
--   | otherwise = getForward xss
-- getForward _ = 0

-- getDepth :: [String] -> Int
-- getDepth [] = 0
-- getDepth ((x:xs):xss)
--   | x == 'u' = - (read [last (init xs)] :: Int) + getDepth xss
--   | x == 'd' = (read [last (init xs)] :: Int) + getDepth xss
--   | otherwise = getForward xss
-- getDepth _ = 0

-- juster :: Maybe a -> a
-- juster (Just a) = a
-- juster Nothing = 
