main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (largers (lines contents) "0")
 
largers :: [String] -> String -> Int -- pass in the last int
largers [] _ = 0
largers (x:xs) n
  | x > n = 1 + largers xs x
  | otherwise = largers xs x

