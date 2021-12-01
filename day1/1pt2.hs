main :: IO ()
main = do
  contents <- readFile "input.txt"
  print (largers' (map toInt (lines contents)) 0)
 
largers' :: [Int] -> Int -> Int -- pass in the last int
largers' [x, xs] _ = -1 -- if input array has only two items (subtracts one as it always compares A+A+A to 0 at the start)
largers' (x:(x':(x'':xs))) n
  | (x + x' + x'') > n = 1 + largers' (x':x'':xs) (x + x' + x'') 
  | otherwise = largers' (x':x'':xs) (x + x' + x'')
largers' _ _ = 0

toInt :: String -> Int 
toInt str = read str :: Int

