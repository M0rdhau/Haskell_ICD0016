-- Following is a lambda notation implementation in haskell, which I took from the internet
-- alternatively a function can be implemented with the same signature but without the 
-- infixes. Although the code will become less elegant
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

isPalindrome :: String -> Bool
isPalindrome probable
  | length probable <= 1 = True
  | last probable /= head probable = False 
  | otherwise = isPalindrome(tail (init probable))

longestPalindrome :: [String] -> String 
longestPalindrome [] = []
longestPalindrome [x] = x
longestPalindrome (x:xs) = foldl (\z x -> length z > length x ? z :? x) "" xs

calculateBill :: Double -> Double 
calculateBill start
  | start > 250 = (start - 250) * 1.5 * 1.2 + calculateBill 250.0
  | start > 150 = (start - 150) * 1.2 * 1.2 + calculateBill 150
  | start > 50 = (start - 50) * 0.75 * 1.2 + calculateBill 50
  | otherwise = start * 0.5 * 1.2

main :: IO()
main = do
  print(longestPalindrome["aba", "abba", "labba", "ogo", "goo", "librbil"])
  print(calculateBill 25)
  print(calculateBill 55)
  print(calculateBill 105)
  print(calculateBill 155)
  print(calculateBill 255)
  print(calculateBill 2000)
