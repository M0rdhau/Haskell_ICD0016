-- 1. A triangle is given with its base and height. Write a program 
-- that finds the areas of the list of triangles and finds the triangle 
-- with maximum area, using lambdas and/or higher order 
-- functions.

maximumArea::[(Double, Double)] -> Double
maximumArea [] = 0
maximumArea [(x, y)] = (1/2)*x*y
-- I used termary in assignment 1 and it turns out we already have it in haskell :)
maximumArea xs = foldl max 0 (map(\(x,y) -> (1/2)*x*y)xs)

-- 2. Write a program that takes a list of numbers as an argument, 
-- finds all the squares of these numbers, and finds the sum of 
-- squares that are smaller than 100 using  FOLD.

findSumOfSqrsLt100::[Integer] -> Integer
findSumOfSqrsLt100 [] = 0
findSumOfSqrsLt100 xs = foldl(\x y -> if y < 10 then x + y^2 else x) 0 xs

-- 3. Write a program that takes a list of the years as an argument 
-- and finds the first year in the list which is a leap year using 
-- lambdas and/or higher order functions.  

-- from task 1
isLeap::Int -> Bool
isLeap year
  | year `mod` 400 == 0 = True
  | year `mod` 100 == 0 && (year `mod` 400 > 0) = False
  | year `mod` 4 == 0 = True
  | otherwise = False

firstLeapYear::[Int]->Int
firstLeapYear = head . filter isLeap

main :: IO()
main = do
  print(maximumArea[(2,4), (3,5), (4, 8), (1, 8)])
  print(findSumOfSqrsLt100[2, 4, 5, 6, 7, 10, 23])
  print(firstLeapYear[1023, 1200, 1400, 1600, 2020])