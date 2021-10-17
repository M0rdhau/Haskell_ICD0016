import Data.Attoparsec.Text.Lazy (number)
findNumber :: Int -> String
findNumber x = "number"


findOdd::Int->String
findOdd x =
   if even x
     then "Number is Even"
     else "Number is Odd"

roots::(Float, Float, Float)->(Float, Float)
roots (a,b,c) = (x1, x2) where
  x1 = e + sqrt d / (2 * a)
  x2 = e - sqrt d / (2 * a)
  d = b*b-4*a*c
  e = -b/(2*a)

findCircle::Float->(Float, Float)
findCircle r = (area, circumference) where
  area = r*r*pi
  circumference = 2*r*pi

euclideanDistance::(Float, Float, Float, Float) -> Float
euclideanDistance (x1, x2, y1, y2) = sqrt ((x1-x2)^2 + (y1 - y2)^2)

isLeap::Int -> Bool
isLeap year
  | year `mod` 400 == 0 = True
  | year `mod` 4 == 0 && (year `mod` 100 > 0) = False
  | year `mod` 4 == 0 = True
  | otherwise = False



main :: IO()
main = do
  putStrLn "Input number to calculate if it's odd"
  number <- getLine
  putStrLn(findOdd (read number :: Int))
  putStrLn "Input radius to calculate circumference and area"
  radius <- getLine
  print(findCircle (read radius :: Float))
  putStrLn "Input x1"
  x1 <- getLine
  putStrLn "Input x2"
  x2 <- getLine
  putStrLn "Input y1"
  y1 <- getLine
  putStrLn "Input y2"
  y2 <- getLine
  putStrLn "Distance:"
  print(euclideanDistance (read x1 :: Float,read x2 :: Float,read y1 :: Float,read y2 :: Float))
  putStrLn "Input year to find out if it's leap or not"
  year <- getLine 
  print(isLeap (read year ::Int))