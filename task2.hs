lucky :: Int -> String
lucky 15 = "Llll"
lucky 13 = "ulll"
lucky x = "well"

addThings :: Double->Double->Double->Double
addThings x y z = x+y+z

factorial 0 = 1
factorial n = n*factorial(n-1)

first :: (a, b, c) -> a
first(x,y,z) = x

grader :: Int -> String
grader grade
  | grade >= 80 && grade <= 90 = "A+"
  | grade >= 70 && grade < 80 = "A"
  | grade >= 60 && grade < 70 = "B"
  | grade >= 50 && grade < 60 = "C"
  | grade < 50 = "Fail"
  | otherwise = "wrong grade!"

lineMiddler :: (Double, Double) -> (Double, Double) -> (Double, Double)
lineMiddler (x1, y1) (x2, y2) = ((x1+x2)/2, (y1+y2)/2)

fibonacci :: Int -> [Int]
fibonacci 0 = [0, 1]
fibonacci x = reverse( last(fibonacci (x-1)) + last (init (fibonacci (x-1))) : reverse (fibonacci (x-1)))



listCleanup :: [Int] -> [Int]
listCleanup [] = []
listCleanup [x] = [x]
listCleanup (x:y:xs)  | x == y = listCleanup (x : xs)
                      | otherwise = x : listCleanup(y:xs)



main :: IO()
main = do
    print(factorial 10)
    putStrLn(grader 100)
    putStrLn(grader 81)
    putStrLn(grader 71)
    putStrLn(grader 10)
    print(lineMiddler(5,10)(10,15))
    print(fibonacci 5)
    print(listCleanup[1,1,3,3,2,4,4,4,4,2,4,3,3,3])