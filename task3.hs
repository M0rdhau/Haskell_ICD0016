import GHC.Show (Show)
import Data.Map (Map)
import qualified Data.Map as Map


data MyBit = One | Zero deriving(Eq, Show)

orGate :: (MyBit,MyBit) -> MyBit
orGate (a, b)
  | a == Zero && b == Zero = Zero
  | otherwise = One

data Book = Book{
  bookName :: String,
  authorName :: String,
  isbn :: String,
  yearPublished :: Int,
  versionNumber :: Int
} deriving (Show)

book1 = Book "The Wizard of Earthsea" "Ursula K. Le Guin" "0547773749" 1968 10

filterEven:: [Int] -> [Int]
filterEven = filter (\n -> n `mod` 2 == 1)

myMap :: Integer -> Map Integer [Integer]
myMap n = Map.fromList (map makePair [1..n])
  where makePair x = (x, [x*4])

main :: IO()
main = do
  print(orGate(One, Zero))
  print(orGate(One, One))
  print(orGate(Zero, One))
  print(orGate(Zero, Zero))
  print book1
  print(filterEven[1,2,3,4,5,6,7,8,9,10])
  print(myMap 4)