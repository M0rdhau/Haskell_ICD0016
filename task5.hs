takeChars :: IO(Char, Char, Char, Char)
takeChars = do
  a <- getChar
  getChar
  b <- getChar
  getChar
  c <- getChar
  getChar
  d <- getChar
  return (a, b, c, d)

displayChars::IO(Char, Char, Char, Char) -> IO String
displayChars xs = do
  (a, b, c, d) <- xs
  return [a, b, c, d]

ltos :: [String] -> String
ltos []    = ""
ltos (m:n) = foldl (\a b -> a ++ "\n" ++ b) m n

prep::(String, String)->IO()
prep (file, otherFile)= do
  -- preparing files
  writeFile file ("I am just experimenting here "++"\n")
  appendFile file (" And some more. " ++ "\n")
  appendFile file (" And some more. " ++ "\n")
  appendFile file (" And some more. " ++ "\n")
  appendFile file (" And some more. " ++ "\n")
  appendFile file (" And some more. " ++ "\n")
  writeFile otherFile ("I am just experimenting here "++"\n")
  appendFile otherFile " And some more. "
  appendFile otherFile " And some more. "

replaceLineFour::String->IO()
replaceLineFour file = do
  print("Trying to replace line four in file " ++ file)
  fileContents <- readFile file
  let fileLines = lines fileContents
  if length fileLines > 3
    then do
      print "more than 3 lines found. Enter characters line by line"
      newFourthLine <- displayChars takeChars
      let newLines = take 3 fileLines ++ [newFourthLine] ++ drop 4 fileLines
      let newFileContent = ltos newLines
      writeFile file newFileContent
      fileContentsAgain <- readFile file
      putStr fileContentsAgain
      putStr "\n"
    else do
      print "The file is too short"

main::IO()
main = do
  let file = "abc.txt"
  let otherFile = "abgd.txt"
  prep(file, otherFile)
  replaceLineFour file
  replaceLineFour otherFile
  
