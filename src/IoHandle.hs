module IoHandle (inputOutput) where

import Calculator

-- Manages input and output of program.
inputOutput :: IO ()
inputOutput = do
  putStrLn "Enter the operation or equation:"
  str <- getLine
  print (getResult str)
  putStrLn ""