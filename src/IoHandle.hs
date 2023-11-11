module IoHandle (inputOutput) where

import Calculator

-- Manages input and output of program.
inputOutput :: IO ()
inputOutput = do
  putStrLn "Enter the action to take:"
  putStrLn "1. Operation"
  putStrLn "2. Linear equation"
  putStrLn "3. Quadratic equation"
  mode <- getLine
  case mode of
    "1" -> putStrLn "Enter the operation: "
    "2" -> putStrLn "Enter the linear equation constants: a b "
    "3" -> putStrLn "Enter the quadratic equation constants: a b c"
    _ -> error "Incorrect mode selected"
  str <- getLine
  print (getResult str mode)