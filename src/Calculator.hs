module Calculator (getResult) where

-- Split input into single elements and tell if its operation or equation
parseInput :: String -> ([String], Bool)
parseInput str = ([str], True)

-- Solve operation and retrun the result
solveOperation :: [String] -> Int
solveOperation str = 0

-- Solve equation (linear or quadratic) and return the result
solveEquation :: [String] -> Int
solveEquation str = 0

-- Return the calculator result
getResult :: String -> Int
getResult str = do
  let (parsedInput, isOperation) = parseInput str
  if isOperation
    then solveOperation parsedInput
    else solveEquation parsedInput