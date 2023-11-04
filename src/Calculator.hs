module Calculator (getResult) where

import Data.Char (ord)

data ExpElem
  = Var Char
  | Sign Char
  | Num Int

instance Show ExpElem where
  show (Var p) = show p
  show (Sign p) = show p
  show (Num p) = show p

-- Split input into single elements and tell if its operation or equation
parseInput :: String -> ([ExpElem], Bool)
parseInput input = (splitOperation input, isEquation input)

splitOperation :: String -> [ExpElem]
splitOperation [] = []
splitOperation (x : xs)
  | fromEnum x > 64 && fromEnum x < 124 = Var x : splitOperation xs
  | x `elem` "0123456789" = Num (ord x - 48) : splitOperation xs
  | x `elem` "+-*/" = Sign x : splitOperation xs
  | x == ' ' = splitOperation xs
  | otherwise = error "Syntax error (Wrong expression provided)"

isEquation :: String -> Bool
isEquation =
  foldr
    (\x -> (||) (fromEnum x > 64 && fromEnum x < 124))
    False

-- Solve operation and retrun the result
solveOperation :: [ExpElem] -> [ExpElem]
solveOperation str = str

-- Solve equation (linear or quadratic) and return the result
solveEquation :: [ExpElem] -> [ExpElem]
solveEquation str = str

-- Return the calculator result
getResult :: String -> [ExpElem]
getResult str = do
  let (parsedInput, isOperation) = parseInput str
  if isOperation
    then solveOperation parsedInput
    else solveEquation parsedInput