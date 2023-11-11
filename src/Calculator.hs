{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Calculator (getResult) where

import Data.Fixed (mod')
import Stack (Stack (..), empty, isEmpty, pop, push)
import Text.Read

data ExpElem
  = Operand String
  | Sign Char
  deriving (Show)

instance Eq ExpElem where
  (Sign chr1) == (Sign chr2) = chr1 == chr2
  (Operand chr1) == (Operand chr2) = chr1 == chr2
  (Sign _) == (Operand _) = False
  (Operand _) == (Sign _) = False

instance Num ExpElem where
  (+) :: ExpElem -> ExpElem -> ExpElem
  (Operand x) + (Operand y) = Operand (show $ (read x :: Int) + (read y :: Int))
  (-) :: ExpElem -> ExpElem -> ExpElem
  (Operand x) - (Operand y) = Operand (show $ (read x :: Int) - (read y :: Int))
  (*) :: ExpElem -> ExpElem -> ExpElem
  (Operand x) * (Operand y) = Operand (show $ (read x :: Int) * (read y :: Int))
  abs :: ExpElem -> ExpElem
  abs (Operand x) = Operand (show $ abs (read x :: Int))
  signum :: ExpElem -> ExpElem
  signum (Operand _) = Operand "1"
  fromInteger :: Integer -> ExpElem
  fromInteger n = Operand (show n)

instance Fractional ExpElem where
  (/) :: ExpElem -> ExpElem -> ExpElem
  (Operand x) / (Operand y) = Operand (show $ (read x :: Integer) `div` (read y :: Integer))
  recip :: ExpElem -> ExpElem
  recip = recip
  fromRational :: Rational -> ExpElem
  fromRational = fromRational

-- SECTION: PARSING THE USER INPUT

-- Check if passed string is number (also negative) or variable
isOperand :: String -> Bool
isOperand str = case readMaybe str :: Maybe Integer of
  Just _ -> True
  _ -> case readMaybe str :: Maybe Float of
    Just _ -> True
    _ -> False

-- Check if whole string contains operators
isOperator :: String -> Bool
isOperator = all (`elem` "+-*/^%")

-- Convert infix expression to RPN
infixToRPN :: String -> [ExpElem]
infixToRPN expr = processTokens (words expr) empty []

-- Process token by token and get RPN notation output
processTokens :: [String] -> Stack String -> [ExpElem] -> [ExpElem]
processTokens [] stack output = output ++ pushLeftOperators stack
processTokens (token : tokens) stack output
  | isOperand token = processTokens tokens stack (output ++ [Operand token])
  | token == "(" = processTokens tokens (push token stack) output
  | token == ")" = let (out, newStack) = handleClosingBracket stack [] in processTokens tokens newStack (output ++ out)
  | isOperator token = let (out, newStack) = parseOperator token stack [] in processTokens tokens newStack (output ++ out)
  | otherwise = output

-- Pushes all elements that are inside parentheses back to output list
handleClosingBracket :: Stack String -> [ExpElem] -> ([ExpElem], Stack String)
handleClosingBracket stack output
  | not (isEmpty stack) =
      let (stackTop, newStack) = pop stack
       in if stackTop == "("
            then (output, newStack)
            else handleClosingBracket newStack (output ++ [Sign (head stackTop)])
  | otherwise = error "Unbalanced parenthesis"

-- Checks the operation precendece and push operators in the correct order
parseOperator :: String -> Stack String -> [ExpElem] -> ([ExpElem], Stack String)
parseOperator token stack output
  | not (isEmpty stack) =
      let (stackTop, newStack) = pop stack
       in if precendence stackTop >= precendence token
            then parseOperator token newStack (output ++ [Sign (head stackTop)])
            else (output, push token stack)
  | otherwise = (output, push token stack)

-- Handles pushing operators left in stack after full traverse
pushLeftOperators :: Stack String -> [ExpElem]
pushLeftOperators stack
  | not (isEmpty stack) = let (stackTop, newStack) = pop stack in Sign (head stackTop) : pushLeftOperators newStack
  | otherwise = []

-- Returns operation precendence
precendence :: String -> Int
precendence op
  | op == "^" = 3
  | op `elem` ["*", "/"] = 2
  | op `elem` ["+", "-"] = 1
  | otherwise = 0

-- SECTION: HANDLING OPERATIONS

-- Return value of RPN expression
solveRPN :: [ExpElem] -> Float
solveRPN = evaluateRPN

-- Evaluate RPN expression
evaluateRPN :: [ExpElem] -> Float
evaluateRPN = head . foldl foldingFunction []
  where
    foldingFunction (val1 : val2 : rest) (Sign '+') = (val1 + val2) : rest
    foldingFunction (val1 : val2 : rest) (Sign '-') = (val2 - val1) : rest
    foldingFunction (val1 : val2 : rest) (Sign '*') = (val1 * val2) : rest
    foldingFunction (val1 : val2 : rest) (Sign '/') = (val2 / val1) : rest
    foldingFunction (val1 : val2 : rest) (Sign '^') = (val2 ** val1) : rest
    foldingFunction (val1 : val2 : rest) (Sign '%') = mod' val2 val1 : rest
    foldingFunction xs (Operand numberString) = read numberString : xs
    foldingFunction xs _ = xs

-- SECTION: HANDLING OUTPUT RETURN

-- Solve operation and retrun the result
solveOperation :: [ExpElem] -> String
solveOperation operation = show (solveRPN operation)

solveLinear :: String -> String -> String
solveLinear aStr bStr =
  let a = read aStr :: Double
      b = read bStr :: Double
   in if a == 0
        then
          if b == 0
            then "Infinite solutions - Any value of x satisfies the equation"
            else "No solution - The equation is inconsistent"
        else "Solution to " ++ show a ++ "x + " ++ show b ++ " = 0 : x = " ++ show (-b / a)

solveQuadratic :: String -> String -> String -> String
solveQuadratic aStr bStr cStr =
  let a = read aStr :: Double
      b = read bStr :: Double
      c = read cStr :: Double

      delta = b ** 2 - 4 * a * c
      sqrtDelta = sqrt delta
      denominator = 2 * a

      root1 = (-b + sqrtDelta) / denominator
      root2 = (-b - sqrtDelta) / denominator
   in if delta < 0
        then "No real roots - Discriminant is negative"
        else
          "Roots of the equation "
            ++ aStr
            ++ "x^2 + "
            ++ bStr
            ++ "x + "
            ++ cStr
            ++ " = 0 are: "
            ++ "x = "
            ++ show root1
            ++ " and x = "
            ++ show root2

-- Return the calculator result
getResult :: String -> String -> String
getResult str "1" = let parsedInput = infixToRPN str in solveOperation parsedInput
getResult str "2" = let (a : b : _) = words str in solveLinear a b
getResult str "3" = let (a : b : c : _) = words str in solveQuadratic a b c