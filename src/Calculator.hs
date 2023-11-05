module Calculator (getResult) where

import Data.Char (isDigit)
import Stack (Stack (..), empty, isEmpty, pop, push, top)

data ExpElem
  = Operand String
  | Sign Char

instance Show ExpElem where
  show (Operand p) = show p
  show (Sign p) = show p

isOperand :: String -> Bool
isOperand chr =
  fromEnum (head chr) > 64 && fromEnum (head chr) < 124 || isDigit (head chr)

isOperator :: String -> Bool
isOperator chr = head chr `elem` "=+-*/"

-- Convert infix expression to RPN
infixToRPN :: String -> [ExpElem]
infixToRPN expr = processTokens (words expr) empty []

-- Process token by token and get RPN notation output
processTokens :: [String] -> Stack String -> [ExpElem] -> [ExpElem]
processTokens [] stack output = output ++ pushOperators stack
processTokens (token : tokens) stack output
  | isOperand token = processTokens tokens stack (output ++ [Operand token])
  | token == "(" = processTokens tokens (push token stack) output
  | token == ")" = let (out, newStack) = handleClosingBracket stack in processTokens tokens newStack (output ++ out)
  | isOperator token = let (out, newStack) = handleOperator token stack in processTokens tokens newStack (output ++ out)
  | otherwise = error "Incorrect expression provided"

handleClosingBracket :: Stack String -> ([ExpElem], Stack String)
handleClosingBracket _ = ([], empty)

handleOperator :: String -> Stack String -> ([ExpElem], Stack String)
handleOperator token stack
  | not (isEmpty stack) =
      let (stackTop, newStack) = pop stack
       in if precendence stackTop <= precendence token
            then ([Sign (head stackTop)], newStack)
            else ([], push token stack)
  | isEmpty stack = ([], push token stack)
  | otherwise = ([], stack)

pushOperators :: Stack String -> [ExpElem]
pushOperators stack
  | not (isEmpty stack) = let (stackTop, newStack) = pop stack in Sign (head stackTop) : pushOperators newStack
  | otherwise = []

precendence :: String -> Int
precendence op
  | op `elem` ["*", "/"] = 2
  | op `elem` ["+", "-"] = 1
  | otherwise = 0

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
  let parsedInput = infixToRPN str
  if isEquation str
    then solveOperation parsedInput
    else solveEquation parsedInput