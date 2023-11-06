module Calculator (getResult) where

import Data.Char (isDigit)
import Stack (Stack (..), empty, isEmpty, pop, push)

data ExpElem
  = Operand String
  | Sign Char

instance Show ExpElem where
  show (Operand p) = show p
  show (Sign p) = show p

isOperand :: String -> Bool
isOperand str =
  fromEnum (length str) == 1 && fromEnum (head str) > 64 && fromEnum (head str) < 124 || all isDigit (tail str) && head str == '-' || all isDigit str

isOperator :: String -> Bool
isOperator = all (`elem` "=+-*/")

-- Convert infix expression to RPN
infixToRPN :: String -> [ExpElem]
infixToRPN expr = processTokens (words expr) empty []

-- Process token by token and get RPN notation output
processTokens :: [String] -> Stack String -> [ExpElem] -> [ExpElem]
processTokens [] stack output = output ++ pushOperators stack
processTokens (token : tokens) stack output
  | isOperand token = processTokens tokens stack (output ++ [Operand token])
  | token == "(" = processTokens tokens (push token stack) output
  | token == ")" = let (out, newStack) = handleClosingBracket stack [] in processTokens tokens newStack (output ++ out)
  | isOperator token = let (out, newStack) = handleOperator token stack [] in processTokens tokens newStack (output ++ out)
  | otherwise = output

handleClosingBracket :: Stack String -> [ExpElem] -> ([ExpElem], Stack String)
handleClosingBracket stack output
  | not (isEmpty stack) =
      let (stackTop, newStack) = pop stack
       in if stackTop == "("
            then (output, newStack)
            else handleClosingBracket newStack (output ++ [Sign (head stackTop)])
  | otherwise = error "Unbalanced parenthesis"

handleOperator :: String -> Stack String -> [ExpElem] -> ([ExpElem], Stack String)
handleOperator token stack output
  | not (isEmpty stack) =
      let (stackTop, newStack) = pop stack
       in if precendence stackTop >= precendence token
            then handleOperator token newStack (output ++ [Sign (head stackTop)])
            else (output, push token stack)
  | otherwise = (output, push token stack)

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