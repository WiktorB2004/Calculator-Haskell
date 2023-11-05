module Stack (Stack (..), isEmpty, empty, push, pop, top) where

-- Stack data structure
newtype Stack a = Stack [a]

-- Function to check if the stack is empty
isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _ = False

-- Function to push an element onto the stack
push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x : xs)

-- Function to return empty stack
empty :: Stack a
empty = Stack []

-- Function to pop an element from the stack
pop :: Stack a -> (a, Stack a)
pop (Stack (x : xs)) = (x, Stack xs)
pop _ = error "Empty stack"

-- Function to get the top element of the stack
top :: Stack a -> a
top (Stack (x : _)) = x
top _ = error "Empty stack"