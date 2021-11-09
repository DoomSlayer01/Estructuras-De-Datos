module Stack1
    (Stack,emptyS,isEmptyS,push,top,pop,lenS)
where

stack = push 5 (push 4 (push 3(push 2 (push 1 emptyS))))

data Stack a = Stk [a] deriving (Show ,Eq)

emptyS :: Stack a
emptyS = Stk []

isEmptyS :: Stack a -> Bool
isEmptyS (Stk ls) = isEmptyS' ls

isEmptyS' [] = True
isEmptyS' xs = False

push :: a -> Stack a -> Stack a
push e (Stk ls) = Stk (e:ls)

-- push' :: a -> [a] -> [a]
-- push' e [] = [e]
-- push' e (x:xs) =  x : push' e xs

top :: Stack a -> a
top (Stk ls) = head ls

pop :: Stack a -> Stack a
pop (Stk ls) = Stk (tail ls)

-- pop' :: [a] -> [a]
-- pop' [] = []
-- pop' [x] = []
-- pop' (x:xs) = x : pop' xs

lenS :: Stack a -> Int
lenS (Stk ls) = lenght (ls)

lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1 + lenght xs
