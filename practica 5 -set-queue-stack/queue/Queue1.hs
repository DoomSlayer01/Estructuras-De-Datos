module Queue1  --IMPORTANTE:los elementos entran por el final de la lista , y se sacan por el principio
        (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)
where 

data Queue a = Q [a] deriving (Show,Eq)

emptyQ :: Queue a
emptyQ = Q [] 

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ (Q  _) = False

queue :: a -> Queue a -> Queue a
queue e (Q xs) = Q (queue' e xs)

queue' :: a -> [a] -> [a]
queue' a [] = [a]
queue' a (x:xs) = x : (queue' a xs)

firstQ :: Queue a -> a
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (dequeue' xs)

dequeue' :: [a] -> [a]
dequeue' []     = []
dequeue' (x:xs) = xs
