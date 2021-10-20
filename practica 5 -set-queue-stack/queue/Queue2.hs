module Queue2 -- IMPORTANTE :: agrega los elementos por adelante y los quita por el final
        (Queue,emptyQ,isEmptyQ,queue,firstQ,dequeue)
where

data Queue a = Q [a] deriving (Show,Eq)

emptyQ :: Queue a
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
isEmptyQ (Q []) = True
isEmptyQ (Q _ ) = False

queue :: a -> Queue a -> Queue a
queue a (Q xs) = Q (queue' a xs) 

queue' :: a -> [a] -> [a]
queue' a [] = [a]
queue' a xs = a:xs

firstQ :: Queue a -> a
firstQ (Q xs) = last xs

dequeue :: Queue a -> Queue a
dequeue (Q xs) = Q (init xs)
