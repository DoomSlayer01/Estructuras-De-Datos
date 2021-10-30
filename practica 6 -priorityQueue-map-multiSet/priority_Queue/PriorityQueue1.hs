module PriorityQueue1
    (PriorityQueue,emptyPQ,isEmptyPQ,insertPQ,findMinPQ,deleteMinPQ)
where

data PriorityQueue a = Pq [a] deriving Show

emptyPQ :: PriorityQueue a
emptyPQ = (Pq [])

isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (Pq ls) = null ls

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ e (Pq ls) = Pq (e:ls)

--precondicion en caso de ser una pq vacia la funcion es parcial y tira error
findMinPQ :: Ord a => PriorityQueue a -> a
findMinPQ (Pq ls) = findMinPQ' ls

findMinPQ' :: Ord a => [a] -> a
findMinPQ' [] = error "no puedes pedirle el minimo a una priority queue vacia"
findMinPQ' xs = minimum xs

--precondicion en caso de ser una pq vacia la funcion es parcial y tira error
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMinPQ (Pq ls) = Pq (deleteMinPQ' ls (minimum ls))

deleteMinPQ' :: Ord a => [a] -> a -> [a]
deleteMinPQ' []     e = error " no puedes borrar el min elemento una lista vacia"
deleteMinPQ' (x:xs) e = if x == e then xs else x:deleteMinPQ' xs e

pq = Pq [1,1,2,3,4]



