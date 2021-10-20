module PriorityQueue
    (PriorityQueue,emptyPQ,isEmptyPQ,insertPQ,findMinPQ,deleteMinPQ)
where

data PriorityQueue = PQ [a]

emptyPQ :: PriorityQueue a
emptyPQ (PQ) 
isEmptyPQ :: PriorityQueue a -> Bool


insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a

findMinPQ :: Ord a => PriorityQueue a -> a

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a