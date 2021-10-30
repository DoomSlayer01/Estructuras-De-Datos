import PriorityQueue1

--punto 2

-- en el enunciado pone heapSort pero el prfe dijo que era pqSort era un error ya que
-- una pq implementa con una Heap

pqSort :: Ord a => [a] -> [a]
pqSort (xs) = toListPQ (addToPQ xs emptyPQ)

addToPQ :: Ord a => [a] -> PriorityQueue a -> PriorityQueue a
addToPQ [] pq =  emptyPQ
addToPQ (x:xs) pq = insertPQ x (addToPQ xs pq)

toListPQ :: Ord a => PriorityQueue a -> [a]
toListPQ pq = if isEmptyPQ pq then [] else findMinPQ pq : (toListPQ (deleteMinPQ pq))

