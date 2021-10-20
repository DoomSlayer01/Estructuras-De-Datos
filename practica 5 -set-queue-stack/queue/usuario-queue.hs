import Queue2

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q then 0 else 1 +(lengthQ(dequeue q))

queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q then [] else (firstQ q):(queueToList (dequeue q))

unionQ :: Queue a -> Queue a -> Queue a
unionQ queue1 queue2 = if isEmptyQ queue2 then queue1 else unionQ (queue (firstQ queue2) queue1) (dequeue queue2)

