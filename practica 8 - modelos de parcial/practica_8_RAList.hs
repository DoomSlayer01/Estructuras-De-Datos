data RAList a = MkR Int (Map Int a) (Heap a)

-- Propósito:devuelve una lista vacía.
-- ! Eficiencia:O(1).
emptyRAL :: RAList a
emptyRAL = MKR 0 (emptyM) (emptyH)

-- Propósito:indica si la lista está vacía.
-- ! Eficiencia:O(1).
isEmptyRAL :: RAList a -> Bool
isEmptyRAl (MkR n mapIA heapA) = null emptyH

-- Propósito:devuelve la cantidad de elementos.
-- ! Eficiencia:O(1).
lengthRAL :: RAList a -> Int
lengthRAL (MkR n mapIA heapA) = n

-- Propósito:devuelve el elemento en el índice dado.
-- Precondición:el índice debe existir.
-- ! Eficiencia:O(logN).
get :: Int -> RAList a -> a
get n (MkR int mapIA heapA) = valorJust(lookupM n mapIA)

-- Propósito:devuelve el mínimo elemento de la lista.
-- Precondición:la lista no está vacía.
-- ! Eficiencia:O(1).
minRAL :: Ord a => RAList a -> a
minRAL (MkR Int mapIA heapA) = findMin heapA

-- Propósito:agrega un elemento al final de la lista.
-- ! Eficiencia:O(logN).
add :: Ord a => a -> RAList a -> RAList a
add e (MkR i mapIA heapA) = MKR (i+1) (assocM i e mapIA) (insertH e)

-- Propósito:transforma unaRAListen una lista, respetando el orden de los elementos.
-- ! Eficiencia:O(NlogN).
elems :: Ord a => RAList a -> [a]
elems (MkR i mapIA heapA)  = elems' heapA

elems' :: Heap a -> [a]
elems' heapA = if isEmptyH heapA 
                then []
                else findMinH heapA : (elems' deleteMinH heapA)

-- Precondición:la lista no está vacía.
-- Propósito:elimina el último elemento de la lista.
-- !Eficiencia:O(NlogN).
remove :: Ord a => RAList a -> RAList a
remove (MkR i mapIA heapA) = MkR (i-1) (deleteM (i-1) mapIA) (actualizarH (findMin heapA) heapA)

actualizarH :: a -> Heap a -> Heap a
actualizarH a heapA = if isEmptyH heapA then emptyH
                                        else if findMinH == a 
                                                then actualizarH a (deleteMinH heapA)
                                                else insertH findMin h (actualizarH a (deleteMinH heapA) )

-- Propósito:reemplaza el elemento en la posición dada.
-- Precondición:el índice debe existir.
-- ! Eficiencia:O(NlogN).
set :: Ord a => Int -> a -> RAList a -> RAList a
set i a (MkR i2 mapIA heapA) = MKR i2 (assocM i a mapIA ) (actualizarH (valorJust(lookUpMi i mapIA)) a heapA)

actualizarH ::Ord a -> a -> a -> Heap a -> Heap a
actualizarH oldA a heapA = if findMin heapA == oldA 
                                then insertH a(deleteMinH heapA)
                                else insertH (findMin heapA) (actualizarH oldA a (deleteMinH heapA))

-- Propósito:agrega un elemento en la posición dada.
-- Precondición:el índice debe estar entre 0 y la longitud de la lista.
-- Observación:cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
-- ! Eficiencia:O(NlogN).
-- ! Sugerencia:definir una subtarea que corra los elementos delMapen una posición a partir de una posición dada.
-- !Pasartambién como argumento la máxima posición posible.
addAt :: Ord a => Int -> a -> RAList a -> RAList a
