module Set
    (Set,emptyS,addS,belongS,sizeS,removeS,unionS,setToList)

where
-- dato importante se podria todo mas con costo de eficiencia constante pero para eso se tiene que cobrar un costo de memoria
-- asi que la manera de modelar una estructura depende de lo que quieras hacer 

data Set = St [] Int
-- invariantes :
-- *si la lista esta vacia el contador de elementos es 0 (se aplica de modo inverso)
-- *el contador de elementos no tiene que ser negativo
-- *la lista no puede tener repetidos

emptyS :: Set a
emptyS = ST [] 0

addS :: Eq a => a -> Set a -> Set a
addS  a (St xs n) = if elem a xs then (St xs n) else (St (a:xs) n+1)

belongs :: Eq a => a -> Set a -> Bool
belongS a (St xs n) = elem a xs

sizeS :: Eq a => Set a -> Int
sizeS (St xs n) = n

removeS :: Eq a => a -> Set a -> Set a
removeS a (St xs n ) = St (removeS' a xs) (n-1)

removeS':: Eq a => a -> [a] -> [a]
removeS' a [] = error' no se puede borrar u elemento de una lista vacia'
removeS' a (x:xs) = if a == x then xs else x : removeS' a xs 

unionS :: Eq a => Set a -> Set a -> Set a
unionS (St xs n) st2 = unionS' xs st2

unionS'::Eq a => [a] -> Set a -> Set a
unionS' [] set1 = set1
unionS' (x:xs) set1  = addS x (unionS' xs set1)

setToList :: Eq a => Set a -> [a]
setToList (St xs n) = xs


