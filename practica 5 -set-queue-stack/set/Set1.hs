module Set1
    (Set,emptyS,addS,belongS,sizeS,removeS,unionS,setToList)

where
-- dato importante se podria todo mas con costo de eficiencia constante pero para eso se tiene que cobrar un costo de memoria
-- asi que la manera de modelar una estructura depende de lo que quieras hacer 

data Set a = St [a] Int deriving (Show,Eq)
-- invariantes :
-- *si la lista esta vacia el contador de elementos es 0 (se aplica de modo inverso)
-- *el contador de elementos no tiene que ser negativo
-- *la lista no puede tener repetidos

--Preguntas  -- 

--  cuando borro un elemento tengo que eliminar el x juntos a sus apariciones -> tendria que agregarlo a la invariante?

-- en unionS no pasa nada con los elementos repetidos de la union?

emptyS :: Set a
emptyS = St [] 0

addS :: Eq a => a -> Set a -> Set a
addS  a (St xs n) = if elem a xs then (St xs n) else (St (a:xs) (n+1))

belongS :: Eq a => a -> Set a -> Bool
belongS a (St xs n) = elem a xs

sizeS :: Eq a => Set a -> Int
sizeS (St xs n) = n

removeS :: Eq a => a -> Set a -> Set a
removeS a (St xs n ) = St (removeS' a xs) (n-1)

removeS':: Eq a => a -> [a] -> [a]
removeS' a [] = []
removeS' a (x:xs) = if a == x then xs else x : removeS' a xs 

unionS :: Eq a => Set a -> Set a -> Set a
unionS (St xs n) st2 = unionS' xs st2

unionS'::Eq a => [a] -> Set a -> Set a
unionS' [] set2 = set2
unionS' (x:xs) set2  = addS x (unionS' xs set2)

setToList :: Eq a => Set a -> [a]
setToList (St xs n) = xs


