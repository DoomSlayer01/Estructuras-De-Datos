module Set2 --IMPORTANTE: este acepta repetidos
    (Set,emptyS,addS,belongS,sizeS,removeS,unionS,setToList)

where
-- dato importante se podria todo mas con costo de eficiencia constante pero para eso se tiene que cobrar un costo de memoria
-- asi que la manera de modelar una estructura depende de lo que quieras hacer 

data Set a = St [a] deriving (Show,Eq)
-- invariantes :
-- *si la lista esta vacia el contador de elementos es 0 (se aplica de modo inverso)
-- *el contador de elementos no tiene que ser negativo*


emptyS :: Set a
emptyS = St [] 

addS :: Eq a => a -> Set a -> Set a
addS  a (St xs) = St (a:xs)

belongS :: Eq a => a -> Set a -> Bool
belongS a (St xs) = elem a xs

sizeS :: Eq a => Set a -> Int
sizeS (St xs) =  sizeS' xs

sizeS'::Eq a => [a] -> Int
sizeS'  [] = 0
sizeS' (x:xs) =if elem x xs then sizeS' xs else 1+sizeS' xs

removeS :: Eq a => a -> Set a -> Set a
removeS a (St xs) = St (removeS' a xs)

removeS':: Eq a => a -> [a] -> [a]
removeS' a [] = []
removeS' a (x:xs) = if a == x then removeS' a xs  else x : removeS' a xs 

unionS :: Eq a => Set a -> Set a -> Set a
unionS (St xs) set2 = unionS' xs set2

unionS'::Eq a => [a] -> Set a -> Set a
unionS' [] set2     = set2
unionS' (x:xs) set2 = addS x (unionS' xs set2)

setToList :: Eq a => Set a -> [a]
setToList (St xs) = setToList' xs

setToList' :: Eq a => [a] -> [a]
setToList' [] = []
setToList' (x:xs) = if elem x xs then setToList' xs else x: setToList' xs


