import Set2

data Tree a = NodeT a (Tree a) (Tree a) | EmptyT  deriving Show

losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     set    = []
losQuePertenecen (x:xs) emptyS = []
losQuePertenecen (x:xs) set    = if belongS x set   then x : losQuePertenecen xs set
                                                    else losQuePertenecen xs set

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (xs) =  setToList (addS'' xs emptyS)

addS'' ::Eq a => [a] -> Set a -> Set a
addS'' [] set = set
addS'' (x:xs) set  = addS x (addS'' xs set)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT  = emptyS
unirTodos (NodeT x izq der) = unionS x (unionS (unirTodos izq)  (unirTodos der))

