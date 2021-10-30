import Map1 -- ! este no acepta repetidos

data MultiSet a = MST (Map a Int) deriving Show

multiset1 = addMS "a" $
            addMS "b" $
            addMS "c" $
            addMS "d" $
            addMS "d" $
            addMS "d" $
            addMS "a" $
            addMS "a" $
            addMS "a" $
            addMS "c" $
            addMS "a" $
            addMS "e" $
            emptyMS

multiset2 = addMS "f" $
            addMS "g" $
            addMS "h" $
            addMS "h" $
            addMS "h" $
            addMS "i" $
            addMS "e" $
            addMS "j" $
            addMS "j" $
            addMS "j" $
            addMS "a" $
            addMS "a" $
            addMS "b" $
            emptyMS

--Propósito: denota un multiconjunto vacío.
emptyMS :: MultiSet a
emptyMS = MST (emptyM)
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS e (MST map) = MST (addMS' e map)

addMS' :: Ord a => a -> Map a Int -> Map a Int
addMS' e map = case lookupM e map of
                    Just v -> assocM e (v+1) map
                    Nothing -> assocM e 1 map

--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS a (MST map) = ocurrencesMS' a map

ocurrencesMS' :: Ord a => a -> Map a Int -> Int
ocurrencesMS' a map = case lookupM a map of
                        Just v -> v
                        Nothing -> 0 
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MST map1) (MST map2) = MST (unionMS' map1 map2)

unionMS' :: Ord a => Map a Int -> Map a Int -> Map a Int
unionMS' map1 map2 = unionMS'' (keys map1) map1 map2

unionMS'' :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
unionMS'' []     map1 map2 = map2
unionMS'' (k:ks) map1 map2 = case lookupM k map1 of
                                Just v -> addUnionMS k v map2 (unionMS'' ks map1 map2)
                                Nothing -> map2

addUnionMS :: Ord a => a -> Int -> Map a Int -> Map a Int -> Map a Int
addUnionMS  e n map2 mapRec = case lookupM e map2 of
                                    Just v -> assocM e (n+v) mapRec
                                    Nothing -> assocM e n mapRec

--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
intersectionMS (MST map1) (MST map2) = MST (intersectionMS' map1 map2)

intersectionMS' :: Ord a => Map a Int -> Map a Int -> Map a Int
intersectionMS' map1 map2  = intersectionMS'' (keys map1) map1 map2 

intersectionMS'' :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int 
intersectionMS'' []     map1 map2 = emptyM
intersectionMS'' (k:ks) map1 map2 = case lookupM k map1 of
                                        Just v1 -> addIntersecMS k v1 map2 (intersectionMS'' ks map1 map2) 
                                        Nothing -> (intersectionMS'' ks map1 map2)

addIntersecMS :: Ord a => a -> Int -> Map a Int -> Map a Int -> Map a Int
addIntersecMS  e n map2 mapRec = case lookupM e map2 of
                                    Just v -> assocM e (n+v) mapRec
                                    Nothing -> mapRec
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.
multiSetToList ::Ord a => MultiSet a -> [(a, Int)]
multiSetToList (MST map) = multiSetToList' map

multiSetToList' :: Ord a => Map a Int -> [(a,Int)]
multiSetToList' map = multiSetToList'' map  (keys map )

multiSetToList'' ::Ord a => Map a Int -> [a] -> [(a,Int)]
multiSetToList'' map []     = []
multiSetToList'' map (k:ks) = case lookupM k map of
                                Just v -> (k,v) : (multiSetToList'' map ks)
                                Nothing -> error "parece que una key no se encontraba en el map lo que es paro porque las keys son tomadas de la fucion key"


