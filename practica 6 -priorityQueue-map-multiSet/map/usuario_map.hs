import Map3

--Propósito: obtiene los valores asociados a cada clave del map.
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM map = valuesM' (keys map) map

valuesM' :: Eq k => [k] -> Map k v -> [Maybe v]
valuesM' []     map = []
valuesM' (x:xs) map = lookupM x map : (valuesM' xs map)

--Propósito: indica si en el map se encuenrtan todas las claves dadas
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas []     map = True
todasAsociadas (x:xs) map = case lookupM x map of
                                Just v -> True && (todasAsociadas xs map)
                                Nothing -> False 

--Propósito: convierte una lista de pares clave valor en un map.
listToMap :: Eq k => [(k, v)] -> Map k v
listToMap xs = listToMap' xs emptyM 

listToMap' :: Eq k => [(k,v)] -> Map k v -> Map k v
listToMap' []     map = map
listToMap' (x:xs) map = assocM (fst x) (snd x) (listToMap' xs map)

--Propósito: convierte un map en una lista de pares clave valor.
mapToList :: Eq k => Map k v -> [(k, v)]
mapToList map = mapToList' (keys map) map

mapToList' :: Eq k => [k] -> Map k v -> [(k,v)]
mapToList' []     map = []
mapToList' (x:xs) map = case lookupM x map of
                            Just v -> (x,v) : (mapToList' xs map)
                            Nothing -> mapToList' xs map

--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
--la misma clave.
agruparEq :: Eq k => [(k, v)] -> Map k [v]
agruparEq xs = agruparEq' xs emptyM

agruparEq' :: Eq k => [(k,v)] -> Map k [v] -> Map k [v]
agruparEq' []     map = map
agruparEq' (x:xs) map = groupElem (fst x) (snd x) (agruparEq' xs map)

groupElem :: Eq k => k -> v -> Map k [v] -> Map k [v]
groupElem key value map = case lookupM key map of 
                        Just v -> assocM key (value:v) map
                        Nothing -> assocM key [value] map


--Proposito: dada una lista de claves de tipo k y un mapa que va de k a int, le suma uno a
--cada número asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar []     map = map
incrementar (x:xs) map = case lookupM x map of
                            Just v -> assocM x (v+1)(incrementar xs (deleteM x map))
                            Nothing -> incrementar xs map

--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps map1 map2 = mergeMaps' map1 map2 (keys map1)

mergeMaps' :: Eq k => Map k v -> Map k v -> [k] -> Map k v
mergeMaps' map1 map2 []     = map2
mergeMaps' map1 map2 (x:xs) = case lookupM x map1 of
                                Just v -> mergeMaps' map1 (assocMerge x v map2) xs
                                Nothing -> mergeMaps' map1 map2 xs

assocMerge :: Eq k => k -> v -> Map k v -> Map k v
assocMerge key value map = case lookupM key map of
                                    Just v -> assocM key value (deleteM key map)
                                    Nothing -> assocM key value map

--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar :: [a] -> Map Int a
indexar xs  = indexar' 0 xs emptyM

indexar' :: Int -> [a] -> Map Int a  -> Map Int a
indexar' n []     map =  map
indexar' n (e:es) map =  assocM (n+1) e (indexar' (n+1) es map)

--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias s = ocurrencias' s s 

ocurrencias' :: String ->String -> Map Char Int
ocurrencias' []     s = emptyM
ocurrencias' (x:xs) s  = assocM  x (apariciones x s) (ocurrencias' xs s)

apariciones :: Char -> String -> Int
apariciones a []     = 0
apariciones a (s:st) = if a == s then 1 + (apariciones a st) else apariciones a st



lenght :: [a] -> Int
lenght [] = 0
lenght (x:xs) = 1 + (lenght xs)


map1 :: Map String Int
map1 = assocM "hola2" 2 (assocM "hola2" 2 (assocM "hola1" 1 emptyM))

map2 = assocM "hola3" 3 (assocM "hola2" 3 (assocM "hola1" 3 emptyM))


