module Map3
    (Map , emptyM, assocM, elem, lookupM, deleteM ,keys)
where

-- !invariante de representacion 
-- *si una de las dos listas esta vacia la otra tambn lo estara
-- *las claves no estan repetidas
-- *las listas tienen la misma longitud

data Map k v = Mp [k] [v] deriving Show 

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = Mp [] []

--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM key value  (Mp ks vs) = armarMap (assocM' key value ks vs)

armarMap :: ([k],[v]) -> Map k v
armarMap (ks, vs) = Mp ks vs

assocM' :: Eq k => k -> v -> [k] -> [v] -> ([k],[v])
assocM' key value []         []         = ([key],[value])
assocM' key value (ks : kss) (vs : vss) = if key == ks
                                            then ((key:kss), (value:vss))
                                            else agregarAlPar ks vs (assocM' key value kss vss)

agregarAlPar :: Eq k => k -> v -> ([k],[v]) -> ([k],[v])
agregarAlPar key value (ks,vs) = ((key:ks), (value:vs))


--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM key (Mp ks vs) =  lookupM' key ks vs

lookupM' :: Eq k => k -> [k] -> [v] -> Maybe v
lookupM' key []       []       = Nothing
lookupM' key (k : ks) (v : vs) =  if key == k
                                    then Just v 
                                    else (lookupM' key ks vs)

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (Mp ks vs) = armarMap  (deleteM' key ks vs)

deleteM' :: Eq k => k -> [k] -> [v] -> ([k],[v])
deleteM' key []       []       = ([] , [])
deleteM' key (k : ks) (v : vs) = if (key == k)
                                    then deleteM' key ks vs
                                    else agregarAlPar k v (deleteM' key ks vs)

--Propósito: devuelve las claves del map
keys :: Eq k => Map k v -> [k]
keys (Mp ks vs) = keys' ks

keys' :: Eq k => [k] -> [k]
keys' []       = []
keys' (k : ks) = agregark k (keys' ks)

agregark :: Eq k => k -> [k] -> [k]
agregark key  []        = [key]
agregark key (ks : kss) = if key == ks
                            then ks : kss
                            else ks:(agregark key kss)

map1 = Mp ["hola1","hola2","hola3","hola4","hola1"] [1,2,3,4,1]

