module Map1 -- !no acepta keys and values repetidos
    (Map , emptyM, assocM, elem, lookupM, deleteM ,keys)
where

data Map k v = Mp [(k, v)] deriving Show

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = Mp []

--Propósito: agrega una asociación clave-valor al map.
-- cuando se agrega un elemento  y este elemento tiene una key existente en el map , se reemplaza el valor del elemento existente con el valor del elemento a agregar
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM key value (Mp kvs) =  Mp (assocM' (key, value) kvs)

assocM' :: Eq k =>(k,v) -> [(k,v)] -> [(k,v)]
assocM' kv []  =  [(kv)]
assocM' kv (x:xs) =  if (fst kv) == (fst x)
                        then kv : xs
                        else x :(assocM' kv xs)


--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM e (Mp ls) = lookupM' e ls

lookupM' :: Eq k => k -> [(k,v)] -> Maybe v
lookupM' e []       = Nothing
lookupM' e (kv:kvs) = if e == (fst kv)
                        then Just(snd kv)
                        else lookupM' e kvs

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM e (Mp kvs) = Mp(deleteM' e kvs)

deleteM' :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteM' e []       = []
deleteM' e (kv:kvs) = if e == (fst kv)
                        then deleteM' e kvs
                        else kv : (deleteM' e kvs)

--Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (Mp kvs) = keys' kvs

keys' :: [(k,v)] -> [k]
keys' []       = []
keys' (kv:kvs) = fst kv : (keys' kvs)

map1 = Mp [("hola1",1),("hola2",2),("hola3",3),("hola4",4),("hola4",5)]