module Map2 -- !acepta keys and values repetidos -- por lo que no hay que verificcar si hay repetidos al agregar .
    (Map , emptyM, assocM, elem, lookupM, deleteM ,keys)
where

-- !invariante de representacion 
-- *acepta keys repetidas

data Map k v = Mp [(k, v)] deriving Show 

--Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = Mp []
--Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM key value (Mp kvs) = Mp((key ,value) : kvs)

--Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM key (Mp kvs) =  lookupM' key kvs

lookupM' :: Eq k => k -> [(k,v)] -> Maybe v
lookupM'  key []        = Nothing
lookupM'  key (kv : kvs) = if key == fst kv
                            then Just (snd kv)
                            else lookupM' key kvs

--Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM key (Mp kvs) = Mp (deleteM' key kvs) 

deleteM' :: Eq k => k -> [(k,v)] -> [(k,v)]
deleteM' key []         = []
deleteM' key (kv : kvs) = if key == fst kv 
                            then deleteM' key kvs
                            else kv : deleteM' key kvs

--Propósito: devuelve las claves del map.
-- !dado que el map puede tener repetidos se tiene que devolver las keys sin repetidas para que es usuario no perciba la estructura
keys :: Eq k => Map k v -> [k]
keys (Mp kvs) = keys' kvs

keys' :: Eq k => [(k,v)] -> [k]
keys' []         = []
keys' (kv : kvs) =  agregarKey (fst kv) (keys' kvs)

agregarKey :: Eq k => k -> [k] -> [k]
agregarKey key []         = [key]
agregarKey key (ky : kys) = if key == ky 
                                then (agregarKey key kys)
                                else key : (ky : kys)

map1 = Mp [("hola6",3),("hola2",1),("hola2",2),("hola3",3),("hola3",4),("hola5",3)]