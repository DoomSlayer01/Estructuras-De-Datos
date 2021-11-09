--Propósito:Denota los tripulantes de la nave
tripulantes :: Nave -> Set Tripulante
tripulantes nave = tripulantes' (sectores nave) nave

tripulantes' :: [Sector] -> Nave -> Set Tripulante
tripulantes' []     nave = emptyS
tripulantes' (s:ss) nave = unionS' (tripulantesDe s) (tripulantes' ss nave)

-- !--Propósito:Elimina al tripulante de la nave.
-- !--Pista:Considere reconstruir la nave sin ese tripulante
-- ! bajaDeTripulante :: Tripulante -> Nave -> Nave
-- ! bajaDeTripulante t nave = 

data Nave = MKN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

-- Propósito:Crea una nave con todos esos sectores sin tripulantes.
-- Precondición:la lista de sectores no está vacía
-- ! Costo:O(SlogS)siendoSla cantidad de sectores de la lista.
naveVacia :: [Sector] -> Nave
naveVacia ss = MKN (agregarSectores ss) (emptyH) (head ss , 0)

agregarSectores :: [Sector] -> Map Sector (Set Tripulante) 
agregarSectores []   = emptyM
agregarSectores (s:ss) = assocM s emptyS (agregarSectores ss)

-- Propósito:Obtiene los tripulantes de un sector.
-- ! Costo:O(logS)siendoSla cantidad de sectores.
tripulantesDe :: Sector -> Nave -> Set Tripulante
tripulantesDe sector (MKN mapST hT parSI) = tripulantesDe' sector mapSt

tripulantesDe' :: Sector -> Map Sector (Set Tripulante) -> Set Tripulante
tripulantesDe' sector mapST = valorJust(lookupM sector mapST)

valorJust (Just v) = v
valorJust _        = error "no se encontro el elemento buscado "

-- Propósito:Denota el tripulante con mayor rango.
-- Precondición:la nave no está vacía.
-- ! Costo:O(S)siendoSla cantidad de sectores.
conMayorRango :: Nave -> Tripulante
conMayorRanfo (MKN mapST hT parSI) = findMin hT

-- Propósito:Denota los sectores de la nave
-- ! Costo:O(N).
sectores :: Nave -> [Sector]
sectores (MKN mapST hT parSI) = domM mapST
-- Propósito:Denota el sector de la nave con más tripulantes.
-- ! Costo:O(1).
conMasTripulantes :: Nave -> Sector
conMasTripulantes (MKN mapST hT parSI) =  fst(parSI)

-- Propósito:Denota el conjunto de tripulantes con dicho rango.
-- ! Costo:O(PlogP)siendoPla cantidad de tripulantes.
conRango :: Rango -> Nave -> Set Tripulante
conRango rango (MKN mapST hT parSI) = conRango' rango hT 

conRango :: Rango -> Heap Tripulante -> Set Tripulante
conRango r hT set = if isEmptyH ht
                        then emptyS
                        else if rango(findMin hT) == r 
                                then addS (findMin hT) (conRango r (deleteMin hT) set)
                                else conRango r (deleteMin hT set)

-- Propósito:Devuelve el sector en el que se encuentra un tripulante.
-- Precondición:el tripulante pertenece a la nave.
-- ! Costo:O(Slog Slog P)siendoSla cantidad de sectores yPla cantidad de tripulantes.
sectorDe :: Tripulante -> Nave -> Sector
sectorDe t (MKN mapST hT parSI) = sectorDe t mapST (domM mapST)

sectorDe' :: Tripulante -> Map Sector (Set Tripulante) -> [Sector] -> Sector
sectorDe' t mapST (s:ss)= case lookupM s mapST of
                            Just tripulantes -> if belongS t tripulantes then s  else (sectorDe' t mapST ss)
                            Nothing -> error "no se encontro el tripulante en ninguno de los sectores"

-- Propósito:Agrega un tripulante a ese sector de la nave.
-- Precondición:El sector está en la nave y el tripulante no.
-- ! Costo:No hay datos (justifique su elección).
agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
agregarTripulante t sector (MKN mapST hT parSI) = MKN (agregarTripMap t sector mapST ) ( agregarTripHeap t hT) (actualizarSector t sector parSI)

agregarTripMap :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarTripMap t s mapST = case lookupM s mapST of
                                Just v -> assocM s (addS t v) mapST

agregarTripHeap :: Tripulante -> Heap Tripulante -> Heap Tripulante
agregarTripHeap  t hT = insertH t ht

actualizarSector :: Tripulante -> Sector -> (Sector,Int) -> (Sector,Int)
actualizarSector t s par = if (fst par) == s then (s,(snd par)+1) else par


