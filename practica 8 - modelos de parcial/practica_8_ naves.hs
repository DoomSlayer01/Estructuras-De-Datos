--Propósito:Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- ! Eficiencia:O(S log S)
construir :: [SectorId] -> Nave
construir sectores = N (construir' sectores) (emptyH) (emptyM)

construir' :: [SectorId] -> Map SectorId Sector
construir' []             = emptyM
construir' (sctr : sctrs) = assocM sctr (crearSector sctr) (construir' sctrs)


--Propósito:Incorpora un tripulante a la nave, sin asignarle un sector.
-- ! Eficiencia:O(logT)
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N sctrs tps mxHeapT) = N sctrs (ingresarT' n r tps ) (ingresarHeapT' n r mxHeapT)

ingresarT' :: Nombre -> Rango -> Map Nombre Tripulante -> Map Nombre Tripulante
ingresarT' n r mapTrip = assocM n (crearT n r) mapTrip

ingresarHeapT' :: Nombre -> Rango -> MaxHeap Tripulante
ingresarHeapT' n r heatTrip = insertH (crearT n r ) mapTrip

-- Propósito:Devuelve los sectores asignados a un tripulante.
-- ! Precondición:Existe un tripulante con dicho nombre.
-- ! Eficiencia:O(logM)
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados  n (N sctrsM tpsM mxHeapT) = sectoresAsignados' n tpsM

sectoresAsignados' :: Nombre -> Map nombre Tripulante -> Set SectorId
sectoresAsignados' nombre map  = case lookUpM  nombre map of
                                    Just tripulante -> sectoresT tripulante

-- Propósito:Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- ! Precondición:Existe un sector con dicho id.
-- ! Eficiencia:O(logS)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector sID (N sctrsM tptsM mxHeapT) = datosDeSector' sID sctrsM

datosDeSector' :: SectorId -> Map SectorId Sector -> (Set Nombre, [Componente])
datosDeSector' sID mapTrip =  case lookUpM sID mapTrip of
                                    Just sector -> (tripulantesS sector , componentesS sector)

-- Propósito:Devuelve la lista de tripulantes ordenada por rango, de mayor a menor
-- ! Eficiencia:O(logT)
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N sctrsM tptsM mxHeapT) = tripulantesN' maxHeapT

tripulantesN' :: MaxHeap Tripulante -> [Tripulante]
tripulantesN' maxHeapT = if isEmptyH then [] else maxH maxHeapT : ( tripulantesN' (deleteMaxH maxHeapT))

-- Propósito:Asigna una lista de componentes a un sector de la nave.
-- ! Eficiencia:O(C+ logS), siendoCla cantidad de componentes dados.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cmpnnts sID (N sctrsM tptsM mxHeapT) = N (agregarASector' cmpnnts sID sctrsM) tptsM maxHeapT 

agregarASector' :: [Componente] -> SectorId -> Map SectorId Sector -> Map SectorId Sector
agregarASector' cs sID mapSctrs = assocM sID ( agregarComponentes cs (valorJust (lookUpM siD mapSctrs)) mapSctrs

agregarComponentes :: [Componente] -> Sector -> Sector
agregarComponentes []       sector  = sector
agregarComponentes (c : cs) sector  = agregarC c (agregarComponentes cs sector)

valorJust (Just v) = v
valorJust _        = error "no se encontro el elemento buscado"

-- Propósito:Asigna un sector a un tripulante.Nota:No importa si el tripulante ya tiene asignado dicho sector.
-- ! Precondición:El tripulante y el sector existen.
-- ! Eficiencia:O(logS+ logT+TlogT)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector name sID (N sctrsM tptsM maxHeapT) = N   (assigSctrTripMS name sID sctrsM)  
                                                        (assigSctrTripMT name sID tptsM) 
                                                        (assigSctrTripMXHT name sID maxHeapT)

assigSctrTripMS :: Nombre -> SectorId -> Map SectorId Sector -> Map SectorId Sector
assigSctrTripMS name sID  mapSctrs = assocM sID ( agregarT name (valorJust (lookUpM sID mapSctrs)) ) mapSctrs

assigSctrTripMT :: Nombre -> SectorId -> Map Nombre Tripulante -> Map Nombre Tripulante
assigSctrTripMT name sID  mapTrips = assocM name ( asignarS sID (valorJust(lookUpM name mapTrips)) ) mapTrips

assigSctrTripMXHT :: Nombre -> SectorId -> MaxHeap Tripulante
assigSctrTripMXHT name sID maxHeapTrip = if nombre (maxH maxHeapTrip) == name 
                                            then insertH (asignarS sID (maxH maxHeapTrip) maxHeapTrip
                                            else assigSctrTripMXHT name sID (deleteMaxH maxHeapTrip)

--INTERFAZ DE USUARIO--

--Propósito:Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- eficiencia (t log t)
sectores :: Nave -> Set SectorId
sectores nave = sectores'(tripulantesN nave) t log t 

sectores' :: [Tripulantes] ->Set SectorId  
sectores' []             = emptyS
sectores' (trip : trips) = unionS (sectores trip) (sectores' trips) 

--Propósito:Devuelve los tripulantes que no poseen sectores asignados.
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados nave = sinSectoresAsignados' (tripulantesN nave)

sinSectoresAsignados' :: [Tripulante] -> [Tripulante]
sinSectoresAsignados' []             = []
sinSectoresAsignados' (trip : trips) = if sizeS (sectores trip) == 0 
                                            then trip : (sinSectoresAsignados' trips)
                                            else sinSectoresAsignados' trips


--Propósito:Devuelve todos los barriles de los sectores asignados de la nave.
--t * log t + s *log s  
barriles :: Nave -> [Barril]
barriles nave = barriles' (sectores nave) nave

barriles' :: Set SectorId -> Nave -> [Barril]  
barriles' setSectorId nave = barriles'' (asignacionesPorSector (setToList setSectorId) nave )

barriles'' :: [(Set Nombre, [Componente])] -> [Barril] -- n*2
barriles'' []        = []
barriles'' (c : cs) = (obtenerBarriles  c) ++ barriles'' cs

obtenerBarriles :: Componente -> [Barril] -- 0 (1)
obtenerBarriles (Almacen brrls) = brrls
obtenerBarriles _               = []

asignacionesPorSector :: [SectorId] -> Nave -> [(Set Nombre, [Componente])] -- n log n
asignacionesPorSector []     nave = []
asignacionesPorSector (s:sc) nave = datosDeSector s nave : (asignacionesPorSector sc nave)





