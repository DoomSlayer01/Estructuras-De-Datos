--Propósito:dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personasprogramaron juntas.
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
programasEnComun  p p2 org = if programaronJuntas org p p2
                                then interseccion (programasDe p org) (programasDe p2 org) 
                                else emptyS

--Propósito:denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
esUnGranHacker :: Organizador -> Persona -> Bool
esUnGranHacker org persona = esUnGranHacker' (todosLosProgramas org)(programasDe persona org)

esUnGranHacker' :: [Checksum] -> Set Checksum -> Bool
esUnGranHacker' (c:cs) setC = belongS c setC && esUnGranHacker' cs setC

data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

-- Propósito:Un organizador vacío.
-- ! Eficiencia:O(1)
nuevo :: Organizador
nuevo = MKO (emptyM) (emptyM)

-- Propósito:Agrega al organizador un programa con el Checksum indicado; 
-- el conjunto es el conjunto de personas autoresde dicho programa.
-- Precondición:el identificador del programa que se agrega no fue usado previamente en el organizador, 
-- y el Set de personas no está vacío.
-- ! Eficiencia:no hay ninguna garantía de eficiencia.
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma (MKO mapCP mapPC) c setP = MKO (assocM c setP mapCP) (actualizarAutoresMap c (set2List setP) mapPC )

actualizarAutoresMap :: Checksum -> [Persona] -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
actualizarAutoresMap c (p:ps) mapPC =  case lookupM p mapPC of
                                            Just v -> assocM p (addS c v ) (actualizarAutoresMap c ps mapPC)
                                            Nothing ->assocM p (addS c emptyS) (actualizarAutoresMap c ps mapPC)


-- Propósito:denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
-- ! Eficiencia:O(C)en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas (MKO mapCP mapPC) = domM mapCP

-- Propósito:denota el conjunto de autores que aparecen en un programa determinado.
-- Precondición:el Checksum debe corresponder a un programa del organizador.
-- ! Eficiencia:O(logC)en peor caso, dondeCes la cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe (MKO mapCP mapPC) c = valorJust (lookupM c mapCP)

-- Propósito:denota el conjunto de programas en los que participó una determinada persona.
-- Precondición:la persona debe existir en el organizador.
-- ! Eficiencia:O(logP)en peor caso, dondePes la cantidad total de personas del organizador.
programasDe :: Organizador -> Persona -> Set Checksum
programasDe (MKO mapCP mapPC) p = valorJust (lookupM p mapPC)

-- Propósito:dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
-- Precondición:las personas deben ser distintas.
-- ! Eficiencia:O(logP+ClogC)en peor caso, donde 'P'es la cantidad de personas distintas que aparecen en todos losprogramas del organizador, 
-- ! y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas (MKO mapCP mapPC) p p2 =  not (isEmptyS 
                                                    (intersection 
                                                        (valorJust (lookupM p mapPC)) 
                                                        (valorJust(lookupM p2 mapPC))
                                                ))

-- Version 2 aunque me parece la solucion correcta 
-- programaronJuntas' :: Map Checksum -> [Checksum]-> Persona -> Persona -> Set Checksum
-- programaronJuntas' mapCP (c:cs) p p2 = case lookupM c mapCP of
--                                             Just setP -> (belongS p setP && belongS p2 setP) || (programaronJuntas' mapCP cs p p2)
--                                             Nothing   -> (programaronJuntas' mapCP cs p p2)
