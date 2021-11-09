
data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)


--Propósito:Devuelve una escuela vacía.
--! Eficiencia:O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

--Propósito:Indica si la escuela está vacía.
--! Eficiencia:O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM setH mapNM pqM) = isEmptyPQ pqM 

--Propósito:Incorpora un mago a la escuela (si ya existe no hace nada).
--! Eficiencia:O(logM)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar name (EDM setH mapNM pqM) = EDM setH (regNewMagoMap' name mapNM) (regNewMagoPQ' name pqM)

regNewMagoMap' :: Nombre -> Map Nombre Mago -> Map Nombre Mago
registMagoMap' name mapNM = assocM name (crearM nombre) mapNM

regNewMagoPQ' :: Nombre -> PriorityQueue Mago -> PriorityQueue Mago 
registMagoPQ' name pqM = insertPQ (crearM name) pqM

--Propósito:Devuelve los nombres de los magos registrados en la escuela.
--! Eficiencia:O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM setH mapNM pqM) = domM mapNM

--Propósito:Devuelve los hechizos que conoce un mago dado.
--Precondición:Existe un mago con dicho nombre.
--! Eficiencia:O(logM)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe name (EDM setH mapNM pqM) = hechizosDe' name mapNM

hechizosDe' :: Nombre -> Map Nombre Mago
hechizosDe' name mapNM = hechizos (valorJust (lookupM name mapNM))  

valorJust (Just v) = v
valorJust _        = error"no esta el elemento buscado"

--Propósito:Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición:Existe un mago con dicho nombre.
--! Eficiencia:O(logM)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender name (EDM setH mapNM pqM) = leFaltanAprender' (valorJust(lookupM name mapNM)) setH

leFaltanAprender' :: Mago -> Set Hechizos -> Int
leFaltanAprender' mago setH = (sizeS(hechizos mago)) - (sizeS setH)

--Propósito:Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición:Hay al menos un mago.
--! Eficiencia:O(logM)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM setH mapNM pqM) = (maxPQ pqM ,(EDM setH (deleteM (maxPQ pqM) mapNM)) (deleteMaxPQ pqM))

--Propósito:Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota:No importa si el mago ya conoce el hechizo dado.
--Precondición:Existe un mago con dicho nombre.
--! Eficiencia:O(MlogM+ logH)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar h name (EDM setH mapNM pqM) = EDM (enseñarSet' h) (enseñarM h name mapNM) (enseñarPQ h name pqM)

enseñarSet :: Hechizo -> Set Hechizo -> Set Hechizo
enseñarSet h setH = addS h seth

enseñarM :: Hechizo -> Nombre -> Map Nombre Mago -> Map Nombre Mago
enseñarM  h name  mapNM  = assocM name (aprender h (valorJust(lookupM name mapNM)))

enseñarPQ  :: Hechizo -> Nombre -> PriorityQueue Mago -> PriorityQueue Mago
enseñarPQ h name pqM  = if nombre(maxPQ pqM) == name
                            then insertPQ (aprender(maxPQ pqM)) (deleteMaxPQ pqM) 
                            else enseñarPQ h name (deleteMaxPQ pqM)
-- USUARIO --

--Propósito:Retorna todos los hechizos aprendidos por los magos.
--! Eficiencia:O(M∗(logM+HlogH))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos escuela  = hechizosAprendidos' (magos escuela) escuela

hechizosAprendidos' :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidos []     escuela = emptyS
hechizosAprendidos (n:ns) escuela = if estaVacia escuela 
                                        then emptyS
                                        else unionS (hechizosDe name escuela) (hechizosAprendidos ns escuela)

--Propósito:Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--! Eficiencia:O(logM)
hayUnExperto :: EscuelaDeMagia -> Bool 
hayUnExperto escuela =(leFaltanAprender (nombre(fst(egresarUno escuela))) escuela) == 0 

--Propósito:Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichosmagos.
--! Eficiencia:O(MlogM)
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos escuela = if estaVacia escuela 
                            then ([],escuela)
                            else ( (fst(egresarUno escuela) : (fst(egresarExpertos (snd (egresarUno escuela))))), snd (egresarUno escuela) )