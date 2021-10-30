 --Todo: practica 2 listas y recursion

sumatoria :: [Int] -> Int
sumatoria []     = 0
sumatoria (x:xs) = x + sumatoria xs 

longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs 

sucesores :: [Int] -> [Int]
sucesores []     = []
sucesores (x:xs) = (x + 1 ) : sucesores xs

conjuncion :: [Bool] -> Bool
conjuncion []     = True
conjuncion (x:xs) = x && conjuncion xs 

disyuncion :: [Bool] -> Bool
disyuncion []     = False
disyuncion (x:xs) = x || disyuncion xs

aplanar :: [[a]] -> [a]
aplanar []     = []
aplanar (x:xs) =  x ++ aplanar xs

pertenece :: Eq a => a -> [a] -> Bool
pertenece elem  []    = False 
pertenece elem (x:xs) = elem == x || pertenece elem xs

apariciones :: Eq a => a -> [a] -> Int
apariciones elem []     = 0
apariciones elem (x:xs) = if x == elem 
                            then 1 + apariciones elem xs
                            else apariciones elem xs

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n [] = []
losMenoresA n (x:xs) = if n > x then x : losMenoresA n xs else losMenoresA n xs

losDeLongitudMayorA :: Int -> [[a]] -> [[a]]
losDeLongitudMayorA n [] = []
losDeLongitudMayorA n (x:xs) = if n < longitud x  
                                    then x : losDeLongitudMayorA n xs
                                    else losDeLongitudMayorA n xs

agregaAlFinal :: [a] -> a -> [a]
agregaAlFinal [] elem = [elem]
agregaAlFinal (x:xs) elem = x : agregaAlFinal xs elem

concatenar :: [a] -> [a] -> [a]
concatenar [] ys = ys
concatenar (x:xs) ys = x: (concatenar xs ys)

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregaAlFinal (reversa xs) x

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos  []  [] = []
zipMaximos (xs) [] = xs
zipMaximos []   ys = ys
zipMaximos (x:xs) (y:ys) = if x > y then x : zipMaximos xs ys else y : zipMaximos xs ys

elMinimo :: Ord a => [a] -> a
elMinimo [n] = n 
elMinimo (x:xs:xss) = if x < xs then elMinimo (x:xss) else elMinimo (xs:xss)

--Recursion sobre numeros

factorial :: Int -> Int
factorial 0 = 1 
factorial n = n*(factorial(n-1))

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

repetir :: Int -> a -> [a]
repetir 0 elem = []
repetir n elem = elem : repetir (n-1) elem

losPrimeros :: Int -> [a] -> [a]
losPrimeros 0  _ = []
losPrimeros n [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs:xss) = sinLosPrimeros (n-1) (xs:xss)

--Registros 

data Persona = Pers String Int deriving Show 

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n personas = mayoresA2 n personas

mayoresA2 :: Int ->[Persona] -> [Persona]
mayoresA2 0 xs = xs
mayoresA2 n (x:xs) = if edad x > n then x : mayoresA2 n xs else mayoresA2 n xs

edad :: Persona -> Int
edad (Pers _ edad) = edad

promedioEdad :: [Persona] -> Int
promedioEdad [] = 0
promedioEdad (x:xs) = (edad x) + promedioEdad xs 

elMasViejo :: [Persona] -> Persona
elMasViejo [p] = p
elMasViejo (x:xs:xss) = if (edad x) > (edad xs) 
                            then elMasViejo (x:xss)
                            else elMasViejo (xs:xss)

data TipoDePokemon = Agua | Fuego | Planta deriving (Show,Eq)
data Pokemon = ConsPokemon TipoDePokemon Int deriving Show
data Entrenador = ConsEntrenador String [Pokemon] deriving Show

tipo :: Pokemon -> TipoDePokemon 
tipo (ConsPokemon tipo int) = tipo

cantPokemon :: Entrenador -> Int
cantPokemon (ConsEntrenador _ poks) = longitud poks

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tipo (ConsEntrenador _ poks) = apariciones tipo (tiposPokemones poks)

tiposPokemones :: [Pokemon] -> [TipoDePokemon]
tiposPokemones [] = []
tiposPokemones (x:xs) = (tipo x) : tiposPokemones xs

losKGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losKGanan tipo (ConsEntrenador _ poks1) (ConsEntrenador _ poks2) = losKGanan2 tipo poks1 poks2

losKGanan2 :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
losKGanan2 t xs [] = longitud xs
losKGanan2 t [] ys = 0
losKGanan2 t (x:xs) (ys) = if (tipo x == t) then (cantidadGanaA x ys) + losKGanan2 t xs ys else losKGanan2 t xs ys

cantidadGanaA :: Pokemon -> [Pokemon] -> Int
cantidadGanaA (ConsPokemon t int) (x:xs) = if superaA t (tipo x) 
                                                then 1 + cantidadGanaA (ConsPokemon t int) xs 
                                                else cantidadGanaA (ConsPokemon t int) xs

superaA :: TipoDePokemon -> TipoDePokemon -> Bool 
superaA  Agua Fuego    = True
superaA  Fuego Planta  = True
superaA  Planta Agua   = True
superaA  _      _      = False

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (ConsEntrenador _ poks1) = maestroPokemon poks1

maestroPokemon :: [Pokemon] -> Bool
maestroPokemon poks = contieneTipo Planta poks && contieneTipo Fuego poks && contieneTipo Agua poks

contieneTipo :: TipoDePokemon -> [Pokemon] -> Bool 
contieneTipo t [] = False
contieneTipo t (x:xs) = (tipo x) == t || contieneTipo t xs 

data Seniority = Junior | Semijunior | Senior  deriving Show
data Proyecto = ConsProyecto String  deriving (Show,Eq)
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = ConsEmpresa [Rol] deriving Show

--sin repetidos
proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa roles) =eliminarDuplicados(sacarProyectos roles)

eliminarDuplicados :: [Proyecto] -> [Proyecto]
eliminarDuplicados [] = []
eliminarDuplicados (x:xs) = if pertenece x xs 
                                then eliminarDuplicados xs 
                                else x : eliminarDuplicados xs

sacarProyectos :: [Rol] -> [Proyecto]
sacarProyectos [] = []
sacarProyectos (x:xs) = proyecto x : sacarProyectos xs


proyecto :: Rol -> Proyecto 
proyecto (Developer _ proyect) = proyect
proyecto (Management _ proyect) = proyect

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior (ConsEmpresa roles) (proyectos) = losDevSenior2 roles proyectos

losDevSenior2 :: [Rol] -> [Proyecto] ->Int
losDevSenior2 [] _ = 0 
losDevSenior2 (r:rs) ps = if esSenior (seniorityR r) && (pertenece (proyecto r) ps ) 
                                then 1 + losDevSenior2 rs ps
                                else losDevSenior2 rs ps

seniorityR :: Rol -> Seniority
seniorityR (Developer s _) =  s
seniorityR(Management s _) =  s

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False

cantTrabajaEn :: [Proyecto] -> Empresa -> Int
cantTrabajaEn proyectos (ConsEmpresa roles) = cantTrabajaEn2 proyectos roles

cantTrabajaEn2 :: [Proyecto] -> [Rol] -> Int
cantTrabajaEn2 []        [] = 0 
cantTrabajaEn2 proyectos [] = 0
cantTrabajaEn2 proyectos (r:rs) = if pertenece (proyecto r) proyectos 
                                        then 1 + cantTrabajaEn2 proyectos rs
                                        else cantTrabajaEn2 proyectos rs

asignadosPorProyecto :: Empresa -> [(Proyecto,Int)]
asignadosPorProyecto (ConsEmpresa roles) = asignadosPorProyecto' roles

asignadosPorProyecto' :: [Rol] -> [(Proyecto,Int)]
asignadosPorProyecto' roles = armarParesProyecto (sacarProyectos roles) roles

armarParesProyecto :: [Proyecto] -> [Rol] -> [(Proyecto,Int)]
armarParesProyecto [] _ = []
armarParesProyecto _ [] = []
armarParesProyecto (p:ps) (roles) =  (p,(cantTrabajaEn2 [p] roles)) : armarParesProyecto ps roles


