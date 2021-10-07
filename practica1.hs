--Numeros Enteros

sucesor:: Int -> Int
sucesor x = x + 1 

sumar:: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int,Int)
divisionYResto x y =( div x y ,mod x y)

maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if (x > y ) then x else y

--TiposEnumerativos

data Dir = Norte | Sur | Oeste | Este deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Oeste Oeste = True 
iguales Este Este = True
iguales _ _ = False

siguiente :: Dir -> Dir
--Es una funcion total porque cubre todos los casos porque no necesita una precondicion
siguiente Norte = Oeste 
siguiente Sur   = Este
siguiente Este  = Norte
siguiente Oeste = Sur


data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving(Show,Eq)

primeroYUltimoDia :: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM _         = False

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues Lunes Domingo    = True
vieneDespues Martes Lunes     = True
vieneDespues Miercoles Martes = True
vieneDespues Jueves Miercoles = True
vieneDespues Viernes Jueves   = True
vieneDespues Sabado Viernes   = True
vieneDespues Domingo Sabado   = True
vieneDespues _       _        = False

estaEnElMedio :: DiaDeSemana -> Bool 
estaEnElMedio Lunes   = False
estaEnElMedio Domingo = False
estaEnElMedio _       = True

estaEnElMedio2 :: DiaDeSemana -> Bool
estaEnElMedio2 dia = dia /= Lunes && dia /= Domingo

negar :: Bool -> Bool 
negar True  = False
negar False = True

implica :: Bool -> Bool -> Bool 
implica True False = False
implica _    _     = True

and :: Bool -> Bool -> Bool 
and True True = True
and _    _    = False

or :: Bool -> Bool -> Bool 
or False False = False 
or _     _     = True

--Registros

data Persona = Pers String Int deriving (Show)

nombre :: Persona -> String 
nombre (Pers n edad) = n 

edad :: Persona -> Int
edad (Pers n edad)  = edad 

crecer :: Persona -> Persona
crecer (Pers n edad) = Pers n (edad+1)

cambioDeNombre :: String -> Persona -> Persona 
cambioDeNombre cambio (Pers n edad) = Pers cambio edad

esMayorQueLaOtra :: Persona -> Persona -> Bool 
esMayorQueLaOtra per1 per2 = edad per1 > edad per2

laQueEsMayor :: Persona -> Persona -> Persona 
laQueEsMayor per1 per2 = if esMayorQueLaOtra per1 per2  then per1 else per2

data TipoDePokemon = Agua | Fuego | Planta deriving (Show,Eq)
data Pokemon = Pok TipoDePokemon Int deriving (Show)
data Entrenador = Entr String Pokemon Pokemon deriving (Show)

pokPlanta = Pok Planta 123
pokFuego = Pok Fuego 123
pokAgua = Pok Agua 123
ash = Entr "Ash" pokFuego pokFuego


{- Agua supera Fuego
   Fuego supera Planta
   Planta supera Agua -}

superaA :: Pokemon -> Pokemon -> Bool 
superaA (Pok Agua _) (Pok Fuego _)   = True
superaA (Pok Fuego _) (Pok Planta _) = True
superaA (Pok Planta _) (Pok Agua _)  = True
superaA _               _            = False

cantidadPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantidadPokemonesDe tipo (Entr _ pok1 pok2) = cantidadPokTipo tipo pok1 pok2

cantidadPokTipo :: TipoDePokemon -> Pokemon -> Pokemon -> Int
cantidadPokTipo  tipo (Pok tipo1 _) (Pok tipo2 _) = if tipo == tipo1 && tipo == tipo2 
                                                         then 2 
                                                         else if tipo /= tipo1 && tipo /= tipo2 
                                                                  then 0 
                                                                  else 1

juntarPokemones :: (Entrenador,Entrenador) -> [Pokemon]
juntarPokemones ((Entr _ pok1 pok2),(Entr _ pok3 pok4)) = [pok1,pok2,pok3,pok4]

--Funciones Polimorficas

loMismo :: a -> a 
loMismo a = a

siempreSiete :: a -> Int
siempreSiete x = 7 

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

--Patter matching sobre listas 

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x:xs) = x

sinElPrimero :: [a] -> [a]
sinElPrimero [] = []
sinElPrimero (x:xs) = xs

splitHead :: [a] -> (a,[a])
splitHead (x:xs) = (x , xs)


--ejercicios adicionales de años anteriores 

data Pizza = Prepizza | Agregar Ingrediente Pizza  deriving Show

data Ingrediente = Salsa | Queso | Jamon | AceitunasVerdes Int deriving (Show,Eq)

pizza =Agregar Jamon(Agregar (AceitunasVerdes 2) (Agregar (AceitunasVerdes 3)( Agregar Queso (Agregar Jamon Prepizza))))

pizza2 = Agregar (AceitunasVerdes 2) Prepizza

ingredientes :: Pizza -> [Ingrediente]
ingredientes Prepizza = []
ingredientes (Agregar ingrediente pizza) = ingrediente : ingredientes pizza 

tieneJamon :: Pizza -> Bool
tieneJamon Prepizza = False
tieneJamon (Agregar ingrediente pizza) = esJamon ingrediente || tieneJamon pizza

esJamon :: Ingrediente -> Bool 
esJamon Jamon = True
esJamon _     = False

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Agregar ingrediente pizza) = if esJamon ingrediente then sacarJamon pizza else (Agregar ingrediente (sacarJamon pizza))

armarPizza :: [Ingrediente] -> Pizza
armarPizza [] = Prepizza
armarPizza (ingrediente : ingredientes) = Agregar ingrediente (armarPizza ingredientes)

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Agregar ingrediente pizza) =  if esAceituna ingrediente 
                                                   then Agregar (duplicarIngrediente ingrediente) (duplicarAceitunas pizza) 
                                                   else Agregar ingrediente (duplicarAceitunas pizza)

esAceituna :: Ingrediente -> Bool 
esAceituna (AceitunasVerdes int) = True
esAceituna _                     = False

duplicarIngrediente :: Ingrediente -> Ingrediente
duplicarIngrediente (AceitunasVerdes n) = AceitunasVerdes (n*2)

sacar :: [Ingrediente] -> Pizza -> Pizza
sacar xs pizza = sacarIngPizza (xs) pizza

sacarIngPizza :: [Ingrediente] -> Pizza -> Pizza
sacarIngPizza [] pizza                       = pizza
sacarIngPizza xs Prepizza                    = Prepizza
sacarIngPizza xs (Agregar ingrediente pizza) = if pertenece ingrediente xs
                                                      then sacarIngPizza (actLista xs ingrediente) pizza
                                                      else Agregar ingrediente (sacarIngPizza xs pizza) 

pertenece ::Eq a =>  a -> [a] -> Bool 
pertenece n [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs 

actLista :: [Ingrediente] -> Ingrediente ->[Ingrediente]
actLista [] ing = []
actLista (x:xs) ing = if x == ing then (xs) else x : actLista xs ing

cantJamon :: [Pizza] -> [(Int,Pizza)]
cantJamon pizzas = cantJamon2 pizzas

cantJamon2 :: [Pizza] -> [(Int,Pizza)]
cantJamon2 [] = []
cantJamon2 (x:xs) = armarPar x : cantJamon xs

armarPar :: Pizza -> (Int,Pizza)
armarPar Prepizza = (0,Prepizza)
armarPar pizza = (contarJamon pizza , pizza)

contarJamon :: Pizza -> Int
contarJamon Prepizza = 0
contarJamon (Agregar ingrediente pizza) = if esJamon ingrediente 
                                             then 1 + contarJamon pizza 
                                             else contarJamon pizza


mayorNAceitunas :: Int -> [Pizza] -> [Pizza]
mayorNAceitunas int pizzas = conMasAceitunas int pizzas

conMasAceitunas :: Int -> [Pizza] -> [Pizza]
conMasAceitunas 0  pizzas  = pizzas
conMasAceitunas int (x:xs) = if tieneMasDeN int x 
                                 then x : conMasAceitunas int xs 
                                 else conMasAceitunas int xs

tieneMasDeN :: Int -> Pizza -> Bool
tieneMasDeN 0 Prepizza = False
tieneMasDeN int pizza  = (cantAceitunas pizza) > int

cantAceitunas :: Pizza -> Int
cantAceitunas           Prepizza          = 0
cantAceitunas (Agregar ingrediente pizza) = if esAceituna ingrediente
                                                then accederAceituna ingrediente + (cantAceitunas pizza)
                                                else cantAceitunas pizza

accederAceituna :: Ingrediente -> Int
accederAceituna (AceitunasVerdes int) = int
accederAceituna _                     = 0


cantIngrediente :: Ingrediente -> Pizza -> Int
cantIngrediente ing           Prepizza          =  0
cantIngrediente ing (Agregar ingrediente pizza) =  if esAceituna ing
                                                      then (accederAceituna ingrediente) + cantIngrediente ing pizza
                                                      else if ing == ingrediente
                                                               then 1 + cantIngrediente ing pizza
                                                               else cantIngrediente ing pizza

-- ejercicio de tesoro

data Objeto = Cacharro | Tesoro  deriving (Show,Eq)
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving (Show,Eq)

camino = Cofre [Cacharro](Cofre [Tesoro,Cacharro](Cofre [Tesoro,Cacharro] Fin))

hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada camino) = False || hayTesoro camino
hayTesoro (Cofre obj camino) = contieneTesoro obj || hayTesoro camino

contieneTesoro :: [Objeto] -> Bool
contieneTesoro [] = False
contieneTesoro (x:xs) = x == Tesoro || contieneTesoro xs

--precondicion existe al menos un tesoro en el camino
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                 = 0
pasosHastaTesoro (Nada camino)       = if hayTesoro camino 
                                          then pasosHastaTesoro Fin
                                          else 1 + pasosHastaTesoro camino
pasosHastaTesoro (Cofre obj camino) = if contieneTesoro obj 
                                          then pasosHastaTesoro Fin
                                          else 1 + pasosHastaTesoro camino

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn _ Fin                = False
hayTesoroEn 0 (Nada _ )          = False
hayTesoroEn 0 (Cofre obj _ )     = contieneTesoro obj
hayTesoroEn n (Nada camino)      = hayTesoroEn (n-1) camino
hayTesoroEn n (Cofre obj camino) = hayTesoroEn (n-1) camino

alMenosNtesoros :: Int -> Camino -> Bool
alMenosNtesoros n (camino) = cantidadTesoros camino >= n 

cantidadTesoros :: Camino -> Int
cantidadTesoros Fin = 0 
cantidadTesoros (Nada camino) = cantidadTesoros camino
cantidadTesoros (Cofre obj camino) = if contieneTesoro obj 
                                          then 1 + cantidadTesoros camino
                                          else cantidadTesoros camino

-- cantTesorosEntre :: Int -> Int -> Camino -> Int
-- cantTesorosEntre n n Fin                =
-- cantTesorosEntre n n (Nada camino)      =
-- cantTesorosEntre n n (Cofre obj camino) =











