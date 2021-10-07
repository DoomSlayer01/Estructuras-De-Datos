data Pizza = Prepizza | Capa Ingrediente Pizza

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int  deriving Show

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza                 = 0
cantidadDeCapas (Capa ing pizza) = 1+ cantidadDeCapas pizza

armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (x:xs) = Capa x (armarPizza xs)

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza         = Prepizza
sacarJamon (Capa ing pizza) = if esIngrediente Jamon ing    then sacarJamon pizza
                                                            else Capa ing (sacarJamon pizza)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Salsa Salsa = True
esIngrediente Queso Queso = True
esIngrediente Jamon Jamon = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _      _    = False

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza         = True
tieneSoloSalsaYQueso (Capa ing pizza) =esQuesoOSalsa ing && tieneSoloSalsaYQueso pizza

esQuesoOSalsa :: Ingrediente -> Bool
esQuesoOSalsa Salsa = True
esQuesoOSalsa Queso = True
esQuesoOSalsa _     = False

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza         = Prepizza
duplicarAceitunas (Capa ing pizza) = Capa (duplicarAceitunas'((Aceitunas _) ing) ing)  (duplicarAceitunas pizza)

duplicarAceitunas':: Bool -> Ingrediente -> Ingrediente
duplicarAceitunas' True (Aceitunas n ) = (Aceitunas n*2)
duplicarAceitunas' False _             = _

cantCapasPorPizza :: [Pizza] -> [(Int,Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (x:xs) = armarParPizza x : cantCapasPorPizza xs

armarParPizza :: Pizza -> (Int,Pizza)
armarParPizza Prepizza = (0,Prepizza) 
armarParPizza pizza    = (cantidadDeCapas pizza , pizza )

--Mapa de dir

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

hayTesoro :: Mapa -> Bool
hayTesoro (Fin cofre)                     = tieneTesoro cofre
hayTesoro (Bifurcacion cofre mapa1 mapa2) = tieneTesoro cofre || hayTesoro mapa1 || hayTesoro mapa2 

tieneTesoro :: Cofre -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x ||tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Bifurcacion cofre _ _ )= tieneTesoro cofre
hayTesoroEn [] (Fin cofre)             = tieneTesoro cofre
hayTesoroEn (x:xs) (Bifurcacion cofre mapa1 mapa2) =  case x of Izq ->hayTesoroEn xs mapa1 
                                                                Der ->hayTesoroEn xs mapa2
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin cofre)               = [] --en este caso preguntaria si hay un cofre pero como la precondicion me asegura que existe el cofre no hay necesidad de hacerlo
caminoAlTesoro (Bifurcacion cofre m1 m2) = if tieneTesoro cofre then []
                                                                else if hayTesoroEn [Izq] m1 then Izq : caminoAlTesoro m1 else Der : caminoAlTesoro m2

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cría Nombre
data Manada = M Lobo

manada = M (Cazador "caz1" [] (Explorador ex1 [](Cria "criaEx1")(Cria"criaEx2"))(Explorador ex1 [](Cria "criaEx3")(Cria"criaEx4"))(Cria "criaCaz"))

buenaCaza :: Manada -> Bool
buenaCaza (M lobo) = cantidadDePresas lobo > cantidadDeCrias' lobo

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cria _)                         = 0
cantidadDePresas (Explorador _ _ lob1 lob2)       = (cantidadDePresas lob1)+(cantidadDePresas lob2)
cantidadDePresas (Cazador _ presa lob1 lob2 lob3) = cantPresa presa +(cantidadDePresas lob1)+(cantidadDePresas lob2)+(cantidadDePresas lob3)

cantPresa :: [Presa] -> Int
cantPresa ps = length ps

cantidadDeCrias' :: Lobo ->Int
cantidadDeCrias' (Cria _) = 1
cantidadDeCrias' (Explorador _ _ lob1 lob2) = (cantidadDeCrias' lob1 ) + (cantidadDeCrias'lob2)
cantidadDeCrias' (Cazador _ _ lob1 lob2 lob3) = cantidadDeCrias' lob1 + cantidadDeCrias'lob2 + cantidadDeCrias'lob3


esAlfa :: Manada -> (Nombre,Int)
esAlfa (M lobo) = esAlfa' lobo


-- estaba bien solo habia que hacer un comparadador de alfas mas granular de 2  porque al ser de tres se vuelve inestable
-- quedaria algo como 
--         (comparar lobActual (comparar esAlfa lob1 (comparar esAlfa lob2 esAlfa lob3))
esAlfa' :: Lobo -> (Nombre,Int)
esAlfa' (Cria _ )                    = (_ ,0)
esAlfa' (Explorador nombre _ lob1 lob2)   = maximoAlfa (nombre,0) (esAlfa' lob1)  (esAlfa'lob2)
esAlfa' (Cazador nombre presas lob1 lob2 lob3) = if  (cantPresas presa) >= snd ( maximoAlfa (esAlfa' lob1) (esAlfa'lob2) (esAlfa' lob3)) then (nombre,cantPresa presas) else  ( maximoAlfa (esAlfa' lob1) (esAlfa'lob2) (esAlfa' lob3))

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (Manda lobo) = losQueExploraron' t lobo

losQueExploraron':: Territorio -> Lobo -> [Nombre]
losQueExploraron' t (Cria _)                             = []
losQueExploraron' t (Explorador nombre territ lob1 lob2) = if (elem x xs) then nombre :(losQueExploraron' t lob1 ++ losQueExploraron' t lob2)
                                                                          else losQueExploraron' t lob1 ++ losQueExploraron' t lob2
losQueExploraron' t (Cazador _ _ lob1 lob2 lob3)         = losQueExploraron' t lob1 ++ losQueExploraron' t lob2 ++ losQueExploraron' t lob3

-- superioresAlCazador :: Nombre -> Manada -> [Nombre]
-- superioresAlCazador n (M lobo) = superioresAlCazador' n lobo

-- superioresAlCazador' :: Nombre -> Manada -> [Nombre]
-- superioresAlCazador'



