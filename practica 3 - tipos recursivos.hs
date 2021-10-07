data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

celda1 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
--pensar si ya hay una funcion que resuelva el problema
--RTA : si hay una que se llama contar aceitunas creo que contaba la cantidad de aceitunas en una pizza
nroBolitas :: Color -> Celda -> Int
nroBolitas col CeldaVacia  = 0
nroBolitas col (Bolita col2 celda2) = (unoSiEsColor (esColor col col2)) + (nroBolitas col celda2)

unoSiEsColor :: Bool -> Int
unoSiEsColor True = 1
unoSiEsColor _    = 0

esColor :: Color -> Color ->Bool
esColor Azul Azul = True
esColor Rojo Rojo = True
esColor _    _    = False

poner :: Color -> Celda -> Celda
poner col CeldaVacia           = Bolita col (CeldaVacia)
poner col (Bolita col2 celda2) = Bolita col(poner col celda2)

sacar :: Color -> Celda -> Celda
sacar col CeldaVacia           = CeldaVacia
sacar col (Bolita col2 celda2 )= sacarSi col2 (esColor col col2) (sacar col celda2)

sacarSi :: Color -> Bool -> Celda -> Celda
sacarSi col True celda  = celda
sacarSi col False celda = Bolita col celda

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 col CeldaVacia           =  CeldaVacia
ponerN n col CeldaVacia           = poner col (ponerN (n-1) col CeldaVacia)
ponerN 0 col (Bolita col2 celda2) = (Bolita col2 celda2)
ponerN n col (Bolita col2 celda2) = poner col (ponerN (n-1) col celda2)


--Camino hacia tesoro

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Nada Camino | Cofre [Objeto] Camino deriving Show

caminoTesoro   = Cofre [Cacharro,Tesoro] (Nada (Cofre [Cacharro] Fin))
caminoNoTesoro = Cofre [Cacharro] (Nada (Cofre [Cacharro] Fin))
caminoRango    = Cofre [Cacharro] (Cofre [Cacharro](Cofre [Cacharro,Tesoro](Cofre [Cacharro](Cofre [Tesoro](Cofre[Cacharro](Nada Fin))))))
                                                            --------2------------- 3 -------------- 4 ----------- 5 --------
hayTesoro :: Camino -> Bool
hayTesoro Fin                  = False
hayTesoro (Nada camino )       = hayTesoro camino 
hayTesoro (Cofre objts camino) = tieneTesoro objts || hayTesoro camino

tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (x:xs) = esTesoro x || tieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro Fin                  = 0 --poner error de que hay al menos un tesoro
pasosHastaTesoro (Nada camino)        = 1 + (pasosHastaTesoro camino)
pasosHastaTesoro (Cofre objts camino) = sumar1Si(tieneTesoro objts)+(pasosHastaTesoro (cortarCaminoSi (tieneTesoro objts) camino))

sumar1Si :: Bool -> Int
sumar1Si True  = 0
sumar1Si False = 1

cortarCaminoSi :: Bool -> Camino -> Camino
cortarCaminoSi True  _      = Fin
cortarCaminoSi False camino = camino

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 Fin                  = False
hayTesoroEn n Fin                  = False
hayTesoroEn 0 (Nada camino)        = False
hayTesoroEn n (Nada camino)        = hayTesoroEn (n-1) camino
hayTesoroEn 0 (Cofre obtjs camino) = tieneTesoro obtjs
hayTesoroEn n (Cofre objts camino) = hayTesoroEn (n-1) camino

-- alMenosNTesoros :: Int -> Camino -> Bool
-- alMenosNTesoros 0 Fin                   = True
-- alMenosNTesoros n Fin                   = False
-- alMenosNTesoros 0 (Nada camino)         = contrario (hayTesoro camino)
-- alMenosNTesoros n (Nada camino)         = (alMenosNTesoros (n-1) camino)
-- alMenosNTesoros 0 (Cofre objts camino) =  contrario (tieneTesoro objts)
-- alMenosNTesoros n (Cofre objts camino) = (alMenosNTesoros (n-1) camino)

-- contrario :: Bool -> Bool
-- contrario True = False
-- contrario False = True

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n camino = n == cantidadTesoros camino

cantidadTesoros :: Camino -> Int
cantidadTesoros Fin                  = 0
cantidadTesoros (Nada camino)        = cantidadTesoros camino
cantidadTesoros (Cofre objts camino) = sumar1Si(tieneTesoro objts) + cantidadTesoros camino

--desafio--

cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre x y Fin                  = 0

cantTesorosEntre 0 0 (Nada camino)        = 0
cantTesorosEntre 0 y (Nada camino)        = cantTesorosEntre  0    (y-1) camino
cantTesorosEntre x y (Nada camino)        = cantTesorosEntre (x-1) (y)   camino

cantTesorosEntre 0 0 (Cofre objts camino) = 0 
cantTesorosEntre 0 y (Cofre objts camino) = unoSi(tieneTesoro objts) + cantTesorosEntre  0 (y-1) camino
cantTesorosEntre x y (Cofre objts camino) = cantTesorosEntre (x-1) y camino

unoSi:: Bool -> Int
unoSi True = 1
unoSi False = 0


--tipos arboreos

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbola :: Tree Int
arbola = NodeT (0) 
        (NodeT (1) EmptyT EmptyT) (NodeT (1) 
                                        (NodeT (2) EmptyT (NodeT (3) EmptyT EmptyT)) (NodeT (2) EmptyT (NodeT(3)
                                                                                                                EmptyT (NodeT (4) EmptyT EmptyT))))


treePrueba :: Tree Int
treePrueba = (NodeT 1
                (NodeT 2 
                    (NodeT 3 EmptyT EmptyT) (NodeT 4 EmptyT EmptyT)) 
                (NodeT 5 
                    (NodeT 6 EmptyT EmptyT) (NodeT 7 EmptyT EmptyT))
             )

sumarT:: Tree Int -> Int
sumarT EmptyT            = 0
sumarT (NodeT x izq der) = x + (sumarT izq) + (sumarT der)

sizeT:: Tree a -> Int
sizeT EmptyT            = 0
sizeT (NodeT x izq der) = 1 + max (sizeT izq)  (sizeT der)

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT            = EmptyT
mapDobleT (NodeT x izq der) = NodeT (x*2) (mapDobleT izq) (mapDobleT der)

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT = False
perteneceT e (NodeT x izq der) = x==e || perteneceT e izq || perteneceT e der

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x izq der) = unoSi (x==e) + (aparicionesT e izq) + (aparicionesT e der)

leaves :: Tree a ->[a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = x:[]
leaves (NodeT x izq der) = (leaves der)++(leaves izq)

height :: Tree a -> Int
height EmptyT                  = 0
height (NodeT x EmptyT EmptyT) = 1
height (NodeT x izq der)       = 1 + max (height der) (height izq)

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x izq der) = NodeT x (mirrorT der) (mirrorT izq)

toList :: Tree a -> [a]
toList EmptyT            = []
toList (NodeT x izq der) =[x]++(toList izq)++(toList der)

levelN :: Int -> Tree a -> [a]
levelN n EmptyT            = []
levelN 0 (NodeT x izq der) = [x]
levelN n (NodeT x izq der) = (levelN (n-1) izq)++(levelN (n-1) der)


listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT            = []
listPerLevel (NodeT x izq der) = [ x ]: (listasPorNivel (listPerLevel izq) (listPerLevel der))

listasPorNivel :: [[a]] -> [[a]] -> [[a]]
listasPorNivel [] []         = []                           
listasPorNivel xs []         = xs
listasPorNivel [] ys         = ys
listasPorNivel (x:xs) (y:ys) = (x ++ y) : (listasPorNivel xs ys)


ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT            = []
ramaMasLarga (NodeT x izq der) = x : (ramaMasLarga(maximaRama izq der))

maximaRama :: Tree a -> Tree a -> Tree a
maximaRama EmptyT EmptyT = EmptyT 
maximaRama rama1  rama2  = if ((height rama1) > (height rama2))then rama1 else rama2 

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x EmptyT EmptyT) = [[x]]
todosLosCaminos (NodeT x izq der ) = (agregarATodos x (todosLosCaminos izq)) ++ (agregarATodos x (todosLosCaminos der))


agregarATodos :: a ->[[a]] ->[[a]]
agregarATodos e  [] = []
agregarATodos e  (x:xs) = (e:x) : (agregarATodos e xs)