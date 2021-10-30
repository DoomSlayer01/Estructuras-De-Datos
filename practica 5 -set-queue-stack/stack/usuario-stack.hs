import Stack1

--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar :: [a] -> Stack a
apilar xs =  apilar' (reversa xs) emptyS

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = agregaAlFinal (reversa xs) x

agregaAlFinal :: [a] -> a -> [a]
agregaAlFinal [] elem = [elem]
agregaAlFinal (x:xs) elem = x : agregaAlFinal xs elem

apilar' :: [a]-> Stack a -> Stack a
apilar' []  stack     = stack
apilar' (x:xs) stack  = push x (apilar' xs stack)


--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar :: Stack a -> [a]
desapilar stack = if isEmptyS stack then [] else top stack : (desapilar (pop stack)) 

--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 e stack  =  push e stack
insertarEnPos n e stack  =  insertarEnPos (n-1) e (pop stack)

stack = push 5 (push 4 (push 3(push 2 (push 1 emptyS))))