-- Ejercicio 1

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

ultimo :: [t] -> t
ultimo (x:[]) = x
ultimo (x:xs) = ultimo xs

principio :: [t] -> [t]
principio (x:[]) = []
principio (x:xs) = x : principio xs

reverso :: [t] -> [t]
reverso (x:[]) = [x]
reverso x = ultimo x : reverso (principio x)

-- Ejercicio 2

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece n [] = False
pertenece n (x:xs) | xs == [] && n /= x = False
| x == n = True
| otherwise = pertenece n xs

quitar :: (Eq t) => t -> [t] -> [t]
quitar n [] = []
quitar n (x:[]) | n /= x = [x]
quitar n (x:xs) | x == n = xs
| otherwise = x : quitar n xs

quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos n [] = []
quitarTodos n (x:xs) | pertenece n (x:xs) == False = (x:xs)
| otherwise = quitarTodos n (quitar n (x:xs))

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:[]) = [x]
eliminarRepetidos (x:xs) | pertenece x xs == False = x : eliminarRepetidos xs
| otherwise = x : eliminarRepetidos (quitarTodos x xs)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [x] [y] = x == y
mismosElementos (x:xs) [] = False
mismosElementos [] (y:ys) = False
mismosElementos (x:xs) (y:ys) | pertenece x (y:ys) == True = mismosElementos (quitarTodos x xs) (quitarTodos x ys)
| otherwise = False



-- ejercicio 3.3

maximo :: [Integer] -> Integer
maximo (x:[]) = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
| otherwise = maximo (y:xs)

sumarN :: Integer -> [Integer] -> [Integer]
sumarN n (x:[]) = [x+n]
sumarN n (x:xs) = (x+n) : sumarN n xs

-- ejercicio 3.9

ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar (x:xs) = maximo (x:xs) : ordenar (quitarTodos (maximo (x:xs)) (x:xs))

-- Era areves [1,2,3,4,5,6,7,8] asi