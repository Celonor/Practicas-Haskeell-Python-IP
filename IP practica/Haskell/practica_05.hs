-- ============
-- Ejercicio 1
-- ============

-- Ejercicio 1.1
longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Ejercicio 1.2
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- Ejercicio 1.3
principio :: [t] -> [t]
principio [x] = []
principio (x:xs) = x:principio xs

-- Ejercicio 1.4
reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = reverso xs ++ [x]

-- ===========
-- Ejercicio 2
-- ===========

-- Ejercicio 2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece n (x:xs) | n == x = True
                   | otherwise = pertenece n xs

-- Ejercicio 2.2
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [] = True
todosIguales [_] = True
todosIguales (x:y:xs) | x /= y = False
                      | otherwise = todosIguales (y:xs)

-- Ejercicio 2.3
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [] = True
todosDistintos [_] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs

-- Ejercicio 2.4
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

-- Ejercicio 2.5
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar n (x:xs) | x == n = xs
                | otherwise = x : quitar n xs

-- Ejercicio 2.6
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos n (x:xs) | n == x = quitarTodos n xs
                     | otherwise = x : quitarTodos n xs

-- Ejercicio 2.7
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

-- Ejercicio 2.8
mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos _ [] = False
mismosElementos [] _ = False
mismosElementos (x:xs) y | pertenece x y = mismosElementos (quitarTodos x xs) (quitarTodos x y)
                         | otherwise = False

-- Ejercicio 2.9
capicua :: (Eq t) => [t] -> Bool
capicua x = x == reverso x

-- ===========
-- Ejercicio 3
-- ===========

-- Ejercicio 3.1
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- Ejercicio 3.2
productoria :: [Integer] -> Integer
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- Ejercicio 3.3
maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:y:xs) | x >= y = maximo (x:xs)
                | otherwise = maximo (y:xs)

-- Ejercicio 3.4
sumarN :: Integer -> [Integer] -> [Integer]
sumarN _ [] = []
sumarN n [x] = [x+n]
sumarN n (x:xs) = (x+n) : sumarN n xs

-- Ejercicio 3.5
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- Ejercicio 3.6
sumarElUltimo :: [Integer] -> [Integer]
sumarElUltimo x = sumarN (ultimo x) x

-- Ejercicio 3.7
pares :: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = x : pares xs
             | otherwise = pares xs

-- Ejercicio 3.8
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN _ [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x : multiplosDeN n xs
                      | otherwise = multiplosDeN n xs

-- Ejercicio 3.9
ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar [x] = [x]
ordenar x = ordenar (quitar (maximo x) x) ++ [maximo x]

-- ===========
-- Ejercicio 4
-- ===========

type Texto = [Char]

-- Ejercicio 4.a
sacarBlancosRepetidos :: Texto -> Texto
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos (' ':' ':xs) = sacarBlancosRepetidos (' ':xs)
sacarBlancosRepetidos (x:xs) = x : sacarBlancosRepetidos xs

-- Ejercicio 4.b
sacarBlancosExtremos :: Texto -> Texto
sacarBlancosExtremos [] = []
sacarBlancosExtremos (' ':xs) = sacarBlancosExtremos xs
sacarBlancosExtremos x | ultimo x == ' ' = sacarBlancosExtremos (principio x)
                       | otherwise = x

limparCadena :: Texto -> Texto
limparCadena x = sacarBlancosExtremos (sacarBlancosRepetidos x)

contarLetras :: Char -> Texto -> Integer
contarLetras _ [] = 0
contarLetras n (x:xs) | x == n = 1 + contarLetras n xs
                      | otherwise = contarLetras n xs

contarPalabras :: Texto -> Integer
contarPalabras x | limparCadena x == [] = 0
                 | otherwise = contarLetras ' ' (limparCadena x) + 1

-- Ejercicio 4.c
primeraPalabra :: Texto -> Texto
primeraPalabra [] = []
primeraPalabra (' ':xs) = []
primeraPalabra (x:xs) = x:primeraPalabra xs

sacarPrimeraPalabra :: Texto -> Texto
sacarPrimeraPalabra [] = []
sacarPrimeraPalabra (' ':xs) = xs
sacarPrimeraPalabra (x:xs) = sacarPrimeraPalabra xs

palabrasAux :: Texto-> [Texto]
palabrasAux [] = []
palabrasAux ls = primeraPalabra ls : palabrasAux (sacarPrimeraPalabra ls)

palabras :: Texto-> [Texto]
palabras ls = palabrasAux (limparCadena ls)

-- Ejercicio 4.d
palabraMasLargaAux :: [Texto]-> Texto
palabraMasLargaAux [] = []
palabraMasLargaAux [x] = x
palabraMasLargaAux (x:y:ls) | longitud x >= longitud y = palabraMasLargaAux (x:ls)
                            | otherwise = palabraMasLargaAux (y:ls)

palabraMasLarga :: Texto-> Texto
palabraMasLarga ls = palabraMasLargaAux (palabras ls)

-- Ejercicio 4.e
aplanar :: [Texto]-> Texto
aplanar [] = []
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs 

-- Ejercicio 4.f
aplanarConBlancos :: [Texto]-> Texto
aplanarConBlancos [] = []
aplanarConBlancos [x] = x
aplanarConBlancos (x:xs) = x ++ [' '] ++ aplanarConBlancos xs

-- Ejercicio 4.g
repetirLetra :: Integer -> Char -> Texto
repetirLetra 0 _ = []
repetirLetra n k = k : repetirLetra (n-1) k

aplanarConNBlancos :: [Texto]-> Integer-> Texto
aplanarConNBlancos [] _ = []
aplanarConNBlancos ls 0 = aplanar ls
aplanarConNBlancos [x] _ = x
aplanarConNBlancos (x:xs) n = x ++ repetirLetra n ' ' ++ aplanarConNBlancos xs n

-- ===========
-- Ejercicio 5
-- ===========

-- Ejercicio 5.1

sumaAcumulada :: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [x] = [x]
sumaAcumulada (x:y:ls) = x:sumaAcumulada (y+x:ls)

-- Ejercicio 5.2

auxPrimos :: Integer -> Integer -> [Integer]
auxPrimos x k | k == x = [k]
              | mod x k == 0 = k:auxPrimos (div x k) k
              | otherwise = auxPrimos x (k+1)

descomponerEnPrimos :: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos [x] = [auxPrimos x 2]
descomponerEnPrimos (x:xs) = auxPrimos x 2 : descomponerEnPrimos xs

-- ===========
-- Ejercicio 6
-- ===========

type Nombre = Texto
type Telefono = Texto
type Contacto = (Nombre, Telefono)
type ContactosTel = [Contacto]

-- Ejercicio 6.a

enLosContactos :: Nombre -> ContactosTel -> Bool
enLosContactos _ [] = False
enLosContactos x (y:ys) | x == fst y = True
                        | otherwise = enLosContactos x ys

-- Ejercicio 6.b

agregarContacto :: Contacto -> ContactosTel -> ContactosTel
agregarContacto x [] = [x]
agregarContacto x (y:ys) | fst x == fst y = x:ys
                         | otherwise = y : agregarContacto x ys

-- Ejercicio 6.c

eliminarContacto :: Nombre -> ContactosTel -> ContactosTel
eliminarContacto x [] = []
eliminarContacto x (y:ys) | x == fst y = ys
                          | otherwise = y : eliminarContacto x ys

-- ===========
-- Ejercicio 7
-- ===========

type Identificacion = Integer
type Ubicacion = Texto
type Estado = (Disponibilidad, Ubicacion)
type Locker = (Identificacion, Estado)
type MapaDeLockers = [Locker]
type Disponibilidad = Bool

-- Ejercicio 7.1

existeElLocker :: Identificacion -> MapaDeLockers -> Bool
existeElLocker _ [] = False
existeElLocker n (x:xs) | n == fst x = True
                        | otherwise = existeElLocker n xs

-- Ejercicio 7.2

ubicacionDelLocker :: Identificacion -> MapaDeLockers -> Ubicacion
ubicacionDelLocker n ((x,(y,z)):ls) | n == x = z
                                    | otherwise = ubicacionDelLocker n ls

-- Ejercicio 7.3

estaDisponibleElLocker :: Identificacion -> MapaDeLockers -> Bool
estaDisponibleElLocker n ((x,(y,z)):ls) | n == x = y
                                        | otherwise = estaDisponibleElLocker n ls

-- Ejercicio 7.4

ocuparLocker :: Identificacion -> MapaDeLockers -> MapaDeLockers
ocuparLocker n ((x,(y,z)):ls) | n == x = ((x,(False,z)):ls)
                              | otherwise = (x,(y,z)) : ocuparLocker n ls

lockers = [(100,(False,"ZD39I")),(101,(True,"JAH3I")),(103,(True,"IQSA9")),(105,(True,"QOTSA")),(109,(False,"893JJ")),(110,(False,"99292"))]

-- ===========
-- Ejercicio 8
-- ===========

-- Ejercicio 8.1

sumaLista :: [Integer] -> Integer
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

sumaTotal :: [[Integer]] -> Integer
sumaTotal [] = 0
sumaTotal (x:xs) = sumaLista x + sumaTotal xs

-- Ejercicio 8.2

aparicionesFila :: Integer -> [Integer] -> Integer
aparicionesFila _ [] = 0
aparicionesFila e (x:xs) | e == x = 1 + aparicionesFila e xs
                         | otherwise = aparicionesFila e xs

cantidadDeApariciones :: Integer -> [[Integer]] -> Integer
cantidadDeApariciones _ [] = 0
cantidadDeApariciones e (x:xs) = aparicionesFila e x + cantidadDeApariciones e xs

-- Ejercicio 8.3

contarEnFila :: String -> [String] -> Int
contarEnFila _ [] = 0
contarEnFila p (x:xs) | p == x = 1 + contarEnFila p xs
                      | otherwise = contarEnFila p xs

contarPalabras2 :: String -> [[String]]-> Int
contarPalabras2 _ [] = 0
contarPalabras2 p (x:xs) = contarEnFila p x + contarPalabras2 p xs

-- Ejercicio 8.4

aparicionesFila2 :: (Eq a) => a -> [a] -> Integer
aparicionesFila2 _ [] = 0
aparicionesFila2 n (x:xs) | n == x = 1 + aparicionesFila2 n xs
                         | otherwise = aparicionesFila2 n xs

cantidadDeApariciones2 :: (Eq a) => a -> [[a]] -> Integer
cantidadDeApariciones2 _ [] = 0
cantidadDeApariciones2 n (x:xs) = aparicionesFila2 n x + cantidadDeApariciones2 n xs

-- Ejercicio 8.5

escalarEnLista :: Integer -> [Integer] -> [Integer]
escalarEnLista _ [] = []
escalarEnLista n (x:xs) = (n*x) : escalarEnLista n xs

multiplicarPorEscalar :: Integer -> [[Integer]] -> [[Integer]]
multiplicarPorEscalar _ [] = []
multiplicarPorEscalar n (x:xs) = escalarEnLista n x : multiplicarPorEscalar n xs

-- Ejercicio 8.6

concatenarFila :: [String] -> String
concatenarFila [] = ""
concatenarFila (x:xs) = x ++ concatenarFila xs

concatenarFilas :: [[String]] -> [String]
concatenarFilas [] = []
concatenarFilas (x:xs) = concatenarFila x : concatenarFilas xs

-- Ejercicio 8.7

iesimaFila :: Integer -> [[a]] -> [a]
iesimaFila n (x:xs) | n == 0 = x
                    | otherwise = iesimaFila (n-1) xs

-- Ejercicio 8.8

auxColumna :: Integer -> [a] -> a
auxColumna n (x:xs) | n == 0 = x
                    | otherwise = auxColumna (n-1) xs

iésimaColumna :: Integer -> [[a]] -> [a]
iésimaColumna _ [] = []
iésimaColumna n (x:xs) = auxColumna n x : iésimaColumna n xs

-- Ejercicio 8.9

matrizIdentidadFila :: Integer -> Integer -> [Integer]
matrizIdentidadFila 0 _ = []
matrizIdentidadFila n k | k == n = 1 : matrizIdentidadFila (n-1) k
                        | otherwise = 0 : matrizIdentidadFila (n-1) k

auxMatrizIdentidad :: Integer -> Integer -> [[Integer]]
auxMatrizIdentidad n k | k == 1 = [matrizIdentidadFila n k]
                       | otherwise = matrizIdentidadFila n k : auxMatrizIdentidad n (k-1)

matrizIdentidad :: Integer -> [[Integer]]
matrizIdentidad n = auxMatrizIdentidad n n

-- Ejercicio 8.10

auxParesColumna :: [Integer] -> Integer
auxParesColumna [] = 0
auxParesColumna (x:xs) | mod x 2 == 0 = 1 + auxParesColumna xs
                       | otherwise = auxParesColumna xs

cantidadParesColumna :: Integer -> [[Integer]] -> Integer
cantidadParesColumna n ls = auxParesColumna (iésimaColumna n ls)
