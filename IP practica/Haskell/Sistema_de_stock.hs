module Sistema_de_stock where

-- Ejercicio 1

contarStock :: String -> [String] -> Int
contarStock _ [] = 0
contarStock n (x:xs) | n == x = 1 + contarStock n xs
                     | otherwise = contarStock n xs

limpiarRepetidos :: String -> [String] -> [String]
limpiarRepetidos n [] = []
limpiarRepetidos n (x:xs) | n == x = limpiarRepetidos n xs
                          | otherwise = x : limpiarRepetidos n xs

generarStock :: [String] -> [(String, Int)]
generarStock [] = []
generarStock (x:xs) = (x , contarStock x (x:xs)) : generarStock (limpiarRepetidos x xs)

-- Ejercicio 2

stockDeProducto :: [(String, Int)] -> String -> Int
stockDeProducto [] _ = 0
stockDeProducto ((x,y):ls) n | x == n = y
                             | otherwise = stockDeProducto ls n

-- Ejercicio 3

dineroProducto :: (String, Int) -> [(String, Float)] -> Float
dineroProducto (x1,x2) ((y1,y2):ys) | x1 == y1 = fromIntegral x2*y2
                                    | otherwise = dineroProducto (x1,x2) ys

dineroEnStock :: [(String, Int)] -> [(String, Float)] -> Float
dineroEnStock [] [] = 0.0
dineroEnStock [] _ = 0.0
dineroEnStock (x:xs) ys = dineroProducto x ys + dineroEnStock xs ys

-- Ejercicio 4

aplicarOferta :: [(String, Int)] -> [(String, Float)] -> [(String, Float)]
aplicarOferta _ [] = []
aplicarOferta x (y:ys) | stockDeProducto x (fst y) > 10 = (fst y, snd y * 0.80) : aplicarOferta x ys
                       | otherwise = y : aplicarOferta x ys

-- ==================================
--          SOPA DE LETRAS
-- ==================================

-- Ejercicio 5

