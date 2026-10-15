-- Problema 1

mismaTemporada :: ([Char],Int) -> [([Char],Int)] -> [[Char]]
mismaTemporada _ [] = []
mismaTemporada x (l:ls) | snd x == snd l = fst l : mismaTemporada x ls
                        | otherwise = mismaTemporada x ls

limpiar :: ([Char],Int) -> [([Char],Int)] -> [([Char],Int)]
limpiar _ [] = []
limpiar x (l:ls) | snd x == snd l = limpiar x ls
                 | otherwise = l : limpiar x ls

agruparPorCantidadDeTemporadas :: [([Char],Int)] -> [(Int,[[Char]])]
agruparPorCantidadDeTemporadas [] = []
agruparPorCantidadDeTemporadas (x:xs) = (snd x, mismaTemporada x (x:xs)) : agruparPorCantidadDeTemporadas (limpiar x xs)

-- Problema 2

listaPares :: Int -> [Int]
listaPares n = [2,4..n]

alCuadrado :: [Int] -> Int
alCuadrado [] = 0
alCuadrado (l:ls) = l*l + alCuadrado ls

sumaCuadradosPares :: Int -> Int
sumaCuadradosPares 0 = 0
sumaCuadradosPares n = alCuadrado (listaPares n)