import Control.Monad.Accum (MonadAccum(accum))
-- Ejercicio 01
max2:: (Float, Float) -> Float
max2 (x, y)
    | x >= y = x
    | otherwise = y

normaVectorial:: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x**2 + y**2)

max2_curried:: Float -> Float -> Float
max2_curried x y
    | x >= y = x
    | otherwise = y

normaVectorial_curried:: Float -> Float -> Float
normaVectorial_curried x y = sqrt (x**2 + y**2)

ejercicio01 :: IO ()
ejercicio01 = do
    print "max2 (3.5, 7.2):"
    print (max2 (3.5, 7.2))   -- Debería mostrar 7.2
    print "max2 (10, 2):"
    print (max2 (10, 2))      -- Debería mostrar 10.0
    print "max2 (-5, -8):"
    print (max2 (-5, -8))     -- Debería mostrar -5.0

    print "normaVectorial(2, 2):"
    print (normaVectorial(2, 2))
    print "normaVectorial(0, 0):"
    print (normaVectorial(0, 0))

curryy :: ((a, b) -> c) -> a -> b -> c
curryy f x y = f (x, y)

uncurryy :: (a -> b -> c) -> (a, b) -> c
uncurryy f (x, y) = f x y

--curryN :: ((a1, a2, ..., an) -> r) -> a1 -> a2 -> ... -> an -> r

sumaTupla :: (Int, Int) -> Int
sumaTupla (x, y) = x + y

sumaCurried :: Int -> Int -> Int
sumaCurried x y = x + y

ejercicio02 :: IO ()
ejercicio02 = do
    print "(uncurry (curry sumaTupla)) (1, 2) == sumaTupla 1 2"
    print $ (uncurryy (curryy sumaTupla)) (1, 2) == sumaTupla (1, 2)

    print "(curry (uncurry sumaCurried)) 1 2 == sumaCurried (1, 2)"
    print $ (curryy (uncurryy sumaCurried)) 1 2 == sumaCurried 1 2

-- Ejercicio 3
-- Redefinir usando foldr las funciones sum, elem, (++), filter y map

sumFoldr :: [Float] -> Float
sumFoldr = foldr (\x ac -> x + ac) 0

elemFoldr :: Eq a => a -> [a] -> Bool
elemFoldr e = foldr (\x ac -> x == e || ac) False

concatFoldr :: [a] -> [a] -> [a]
concatFoldr xs ys = foldr (\y ac -> y : ac) ys xs

filterFoldr :: Eq a => a -> [a] -> [a]
filterFoldr e = foldr (\x ac -> if x == e then x : ac else ac) []

mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x ac -> f x : ac) []

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f [] = error "lista vacia"
mejorSegun f (x:xs) = foldr (\x ac -> if f x ac then x else ac) x xs

sumasParciales:: Num a => [a] -> [a]
sumasParciales xs = [ sum (take i xs) | i <- [1..length xs]]

sumaAlt:: Num a => [a] -> a
sumaAlt xs = snd (foldr f (if even (length xs) then (-1, 0) else (1, 0)) xs)
    where
        f x (signo, res) = (-signo, res + signo * x)

ejercicio03 :: IO ()
ejercicio03 = do
    print "sumFoldr [1, 2, 3]"
    print (sumFoldr [1, 2, 3])

    print "elemFoldr 2 [1, 2, 3]"
    print (elemFoldr 2 [1, 2, 3])

    print "elemFoldr 4 [1, 2, 3]"
    print (elemFoldr 4 [1, 2, 3])

    print "concatFoldr [1, 2, 3] []"
    print (concatFoldr [1, 2, 3] [])

    print "concatFoldr [1, 2, 3] [4, 5]"
    print (concatFoldr [1, 2, 3] [4, 5])

    print "filterFoldr 2 [1, 2, 3, 4, 2]"
    print (filterFoldr 2 [1, 2, 3, 4, 2])

    print "filterFoldr 2 [1, 3, 4]"
    print (filterFoldr 2 [1, 3, 4])

    print "mapFoldr (*2) [1, 2, 3]"
    print (mapFoldr (*2) [1, 2, 3])

    print "mejorSegun (<) [1, 2, 3, -1]"
    print (mejorSegun (<) [1, 2, 3, -1])

    print "mejorSegun (>) [1, 2, 3, -1]"
    print (mejorSegun (>) [1, 2, 3, -1])

    print "sumasParciales [1, 2, 3, -1]"
    print (sumasParciales [1, 2, 3, -1])

    print "sumaAlt [1, 2, 3, -1]"
    print (sumaAlt [1, 2, 3, -1])

    print "sumaAlt [1, 2, 3]"
    print (sumaAlt [1, 2, 3])


 
{- 
Ejercicio05
Considerar las funciones

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = 
    if null xs then [x]
    else x : elementosEnPosicionesPares tail(xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> 
    if null ys then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)
-}

{- 
elementosEnPosicionesPares NO es una induccion estrutural, porque modifica xs al hacer la recursión. En concreto, hace tail xs para saltearse valores. Foldr va procesando cada elemento en orden
-}
{- 
entrelazar TAMPOCO es una induccion estrutural, porque modifica xs e ys al hacer la recursión.
-}

recr :: (a->[a]->b->b) -> b -> [a] -> b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\x xs r -> if x == e then xs else x:r) []

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e = recr (\x xs r -> if null xs then if x > e then (e:[x]) else (x:[e]) else if x > e then (e:x:xs) else x:r) []

ejercicio06 :: IO()
ejercicio06 = do
    print "sacarUna 2 [1, 2, 3]"
    print (sacarUna 2 [1, 2, 3])

    print "sacarUna 4 [1, 2, 3]"
    print (sacarUna 4 [1, 2, 3])

    print "insertarOrdenado 3 [1, 2, 4]"
    print (insertarOrdenado 3 [1, 2, 4])

    print "insertarOrdenado 4 [1, 2, 3]"
    print (insertarOrdenado 4 [1, 2, 3])

{-
Ejercicio 07
-}

mapPares :: ((a, b) -> c) -> [(a, b)] -> [c]
mapPares f = recr (\x xs r -> f x : r) []


armarPares :: [a] -> [b] -> [(a, b)]
armarPares xs ys = recr zipWith' [] xs
    where
        zipWith' x xs r = case ys of
            [] -> []
            (y:ys') -> (x, y) : armarPares xs ys'

-- | null xs = [(x, head y)]

mapDoble::((a, b) -> c) -> [a] -> [b] -> [c]
mapDoble f xs ys = recr zipWith' [] xs
    where
        zipWith' x xs r = case ys of
            [] -> []
            (y:ys') -> f (x, y) : mapDoble f xs ys'

mapDoble2::((a, b) -> c) -> [a] -> [b] -> [c]
mapDoble2 f xs ys = mapPares f (armarPares xs ys)

ejercicio07 :: IO()
ejercicio07 = do
    print "mapPares sumaTupla [(1, 2), (3, 4), (5, 6)]"
    print (mapPares sumaTupla [(1, 2), (3, 4), (5, 6)])

    print "armarPares [1, 2, 3] [1, 2, 3]"
    print (armarPares [1, 2, 3] [1, 2, 3])

    print "armarPares [1, 2, 3, 4] [1, 2, 3]"
    print (armarPares [1, 2, 3, 4] [1, 2, 3])

    print "mapDoble sumaTupla [1, 2, 3] [1, 2, 3]"
    print (mapDoble sumaTupla [1, 2, 3] [1, 2, 3])

    print "mapDoble sumaTupla [1, 2, 3, 4] [1, 2, 3]"
    print (mapDoble sumaTupla [1, 2, 3, 4] [1, 2, 3])

    print "mapDoble2 sumaTupla [1, 2, 3, 4] [1, 2, 3]"
    print (mapDoble2 sumaTupla [1, 2, 3, 4] [1, 2, 3])
    
{-
Ejercicio 09
-}
ejercicio09 :: IO()
ejercicio09 = do
    print "FoldNat"

-- Ejemplo de uso e impresión en consola
main :: IO ()
main = do
    --ejercicio01
    --ejercicio02
    --ejercicio03
    --ejercicio06
    --ejercicio07
    ejercicio09