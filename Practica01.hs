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

-- Ejemplo de uso e impresión en consola
main :: IO ()
main = do
    ejercicio01
    ejercicio02
    ejercicio03