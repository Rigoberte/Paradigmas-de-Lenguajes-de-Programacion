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

normaVectorialCurried:: Float -> Float -> Float
normaVectorialCurried x y = sqrt (x**2 + y**2)

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

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat _ z 0 = z
foldNat f z n
    | n > 0 = f n (foldNat f z (n-1))
    | otherwise = error "Undefined"

potencia:: Integer -> Integer -> [Integer]
potencia p = foldNat (\x acc -> x^p : acc) []

{-
Ejercicio 09
-}
ejercicio09 :: IO()
ejercicio09 = do
    print "foldNat (+) 0 10"
    print (foldNat (+) 0 10)

    print "foldNat (+) 2 2"
    print (foldNat (+) 2 2)

    print "foldNat (*) 1 4"
    print (foldNat (*) 1 4)

    print "foldNat (*) 0 1"
    print (foldNat (*) 0 1)

    print "foldNat (:) [] 10"
    print (foldNat (:) [] 10)

    print "potencia 1 10"
    print (potencia 1 10)

    print "potencia 2 10"
    print (potencia 2 10)

{-
Ejercicio 12
-}

data AB a = 
    Nil 
    | Bin (AB a) a (AB a)
    deriving (Eq, Show)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f Nil = z
foldAB z f (Bin i r d) = f (foldAB z f i) r (foldAB z f d)

recAB :: b -> (AB a -> b -> a -> AB a -> b -> b) -> AB a -> b
recAB z f Nil = z
recAB z f (Bin i r d) = f i (recAB z f i) r d (recAB z f d)

esNil :: AB a -> Bool
esNil t = case t of
    Nil -> True
    _ -> False

altura :: AB a -> Int
altura = foldAB 0 (\li _ ld -> 1 + max li ld)

cantNodos :: AB a -> Int
cantNodos = foldAB 0 (\li _ ld -> 1 + li + ld)

raiz :: AB a -> a
raiz (Bin _ v _) = v

mejorSegunAB :: (a -> a -> Bool) -> AB a -> Maybe a
mejorSegunAB f = foldAB Nothing g
  where
    g Nothing  x Nothing  = Just x
    g (Just l) x Nothing  = Just (if f l x then l else x)
    g Nothing  x (Just r) = Just (if f r x then r else x)
    g (Just l) x (Just r) =
      let mejorIzq = if f l x then l else x
      in Just (if f r mejorIzq then r else mejorIzq)

{-
esABB :: Ord a => AB a -> Bool
esABB = foldAB Nothing g
  where
    g :: Maybe (a,a,Bool) -> a -> Maybe (a,a,Bool) -> Maybe (a,a,Bool)
    g ml x mr =
      let okL = maybe True (\(_,maxL,ok) -> ok && maxL <= x) ml
          okR = maybe True (\(minR,_,ok) -> ok && x <= minR) mr
          minV = maybe x (\(minL,_,_) -> minL) ml
          maxV = maybe x (\(_,maxR,_) -> maxR) mr
      in Just (minV, maxV, okL && okR)
-}

ejercicio12 :: IO()
ejercicio12 = do
    print "esNil Nil"
    print (esNil Nil)

    print "esNil (Bin (Bin Nil 1 Nil) 10 Nil)"
    print (esNil (Bin (Bin Nil 1 Nil) 10 Nil))

    print "altura Nil"
    print (altura Nil)

    print "altura (Bin (Bin Nil 1 Nil) 10 Nil)"
    print (altura (Bin (Bin Nil 1 Nil) 10 Nil))

    print "cantNodos Nil"
    print (cantNodos Nil)

    print "cantNodos (Bin (Bin Nil 1 Nil) 10 Nil)"
    print (cantNodos (Bin (Bin Nil 1 Nil) 10 Nil))

    print "mejorSegunAB (>) (Bin (Bin Nil 5 Nil) 3 (Bin Nil 8 Nil)"
    print (mejorSegunAB (>) (Bin (Bin Nil 5 Nil) 3 (Bin Nil 8 Nil)))

    print "mejorSegunAB (<) (Bin (Bin Nil 5 Nil) 3 (Bin Nil 8 Nil)"
    print (mejorSegunAB (<) (Bin (Bin Nil 5 Nil) 3 (Bin Nil 8 Nil)))


data RoseTree a = Node a [RoseTree a]
    deriving (Eq, Show)

foldRoseTree :: (a -> [b] -> b) -> RoseTree a -> b
foldRoseTree f (Node r xs) = f r (map (foldRoseTree f) xs)

hojas :: RoseTree a -> [a]
hojas = foldRoseTree f
    where
        f x [] = [x]
        f x xs = concat xs

distancia :: RoseTree a -> [Int]
distancia = foldRoseTree f
    where
        f _ [] = [1]
        f _ xs = map (+1) (concat xs)

alturaRoseTree :: RoseTree a -> Int
alturaRoseTree t = mejorSegun (>) (distancia t)

ejercicio15 :: IO()
ejercicio15 = do
    print "hojas (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []])"
    print (hojas (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []]))

    print "distancia (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []])"
    print (distancia (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []]))

    print "distancia (Node 2 [])"
    print (distancia (Node 2 []))

    print "alturaRoseTree (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []])"
    print (alturaRoseTree (Node 1 [Node 2 [], Node 3 [Node 4 []], Node 5 []]))

    print "alturaRoseTree (Node 2 [])"
    print (alturaRoseTree (Node 2 []))

-- Ejemplo de uso e impresión en consola
main :: IO ()
main = do
    --ejercicio01
    --ejercicio02
    --ejercicio03
    --ejercicio06
    --ejercicio07
    --ejercicio09
    --ejercicio12
    ejercicio15