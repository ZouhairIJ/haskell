module Aufgabe02 where


-- Welche Fehler sind in den folgenden Programm zu finden?
-- f :: Fractional a => a -> a -> a
-- f x y
--  | x > y = x - y
--  | otherwise = x + y
-- aufruf :: Int
-- aufruf = f 3.5 2.0

-- "a" soll ein Instanz von "Ord" und "Fractional" sein
ff :: (Ord a, Fractional a) => a -> a -> a
ff x y | x > y = x - y
       | otherwise = x + y  


aufruf :: Double
aufruf = ff 3.5 2.0

----------------- 

-- Eine Funktion, die die n-te Zahl der Fibonacci-Folge findet.
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)


fibGates :: Int -> Int 
fibGates n | n == 0 = 0
           | n == 1 = 1
           | otherwise = fibGates (n-1) + fibGates (n-2)

-----------------

-- Ein Programm, das den Mittelwert aller ganzen Zahlen von x bis y
-- rechnet (x und y sind ganze Zahlen).
-- Hinweis:
-- - In Haskell gibt es keine automatische Typ-Umwandlung.
-- - Falls Sie den Operator / auf ganze Zahlen anwenden möchten, verwenden Sie die
-- Typkonverter-Funktion fromIntegral.
--  z.B. fromIntegral (summe x y) / fromIntegral (anzahl x y)


sums :: Int -> Int -> Int
sums x y | x <= y = x + sums (x+1) y
         | otherwise = 0

numOfnums :: Int -> Int -> Int 
numOfnums x y | x <= y = 1 + numOfnums(x+1) y
              | otherwise = 0

mittelwert :: Int -> Int -> Double
mittelwert x y = fromIntegral(sums x y) / fromIntegral(numOfnums x y)

-----------------

-- PROBLEM : Sie haben 155 Cent und sehen ein Regal mit Bonbons, die 10 Cent, 20 Cent, 30
-- Cent usw. bis hinauf zu einem Euro kosten.
-- Sie kaufen von jeder Sorte nur ein Bonbon, beginnend mit dem Bonbon für 10 Cent, bis Ihr
-- Restgeld für ein weiteres Bonbon nicht mehr ausreicht. Schreiben Sie ein Programm, das
-- rechnet, wie viele Bonbons Sie kaufen können.

bonbon :: Int -> Int -> Int
bonbon geld sorte | sorte <= geld && sorte <= 100 = 1 + bonbon (geld - sorte) (sorte + 10)
                  | otherwise = 0


