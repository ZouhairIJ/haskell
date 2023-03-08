module Aufgabe01 where

add :: Int -> Int -> Int
add x y = x + y 

-- Der Parameter n ist die Position einer Zahl in der Fibonacci-Folge.
-- Die Fibonacci-Folge ist 0, 1, 1, 2, 3, 5, 8, 13, 21, …
fibo :: Int -> Int 
fibo 0 = 0
fibo 1 = 1
fibo x = fibo(x-1) + fibo(x-2)


-- Hinweis: Ein Palindrom ist ein Wort, das man vorwärts und rückwärts lesen kann, wie z.B.
-- otto, rentner, anna, reittier.
palindrom :: String -> Bool
palindrom c = c == reversee c

-- string = [char]
reversee :: String -> String
reversee (x:xs) = reversee xs ++ [x]
reversee [] = []  
    