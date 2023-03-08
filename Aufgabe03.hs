module Aufgabe03 where


-- Eine Funktion, die das n-te bis m-te Element einer Liste ausgibt.
-- unterListe :: Int → Int → [a] → [a]
-- Hinweis: Sie können die Funktionen drop und take verwenden.

-- drop :: Int -> [a] -> [a] -----> drop 3 [2,6,9,5,7] = [5,7]  --> ignoriert die ersten n Elemente der Liste.
-- take :: Int -> [a] -> [a] -----> ghci> take 3 [2,6,9,5,7] = [2,6,9] --> liefert die ersten n Elemente der Liste

unterListe :: Int -> Int -> [a] -> [a]
unterListe n m liste =  take (m - n) (drop n liste)

-------------------

-- Eine Funktion, die die ersten n Fibonacci-Zahlen in eine Liste aufsammelt.
-- fibListe :: Int → [Int]
-- Hinweis: Verwendung von (++)-Operator.

fibG :: Int -> Int 
fibG n | n == 0 = 0
           | n == 1 = 1
           | otherwise = fibG (n-1) + fibG (n-2)

fibListe :: Int -> [Int]
fibListe 0 = []
fibListe n = fibG n : fibListe (n - 1)
-- DIE RHEINFOLGE IST WICHTIG !


--------------------

-- Eine Funktion, die eine Liste von Paaren in ein Paar von Listen umwandelt.
-- listeZuPaar :: [ ( String , Int ) ] -> ( [ String ] , [ Int ] )
-- z.B. Wäre die Liste [(“aaa“,1), (“bbb“,2)], soll das Ergebnis ([“aaa“,“bbb“] ,[1,2]) sein.

-- 2 hilfs Funktionen :
first :: (String, Int) -> String
first (x,_) = x 
second :: (String, Int) -> Int
second (_,x) = x

listeZuPaar :: [ ( String , Int ) ] -> ( [ String ] , [ Int ] )
listeZuPaar ((name, zahl) : xs) = (name : first (listeZuPaar xs), zahl : second (listeZuPaar xs))
listeZuPaar [] = ([],[])






