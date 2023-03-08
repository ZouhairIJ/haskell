module Aufgaben where
import Data.Char

type Telefon = Integer
type Name = String


-- Rekursiv Prozess
listofname [] = []
listofname ((name,telefon):xs) =  name : listofname xs

listoftelefon [] = []
listoftelefon ((name,telefon):xs) = telefon : listoftelefon xs

fff :: [ ( Name , Telefon ) ] -> ( [ Name ] , [ Telefon ] )
fff listeoftuples = ((listofname listeoftuples), (listoftelefon listeoftuples))


-- Funktionen Höhere Ordnung

-- Mapping

mal10 :: (Num a)=> [a] -> [a]
mal10 [] = []
mal10 (x:xs) = x * 10 : mal10 xs

mal5 :: (Num a)=> [a] -> [a]
mal5 [] = []
mal5 (x:xs) = x * 5 : mal5 xs

mal2 :: (Num a)=> [a] -> [a]
mal2 [] = []
mal2 (x:xs) = x * 2 : mal2 xs


listeBearbeiten :: (a -> b) -> [a] -> [b]
listeBearbeiten f [] = []
listeBearbeiten f (x:xs) = f x : listeBearbeiten f xs



-- listeBearbeiten = map
ungerade n = map f [0..n-1]
            where f x = x * 2 + 1


gerade n = map f [0..n-1]
        where f x = x * 2



-- myFilter xs = [x | x <- xs, (<5) xs x]


-- 0 * 2 + 1 = 1
-- 1 * 2 + 1 = 3
-- 2 * 2 + 1 = 5
-- 3 * 2 + 1 = 7 .. 

-- Liste Falten : Faltung

-- liste Falten für addition und Produkt
--Unterschiede:
    -- Addition : 
        -- "+" Operator
        -- "0" Neutrales Element
    -- Produkt : 
        -- "*" Operator
        -- "1" Neutrales Element
-- => Die Unterschiede werden in Parameter abgebildet 

listeFalten :: [b] -> (b -> a -> a) -> a -> a
listeFalten (x:xs) (f) nelement = f x (listeFalten xs f nelement)
listeFalten [] (f) nelement = nelement
                                    

summe [] = 0
summe (x:xs) = x + summe xs

produkt [] = 1 
produkt (x:xs) = x * produkt xs

listefaltung f ele (x:xs) = f x (listefaltung f ele xs)
listefaltung f ele [] = ele

listefaltungfoldr f elem (x:xs) = f x (listefaltungfoldr f elem xs)
listefaltungfoldr f elem [] = elem


listeFiltern f liste = foldr fun [] liste
                    where
                         fun x xs = (:) (f x) xs

listeFilternn f liste = foldr fun [] liste
                    where 
                        fun x xs = [x | x < 5, (f x) xs]
 
listeFilternnn f liste = foldr (\x xs -> (f x) (:)  xs) [] liste


listeFilternnnn f liste = foldr ((:).f) [] liste

sqrlist [] = []
sqrlist (x:xs) = sqrt x : sqrlist xs


sumLength :: (Num a) => [[a]] -> Int
sumLength [] = 0
sumLength (x:xs) = length x + sumLength xs

sumLength2 liste = sum (map length liste)

even_only [] = []
even_only (x:xs) 
            | is_even x = x : (even_only xs)
            | otherwise = even_only xs
            where
                is_even x = (mod x 2) == 0

even_onlyy p [] = []
even_onlyy p (x:xs) 
            | p x = x : (even_onlyy p xs)
            | otherwise = even_onlyy p xs



filterliste [] = []
filterliste (x:xs) 
            | f x = x : (filterliste xs)
            | otherwise = filterliste xs
            where
                f x = x < 5


filterlistee p [] = []
filterlistee p (x:xs) 
            | (p x) = x : (filterlistee p xs)
            | otherwise = filterlistee p xs

filterlisteee p [] = []
filterlisteee p (x:xs) = if (p x) then x : (filterlisteee p xs)
                        else filterlisteee p xs


mapkom f liste = foldr (\x xs -> (f x) : xs) [] liste

maxi n m | n > m = n 
         | otherwise = m

allEqual n m s | (n == m) && (m == s) = True
               | otherwise = False

--Jeweilige Woche : wie viel verkäufe  = 300
sales :: Int -> Int
sales 0 = 7
sales 1 = 20
sales 2 = 50
sales 3 = 100
sales 4 = 105
sales 5 = 150
sales 6 = 110
sales 7 = 111

-- Alle Verkäufe
totalSales :: Int -> Int 
totalSales 0 = sales 0
totalSales n = sales n + totalSales(n-1)



-- Maximale Verkäe bis zur eingegebene Wochennummer liefert
maxSaless :: Int -> Int
maxSaless 0 = 0
maxSaless w = if sales w <= maxSaless (w-1) then maxSaless (w-1)
            else w

maxSales :: Int -> Int
maxSales 0 = 0
maxSales n | sales (maxSales (n-1)) >= sales n = maxSales (n-1)
           | otherwise = n

maxSalesss 0 = 0
maxSalesss w = if sales w <= sales (maxSalesss(w-1)) then maxSalesss (w-1)
                else w


---------------- 


-- Verwenden Sie die Funktion foldr, um die Funktion map' zu definieren:
-- map' :: (a -> b) -> [a] -> [b]
-- map' _ [ ] = [ ]
-- map' f (x:xs) = f x : map' f xs

mapp' :: (a -> b) -> [a] -> [b]
mapp' _ [ ] = [ ]
mapp' f (x:xs) = f x : mapp' f xs

mappp' f liste = foldr (\x xs -> f x : xs) [] liste

removeWhitespace2 :: String -> String
removeWhitespace2 str = [ c | c <- str, c /= ' ' ]

----------------------------- 

-- a) Implementieren Sie eine rekursive Funktion take_while, die ein Prädikat und eine Liste als
-- Parameter hat und das längste Anfangsstück der Liste zurückgibt, dessen Elemente alle das
-- Prädikat erfüllen.
-- Zum Beispiel soll der Aufruf take_While (\x -> x < 3) [1,2,1,1,4,2,5] zu [1,2,1,1] ausgewertet
-- werden.
-- b) Schreiben Sie das Programm mittels foldr um.

tak_while p [] = []
tak_while p liste = foldr (\x xs -> if p x then x:xs else []) [] liste 

reTake p [] = []
reTake p (x:xs) | p x = x : reTake p xs
                | otherwise = reTake p xs

retake p liste = foldr f [] liste 
            where   
                f x xs = if (p x) then (:) x xs else []

----

take_whilee p [] = []
take_whilee p (x:xs) | p x = x : take_whilee p xs   
                     | otherwise = take_whilee p xs

take_whileFoldr p liste = foldr f [] liste
                        where
                            f x xs = if (p x) then (:) x xs else []

take_whileFoldrr p liste = foldr (\x xs -> if (p x) then x:xs else []) [] liste

--- 

helpme _ n [] = n 
helpme f n (x:xs) = f x (helpme f n xs)

take_while':: (a -> Bool) -> [a] -> [a]
take_while' f l = foldr (\x xs -> if (f x)then x:xs else []) [] l

-- Aufgabe 3: Schreiben Sie ein Programm, das alle Leerzeichen aus einem Text löscht und den ersten
-- Buchstaben jedes Wortes in einen Großbuchstaben umwandelt (bzw. einen Kameltext erstellen).
-- z.B. wäre der Text „Das ist ein TEXT“, soll die Ausgabe der Funktion „DasIstEinText“ sein.
-- Hinweis:
-- - Eine Zeichenkette vom Typ String ist eine Liste von Zeichen (Also [Char]).
-- z.B. “abc“ und [‘a‘,‘b‘,‘c‘] sind äquivalent.
-- - Importieren Sie Data.Char in Ihrem Programm. Für die Manipulation von Zeichen stehen
-- die folgenden Funktionen in der Bibliothek Data.Char zur Verfügung:
-- isLower, isUpper, toLower, toUpper, …
-- - Sie können die Funktion words verwenden:
-- words :: String -> [String]
-- - Üben Sie mit “map” und “foldr”.


-- 1 Funktion die Leere Zeichen entfernt
    -- "abc de fg" -> ['a','b','c',' ','d','e',' ','f','g'] -> abcdefg
-- entferneSpace " " = "+"


toWords w = words w

exm f w = foldr (\x xs -> if (f x) then x:xs else []) [] w 

-- exm p w = foldr f [] w
--                     where
--                         f x xs = if (p x) then (:) x xs else []

ltowords w = length (toWords w)


uppercaseword xs = map (\x -> (toUpper x)) xs


--entferneSpace satz = foldr (\x xs -> if (f x) then x:xs else []) [] satz


what [] = "empty string!"
what (c:_)
        | isUpper c = "upper case!"
        | isLower c = "lower case"
        | otherwise = "not a letter!"

filterw [] = []
filterw (x:xs) 
            | f x = x : (filterw xs)
            | otherwise = filterw xs
            where
                f x = isUpper x


camelTexttt :: String -> String
camelTexttt str = concatMap removeSpaces (map capitalizeWord (words str))
  where
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs


-- >>> unwords ["Lorem", "ipsum", "dolor"]
-- "Lorem ipsum dolor"

-- >>> words "Lorem ipsum\ndolor"
-- ["Lorem","ipsum","dolor"]

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')


-- Lists Comprehesion
removeWhitespace :: String -> String
removeWhitespace str = [ c | c <- str, c /= ' ' ]

camelT :: String -> String
camelT str = unwords (map capitalizeWord (words str))
  where
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs


myString str = removeWhitespace (camelT str) 


camelTextt :: String -> String
camelTextt str = unwords (map capitalizeWord (words str))
  where
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs

myString2 str = removeWhitespace (camelTextt str) 

--------------

processText :: String -> String
processText str = foldr (\x acc -> if x == ' ' then acc else x : acc) "" (unwords (map capitalizeWord (reje33houmkalimatffarray (lowerkoulchi str))))
  where
    capitalizeWord :: String -> String
    capitalizeWord [] = []
    capitalizeWord (x:xs) = toUpper x : xs


-- foldr hat 3 Argumente, lambda Funktion, empty String, result of fun
-- foldr führt die funktion Lambda auf jeder Char in der str (from input string)
-- empty String ist der Initiale Akkumulator 
-- in each Itteration the same, und am Ende ist der Resultat in der Akkumulator gespeichert 


-- processTT str = foldr (lambda function) "" str 
-- lambda :  will applied of each element
-- "" : Akkumulator da wo das Ergebnis gespeichert wird und wird auch als Start Value für die Akumulator" 
-- str : der Eingabe String str wird vorbereitet :
    -- -> alles klein gemacht (map toLower) 
    -- -> zu einem Array umwandeln (toWords) 
    -- -> auf jeder Element in der List wird durch map capitalizeW größer geschrieben 
    -- -> Array zurück zum String umwandeln 

processTT str = foldr (\x acc -> if x == ' ' then acc else x : acc) "" (unwords (map capitalizeWord (toWords(map toLower str))))
        where
            capitalizeWord [] = []
            capitalizeWord (x:xs) = toUpper x : xs

--processTTT str = foldr (\x acc -> if x == ' ' then acc else x : acc) "" (toWords(map toLower str))


--------------------------

checkParentheses :: String -> Bool
checkParentheses text = checkParentheses' text []
  where
    checkParentheses' :: String -> String -> Bool
    checkParentheses' [] [] = True
    checkParentheses' [] _  = False
    checkParentheses' (x:xs) stack
      | x == '('  = checkParentheses' xs (x:stack)
      | x == ')'  = case stack of
                      []      -> False
                      (y:ys)  -> if y == '(' then checkParentheses' xs ys else False
      | otherwise = checkParentheses' xs stack



checkParenthesess :: String -> Bool
checkParenthesess text = checkParentheses' text 0
  where
    checkParentheses' :: String -> Int -> Bool
    checkParentheses' [] count = count == 0
    checkParentheses' (x:xs) count
      | x == '('  = checkParentheses' xs (count + 1)
      | x == ')'  = if count > 0 then checkParentheses' xs (count - 1) else False
      | otherwise = checkParentheses' xs count


checkPa text = checkPa' text 0
    where 
        checkPa'[] count = count == 0
        checkPa' (x:xs) count 
            | x == '(' = checkPa' xs (count + 1)
            | x == ')' = if count > 0 then checkPa' xs (count -1) else False
            | otherwise = checkPa' xs count


--------------------

data Zahl = Eins | Zwei | Drei
  deriving (Show)

plus Eins Eins = Zwei
plus Zwei Eins = Drei

-- f :: [Zahl] -> Zahl -> Zahl
-- f xs y = if xs == [] then y else plus x y
--  where 
--   x :: Zahl
--   x = head xs


type Vorname = String
type Nachname = String
type Note = Double
liste :: [(Vorname, Nachname, Note)]
liste = [("zouh","pla",0.9),("blabalibadou","blabalibouda",1.0)]

numberofstudents = length liste

numberofstudent liste = numberofstudents' liste 0
                    where
                      numberofstudents' [] count = count
                      numberofstudents' (x:xs) count = numberofstudents' xs (count+1)




-- Definieren Sie einen Typ für Geometrische Figuren, der Rechtecke, Kreise und Dreiecke repräsentiert. Jedes Rechteck hat eine Länge
--  und eine Breite, jeder Kreis hat einen Radius und jedes Dreieck hat drei Seitenlängen.

data Figur = Rechteck Float Float | Kreis Float | Dreieck Float Float Float 

-- Definieren Sie eine Funktion, die die Fläche einer geometrischen Figur berechnet.

flaeche :: Figur -> Float
flaeche (Rechteck w h) = w * h
flaeche (Kreis r) = (3.14) * (r**2) 
flaeche (Dreieck a b c) =  let s = (a + b + c) / 2 
                           in sqrt(s * (s - a) * (s - b) * (s - c))

--Definieren Sie eine Funktion, die überprüft, ob eine geometrische Figur ein Quadrat ist.

quadrat (Rechteck w h) = w == h
quadrat (Kreis r) = False
quadrat (Dreieck a b c ) = a == b && b == c

-- Definieren Sie einen Typ für Musikalben, der Alben von verschiedenen Künstlern und verschiedenen Genres repräsentiert.
-- Jedes Album hat einen Titel, einen Künstler und ein Genre.

data Alben = Titel String | Kuenstler String | Genre String

data Album = Album String String String



--data Waehrung = Euro String Float | Dollar String Float | Pfund String Float

data Waehrung = Waehrung String Float

--Definieren Sie eine Funktion, die den Namen einer Währung in ihre Zeichenkette-Representation konvertiert.

showWaehrung1 w = (\x y -> (x)) 

showWaehrung (Waehrung euro kurs) = euro


mywords = ["zouh","houh", "ijsda", "ksad", "eur"]

-- Liste comprehenssion : [Ausdruck | Generato, filter1, filter2..]
-- Ausdruck : berechnung auf die enzelne Elemente
-- Generator : legt fets welche Elemente aufgenommen werden
-- filter : schliesst bestimmte Elemente aus.
myF mywords= [w | w <- mywords, elem (head w) ['h','e'], length w >= 2]


ymF = [(x,y) | x <- [1..5], y <- [1..5], x `mod` 2 == 0]


----------------------

data Point = Point Float Float deriving Show

makePoint x y = Point x y

--sqrt(((x2-x1)`pow`2)+((y2-y1)`pow`2))
entfernung (Point x1 y1) (Point x2 y2) = sqrt(((x2-x1)^2)+((y2-y1)^2))

-- Funktion zum Erstellen einer Liste von Punkten
listvonpunkten (Point x1 y1) = [[] | p <- , filter]


-- Schreiben Sie eine Funktion evenList, die eine Liste von Ganzzahlen als Eingabe annimmt und eine Liste von Booleans zurückgibt
-- die angibt, ob jede Zahl in der Eingabeliste gerade ist. Zum Beispiel sollte evenList [2, 4, 6] [True, True, True] und
-- evenList [1, 3, 5] [False, False, False] zurückgeben.

evenListe liste = map (\x -> x`mod`2 == 0 ) liste

-----------------

factorialList :: [Int] -> [Int]
factorialList liste = map(\x -> fact x) liste
                  where
                    fact 0 = 1
                    fact n = n * fact(n-1)

----------------

zippit [] _ = []
zippit _ [] = []
zippit (x:xs) (y:ys) = (x,y) : zippit xs ys 

zippitmap xs ys = map (\(x,y)->(x,y)) (zippitmap xs ys) 


zippitfoldr xs ys = foldr f [] (zipp xs ys)
              where 
                zipp [] _ = []
                zipp _ [] = []
                zipp (x:xs) (y:ys) = (x,y) : zipp xs ys
                
                f (a, b) acc = (a, b) : acc 

--------------------- 

data Form = Rechteckk Float Float | Kreiss Float 
              deriving (Show, Eq)

myrechteckk = Rechteckk 5 6

mykreiss = Kreiss 3

--wasForm k = if (k == Kreiss) then "das ist ein Kreis" else "nooo"
--wasForm (Rechteckk a b) = "das ist ein Rechteck"

flaeschee (Rechteckk x y) = x * y
flaeschee (Kreiss r) = (3.14) * r^2

data MyListe = Leer | Element Int MyListe deriving Show

--data MListe a = Leer | Element a (MListe a) deriving Show

langeListe Leer = 0
langeListe (Element _ xs) = 1 + langeListe xs

--MyListe a = (Element 1 (Element 2 (Element 3 Leer)))

liste3 :: List String
liste3 = Cons "ha" (Cons "llo" Nil)

myList :: List Int
myList = Cons 1 (Cons 2 (Cons 3 Nil))



data List a = Nil | Cons a (List a) deriving Show

liste4 :: List Wochentag
liste4 = Cons Montag (Cons Dienstag (Cons Mittwoch Nil))

listToWord l = words l 


showmylist Nil = ""
showmylist (Cons x xs)  = showmylist xs

langee l = langee' l 0
          where
            langee' Nil akk = akk
            langee' (Cons x xs) akk  = langee' xs akk + 1

langeee Nil = 0
langeee (Cons x xs)  = 1 + langeee xs 


data Wochentag = Montag
 | Dienstag
 | Mittwoch
 | Donnerstag
 | Freitag
 | Samstag
 | Sonntag deriving (Show, Eq)

-- Rekursiv Data Deklaration : neuer Typ 
--data Treee a = Leaf a | Node (Treee a) a (Treee a) deriving Show

myTree = Node (Leaf 1) 0 (Node (Leaf 4) 5 (Leaf 6))

--wie Tief eine Baum ist
langeTree t = langeTree' t 0
          where 
            langeTree' (Leaf leaf) akk = akk + 1 
            langeTree' (Node right _ left) akk = if langeTree right > langeTree left then langeTree' right (akk+1) else langeTree' left (akk+1)

---------------

createTree :: [a] -> Tree a
createTree [] = error "Leerer Baum"
createTree [x] = Leaf x
createTree xs = Node (createTree left) root (createTree right)
  where
    left = take (length xs `div` 2) xs
    root = xs !! (length xs `div` 2)
    right = drop (length xs `div` 2 + 1) xs


--------------------

-- Definieren Sie einen Typ für Bäume.

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

myTree_1 =  Node (Leaf 0) 1 (Node (Leaf 2) 3 (Leaf 4))
myTree_2 = Node (Leaf 0) 1 (Node (Node (Leaf 10) 11 (Leaf 111)) 3 (Leaf 4))

lenghtOfTree t = lenghtOfTree' t 0
            where 
              lenghtOfTree' (Leaf l) akk = akk + 1
              lenghtOfTree' (Node left x right) akk = if lenghtOfTree left > lenghtOfTree right then lenghtOfTree' left (akk + 1) 
                                                      else lenghtOfTree' right (akk +1)

-- Definieren Sie einen Typ für Binärbäume.

data BinaryTree a = Empty | Nodee a (BinaryTree a) (BinaryTree a)
    deriving (Show)

myBinTree_2 = Nodee 10 (Nodee 5 (Nodee 6 (Nodee 7 (Nodee 8 (Empty) (Empty)) (Nodee 10 (Empty) (Empty))) (Empty)) (Empty)) Empty

tiefBinTree :: BinaryTree a -> Int
tiefBinTree bintree = tiefBinTree' bintree 1
                  where 
                    tiefBinTree' Empty akk = akk
                    tiefBinTree' (Nodee x left right) akk = if tiefBinTree left > tiefBinTree right then tiefBinTree' left (akk+1)
                                                           else tiefBinTree' right (akk+1) 
tiefBin :: BinaryTree a -> Int
tiefBin Empty = 1
tiefBin (Nodee x left right) = 1 + (maxi (tiefBin left)(tiefBin right))
                            where
                              maxi x y = if x > y then x else y

tiefBinFoldr :: BinaryTree a -> Int
tiefBinFoldr bt = foldr (maxi) 0 (map f [bt])
              where 
                f Empty = 1
                f (Nodee x left right) = 1 + maxi (tiefBinFoldr left) (tiefBinFoldr right)

                maxi x y = if x > y then x else y

c) Definieren Sie eine Funktion, die kontrolliert, ob eine eingegebene Zahl in dem Baum (bzw.
Binärbaum) existiert.

serchInBt :: BinaryTree a -> Int
serchInBt bt x = foldr (\bt -> x) (r == x) (map f [bt])
              where 
                f Empty x = False
                f (Nodee a left right) x = if x /= a then serchInBt left else serchInBt right

searchInBt' Empty x = False
searchInBt' (Node a left right) x = if x /= a then searchInBt' left else searchInBt' right


-- die "foldr" Funktion ist eine höhere Funktion in haskell
-- die "foldr" Funktion ist verwendet um eine Liste von Werten in einen einzelnen Wert zu reduzieren
-- das geschieht wenn die eingegebene Funktion auf jeder Element und den bishergen Wert angewendt wird

-- die "map" funktion ist auch eine höhere Funktion in Haskell
-- die "map" Funktion ist verwendt auf jeder Element der Liste 

myFoldr (x:xs) = foldr (+) 0 xs

-- foldr (ausdruck) (generator / akku) (rest der Liste)
reversList liste = foldr (\x xs -> xs ++ [x]) [] liste

-- Schreiben Sie eine Funktion, die eine Liste von Integer-Werten nimmt und die Summe aller Werte in der Liste berechnet. Verwenden Sie foldr für die Implementierung.
summeliste liste = foldr (+) 0 liste 

-- Schreiben Sie eine Funktion, die eine Liste von String-Werten nimmt und eine neue Liste von String-Werten zurückgibt, bei der jeder String in der Eingabeliste in
-- Großbuchstaben umgewandelt wurde. Verwenden Sie map für die Implementierung.
listeToGross liste = map ((map(\y -> toUpper y))) liste

-- Schreiben Sie eine Funktion, die eine Liste von Listen von Integer-Werten nimmt und eine neue Liste von Integer-Werten zurückgibt, bei der alle Werte in den Listen 
-- in der Eingabeliste addiert werden. Verwenden Sie sowohl map als auch foldr für die Implementierung.
listeadd liste = map (foldr (+) 0) liste
-- d) Definieren Sie eine Funktion, die einen Binärbaum in einer Liste konvertiert.
-- e) Definieren Sie eine Funktion, die eine sortierte Liste in einem Binärbaum konvertiert.




