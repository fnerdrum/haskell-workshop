module ListOperations where

-- Implementer filter' som tar inn et predikat og en liste og
-- returnerer en liste der kun elementene som oppfyller predikatet er
-- til stede
-- Eks: filter' (>0) [1,2,-1,-2] = [1,2]

filter' :: (a -> Bool) -> [a] -> [a]
filter' pred xs = undefined


-- Implementer map' som tar inn en funksjon og en liste og returnerer
-- en ny liste der alle elementene er blitt
-- transformert med funksjonen
-- Eks: map' (+1) [1,2,3] = [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f xs = undefined


-- Implementer head' som returnerer det første elementet i listen
-- Eks: head' [1,2,3] = 1

head' :: [a] -> a
head' xs = undefined


-- Implementer tail' som returnerer alle elementene bortsett fra det
-- første
-- Eks: tail' [1,2,3] = [2,3]

tail' :: [a] -> [a]
tail' xs = undefined


-- Implementer sum' som returnerer summen av alle elementene i listen
-- Eks: sum' [1,2,3] = 6

sum' :: Num a => [a] -> a
sum' xs = undefined


-- Implementer repeat' som tar inn et element og returnerer en
-- uendelig lang liste med dette elementet
-- Eks: repeat' 1 = [1,1..]

repeat' :: a -> [a]
repeat' x = undefined


-- Implementer take' som returnerer de n første elementene i listen.
-- Hvis listen ikke er lang nok returneres bare hele listen
-- Eks: take' 3 [1,2,3,4,5] = [1,2,3]
-- Eks: take' 5 [1,2,3] = [1,2,3]

take' :: Int -> [a] -> [a]
take' n xs = undefined


-- Implementer zipWith' som tar inn en funksjon og to lister. Listene
-- blir slått sammen til en ny liste ved å kombindere elementene på
-- samme indeks ved hjelp av funksjonen. Lengden av listen bestemmes
-- av lengden til den korteste listen
-- Eks: zipWith' (+) [1,2,3] [1,2,3,4,5] = [2,4,6]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = undefined


-- Implementer zip' som slår sammen to lister ved a lage et par av
-- elementene på samme indeks.
-- Eks: zip' [1,2,3] [4,5,6] = [(1,4),(2,5),(3,6)]
-- Hint: Prøv å bruke zipWith' som du implementerte forrige oppgave

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = undefined


-- Implementer en vanlig left fold som tar inn en funksjon, en
-- akkumulator og en liste
-- Eks: fold (+) 0 [1,2,3] = 6

fold :: (b -> a -> b) -> b -> [a] -> b
fold f acc xs = undefined


-- Ved å bruke fold som du implementerte forrige oppgave, lag reverse'
-- som reverserer alle elementene i listen
-- Eks: reverse' [1,2,3] = [3,2,1]

reverse' :: [a] -> [a]
reverse' xs = undefined


-- Ved å bruke fold, lag maximum' som returnerer det største elementet
-- i listen
-- Eks: maximum' [1,2,3] = 3

maximum' :: Ord a => [a] -> a
maximum' xs = undefined
