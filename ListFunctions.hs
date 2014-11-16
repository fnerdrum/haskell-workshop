module ListFunctions where

-- Lag en funksjon som tar inn en liste og returnerer en ny liste som
-- kun inneholder elementene i den originale listen som er positive.
-- Elementene må være i samme rekkefølge

positive = filter (>0)


-- Lag en funksjon som tar inn en liste av lister og returner en flat
-- liste ved å beholde rekkefølgen ev elementene
-- Eks: flatten [[1,2],[3],[4,5,6]]  = [1,2,3,4,5,6]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs


-- Lag en funksjon som tar inn et predikat og en liste og returnerer en ny liste der
-- kun de elemente som IKKE oppfyller predikatet finnes

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred = filter (not . pred)


-- Lag en funksjon som tar inn et element og en liste, og returnerer
-- True hvis listen inneholder elementet

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if (x == y) then True else elem' x ys


-- Lag en funksjon som tar inn et predikat og en liste og som
-- plukker elementer fra listen så lenge predikatet er sant
-- Eks: takeWhile' (>0) [1,2,3,0,4,5] = [1,2,3]

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' cond (x:xs)
    | cond x = x : takeWhile' cond xs
    | otherwise = []


-- Lag en funksjon som tar inn en liste og indekserer alle elementene
-- i listen
-- Eks: index ['a','b','c'] = [(1,'a'),(2,'b'),(3,'c')]

index :: [a] -> [(Int,a)]
index xs = zip [1..] xs
