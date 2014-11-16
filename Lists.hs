module Lists where

-- Lag en funksjon som tar inn en liste og returnerer en ny liste som
-- kun inneholder elementene i den originale listen som er positive.
-- Elementene må være i samme rekkefølge

positive xs = filter (>0) xs

-- Lag en funksjon som tar inn en liste lister og returner en flat
-- liste ved å beholde rekkefølgen ev elementene
-- Ex: flatten [[1,2],[3],[4,5,6]]  = [1,2,3,4,5,6]

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs


-- Lag en funksjon som tar inn et predikat og en liste og returnerer en ny liste der
-- kun de elemente som IKKE oppfyller predikatet finnes

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot pred list = filter (not . pred) list
