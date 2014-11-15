module Exercices where

true = True

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x:(filter' f xs)
    | otherwise = filter' f xs

positive :: Num a => Ord a => [a] -> [a]
positive = filter (>0)

not' :: Bool -> Bool
not' True = False
not' False = True

filterNot :: (a -> Bool) -> [a] -> [a]
filterNot cond = filter' (not . cond)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x):(map' f xs)

head' :: [a] -> a
head' (x:xs) = x

tail' :: [a] -> [a]
tail' (x:xs) = xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

repeat' :: a -> [a]
repeat' x = x:(repeat' x)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x:(take' (n-1) xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = zipWith' (\ x y -> (x,y)) xs ys

index :: [a] -> [(Int,a)]
index xs = zip [1,2..] xs

fold :: (b -> a -> b) -> b -> [a] -> b
fold _ acc [] = acc
fold f acc (x:xs) = fold f (f acc x) xs

sum'' :: Num a => [a] -> a
sum'' xs = fold (+) 0 xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys) = if (x == y) then True else elem' x ys

reverse' :: [a] -> [a]
reverse' xs = fold (\acc x -> x:acc) [] xs

maximum' :: Ord a => [a] -> a
maximum' xs = fold (\acc x -> if (x > acc) then x else acc) (head' xs) xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' cond (x:xs)
    | cond x = x : takeWhile cond xs
    | otherwise = []
