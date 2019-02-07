module Ficha1 where
import Data.List

--EX1
--a)
{-
 - f x = 2 * x
 - g x = x + 1
 -
 - (f . g) x = 2 * (x + 1) = 2x + 2
 -
 - ########################################################
 - f = succ
 - g x = 2 * x
 -
 - (f . g) x = 2*x + 1
 -
 - ########################################################
 - f = succ
 - g = length
 -
 - (f . g) x = succ . length $ x
 -
 - ########################################################
 - g (x,y) = x + y
 - f = succ . (2*)
 -
 - (f . g) (x,y) = succ . (2*) $ (x + y) = 1 + 2 * (x + y)
-}

--b)
-- tem a ver com os monoids, nao interessa a ordem (i guess)

--c)
-- função id = identidade, devolve o próprio input, logo é indiferente onde é usado

--EX2
length' :: [a] -> Int
length' = foldr (\x acc -> succ acc) 0

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

--EX3
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (h:t) = case h of Nothing -> catMaybes t
                            Just a  -> a : catMaybes t

--EX4
uncurry' :: (a->b->c) -> (a,b) -> c
uncurry' f (x,y) = f x y

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

flip' :: (a->b->c) -> b -> a -> c
flip' f y x = f x y

--EX5
data LTree a = Leaf a | Fork (LTree a, LTree a)

flatten :: LTree a -> [a]
flatten (Leaf a) = [a]
flatten (Fork (l,r)) = flatten l ++ flatten r

mirror :: LTree a -> LTree a
mirror (Fork (l,r)) = Fork (mirror l, mirror r)
mirror x = x

fmap' :: (b->a) -> LTree b -> LTree a
fmap' f (Leaf b) = Leaf $ f b
fmap' f (Fork (l,r)) = Fork (fmap' f l, fmap' f r)

--EX6
--a) exercicio 2

--b) f = foldr : []
-- apenas reescreve a lista, f l == id l

--EX7
concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ concat' t

concat'' :: [[a]] -> [a]
concat'' = foldl (\acc x -> acc ++ x) []

--EX8
-- filtra e incrementa uma unidade a todos os números maiores do que 0
-- e.g. f [-1,1,-2,2] == [2,3]

f :: [Int] -> [Int]
f = foldr (\x acc -> if x > 0 then succ x : acc else acc) []

--EX9
--m == map
m :: (a->b) -> [a] -> [b]
m f [] = []
m f (h:t) = f h : m f t

--a)
m' :: (a->b) -> [a] -> [b]
m' f l = foldr (\x acc -> f x : acc) [] l

--b)
m'' :: (a->b) -> [a] -> [b]
m'' f l = map f l

--c)
-- f :: a -> [a]
-- f a = [a]
-- função que recebe uma lista de a's e tranforma numa lista de listas de a's
-- e.g. f [1,2,3] == [[1],[2],[3]]

--d) A função descrita acima faz o contrário da função contrário, logo concat . f $ l == id l
