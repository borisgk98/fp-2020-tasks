{-# LANGUAGE MultiWayIf #-}

module Part2 where

import Part2.Types

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'


------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 (Red x) = x >= 0 && x <= 255
prob7 (Green x) = x >= 0 && x <= 255
prob7 (Blue x) = x >= 0 && x <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 (Color r g b) (Red x) = (Color (r + x) g b)
prob8 (Color r g b) (Green x) = (Color r (g + x) b)
prob8 (Color r g b) (Blue x) = (Color r g (b + x))

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 (Red x) = x
prob9 (Green x) = x
prob9 (Blue x) = x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color r g b) = if | r > g && r > b -> Just . Red $ r
                          | g > r && g > b -> Just . Green $ g
                          | b > r && b > g -> Just . Blue $ b
                          | otherwise -> Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
instance Functor Tree where
  fmap f (Tree l x r) = (Tree (fmap (fmap f) l) (f x) (fmap (fmap f) l))

prob11 :: Num a => Tree a -> a
prob11 (Tree Nothing x Nothing) = x
prob11 (Tree (Just l) x Nothing) = (prob11 l) + x
prob11 (Tree Nothing x (Just r)) = x + (prob11 r)
prob11 (Tree (Just l) x (Just r)) = (prob11 l) + x + (prob11 r)

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12_f :: Ord a => Tree a -> (a, a, Bool)
prob12_f (Tree Nothing x Nothing) = (x, x, True)
prob12_f (Tree (Just l) x Nothing) = let (mn, mx, b) = prob12_f l
                                     in
                                     if | b == False -> (min mn x, max mx x, False)
                                        | mx >= x -> (min mn x, mx, False)
                                        | otherwise -> (mn, x, True)
prob12_f (Tree Nothing x (Just r)) = let (mn, mx, b) = prob12_f r
                                     in
                                     if | b == False -> (min mn x, max mx x, False)
                                        | mn < x -> (mn, max x mx, False)
                                        | otherwise -> (x, mx, True)
prob12_f (Tree (Just l) x (Just r)) = let (mnr, mxr, br) = prob12_f r
                                          (mnl, mxl, bl) = prob12_f l
                                      in
                                      if | br == False || bl == False -> (min (min mnl mnr) x, max (max mxr mxl) x, False)
                                         | x <= mxl -> (min (min mnl mnr) x, max (max mxr mxl) x, False)
                                         | x > mnr -> (min (min mnl mnr) x, max (max mxr mxl) x, False)
                                         | otherwise -> (min (min mnl mnr) x, max (max mxr mxl) x, True)

prob12 :: Ord a => Tree a -> Bool
prob12 t = let (_, _, b) = prob12_f t
           in
           b

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
