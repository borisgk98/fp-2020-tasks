{-# LANGUAGE MultiWayIf #-}

module Part3 where

------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 2 = True
prob18 n = (filter ((==0) . (mod n)) [2..((floor . sqrt . fromIntegral $ n) + 1)]) == []

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию

-- Принемает n и (k, 0), возращает (k, сколько раз n делиться на k)
prob19_div :: Integer -> (Integer, Int) -> (Integer, Int)
prob19_div n (x, k) = if | n < x -> (x, k)
                         | n `mod` x == 0 -> prob19_div (n `div` x) (x, k + 1)
                         | otherwise -> (x, k)

-- Принемает n и k, возращает (k, сколько раз n делиться на k)
prob19_g :: Integer -> Integer -> (Integer, Int)
prob19_g n x = prob19_div n (x, 0)

-- Принемает n и k, возращает n, поделенное максимально на k
prob19_k :: Integer -> Integer -> Integer
prob19_k n k = if n `mod` k == 0 then prob19_k (n `div` k) k else n

-- Разложение n на простые множители (запуск - prob19_f n 2 [])
prob19_f :: Integer -> Integer -> [Integer] -> [Integer]
prob19_f 1 _ a = a
prob19_f n k a = if | k > ((floor . sqrt . fromIntegral $ n) + 1) -> n : a
                    | n `mod` k == 0 -> prob19_f (prob19_k n k) (k + 1) (k : a)
                    | otherwise -> prob19_f n (k + 1) a

prob19_r :: Integer -> Integer -> [(Integer, Int)] -> [(Integer, Int)]
prob19_r 1 _ a = a
prob19_r n x a = if | n `mod` x /= 0 -> prob19_r n (x + 1) a
                    | a == [] -> prob19_r (n `div` x) x [(x, 1)]
                    | (==x) . fst . head $ a -> prob19_r (n `div` x) x ((x, (+1) . snd . head $ a) : (tail a))
                    | prob18 n -> (n, 1) : a
                    | otherwise -> prob19_r n (x + 1) a

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

prob19 :: Integer -> [(Integer, Int)]
prob19 n = reverseList ((prob19_g n) <$> (prob19_f n 2 []))

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20_dsum :: Integer -> Integer
prob20_dsum 1 = 0
prob20_dsum n = (foldr (+) 1 ((\x -> x + n `div` x) <$> (filter ((==0) . (mod n)) [2..(floor . sqrt . fromIntegral $ n)])))

prob20 :: Integer -> Bool
prob20 n = n == (prob20_dsum n)

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21_merge :: Eq a => [a] -> [a] -> [a]
prob21_merge []     ys     = ys
prob21_merge (x:xs) ys = x : prob21_merge xs ys

prob21 :: Integer -> [Integer]
prob21 n = if n == (floor . sqrt . fromIntegral $ n) * (floor . sqrt . fromIntegral $ n)
          then prob21_merge (filter ((==0) . (mod n)) [1..((floor . sqrt . fromIntegral $ n) - 1)]) (reverseList ((\x -> n `div` x) <$> (filter ((==0) . (mod n)) [1..(floor . sqrt . fromIntegral $ n)])))
          else prob21_merge (filter ((==0) . (mod n)) [1..(floor . sqrt . fromIntegral $ n)]) (reverseList ((\x -> n `div` x) <$> (filter ((==0) . (mod n)) [1..(floor . sqrt . fromIntegral $ n)])))

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22_split_f :: String -> Char -> [String] -> [String]
prob22_split_f [] _ curr = curr
prob22_split_f s pref curr =  let ch = head s
                              in
                              if | ch == ' '  -> prob22_split_f (tail s) ch curr
                                 | pref == ' ' -> prob22_split_f (tail s) ch ([head s] : curr)
                                 | otherwise -> prob22_split_f (tail s) ch ((ch : (head curr)) : (tail curr))

prob22_split :: String -> [String]
prob22_split s = prob22_split_f s ' ' []

prob22 :: String -> Integer
prob22 s = foldr (*) 1 (fmap (toInteger . length . (filter (=='i'))) (prob22_split s))

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 n = let d = 1 + 8 * n
               sd = floor . sqrt . fromIntegral $ d
           in
           sd * sd == d

------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 x = (show x) == (reverse . show $ x)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 x y = (prob20_dsum x) == (y) && (x) == (prob20_dsum y)

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 k = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 = error "Implement me!"
