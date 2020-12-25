module Part3 where

import Data.List.NonEmpty (group, toList)
import Data.List (group, nub, sort, find)
import Data.Bool (bool)
------------------------------------------------------------
-- PROBLEM #18
--
-- Проверить, является ли число N простым (1 <= N <= 10^9)
prob18 :: Integer -> Bool
prob18 1 = False
prob18 m = isPrime m 2
  where
    isPrime :: Integer -> Integer -> Bool
    isPrime m i
      | i * i > m = True
      | m `rem` i == 0 = False
      | otherwise = isPrime m (i + 1)

------------------------------------------------------------
-- PROBLEM #19
--
-- Вернуть список всех простых делителей и их степеней в
-- разложении числа N (1 <= N <= 10^9). Простые делители
-- должны быть расположены по возрастанию
prob19 :: Integer -> [(Integer, Int)]
prob19 x = map (\d -> (d, factorize d x)) (primeDivisors x)

primes :: [Integer]
primes = 2 : filter isPrime [3, 5 ..]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n = all (\p -> n `mod` p /= 0) (takeWhile (\p -> p * p <= n) primes)

primeDivisors :: Integer -> [Integer]
primeDivisors x = filter isPrime (divisors x)

factorize :: Integer -> Integer -> Int
factorize divisor number
  | number `mod` divisor == 0 = 1 + factorize divisor (number `div` divisor)
  | otherwise = 0

------------------------------------------------------------
-- PROBLEM #20
--
-- Проверить, является ли число N совершенным (1<=N<=10^10)
-- Совершенное число равно сумме своих делителей (меньших
-- самого числа)
prob20 :: Integer -> Bool
prob20 number = sum ((init . divisors) number) == number

divisors :: Integer -> [Integer]
divisors number = divisorsFrom 1 number
	where
		divisorsFrom :: Integer -> Integer -> [Integer]
		divisorsFrom minDivisor number
			| minDivisor * minDivisor > number = []
			| minDivisor * minDivisor == number = [minDivisor]
			| number `rem` minDivisor == 0
				= [minDivisor] ++ divisorsFrom (succ minDivisor) number ++ [number `div` minDivisor]
			| otherwise = divisorsFrom (succ minDivisor) number

------------------------------------------------------------
-- PROBLEM #21
--
-- Вернуть список всех делителей числа N (1<=N<=10^10) в
-- порядке возрастания
prob21 :: Integer -> [Integer]
prob21 n = quicksort (divisors n)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

------------------------------------------------------------
-- PROBLEM #22
--
-- Подсчитать произведение количеств букв i в словах из
-- заданной строки (списка символов)
prob22 :: String -> Integer
prob22 = toInteger . product . map lettersCount . words
	where
		lettersCount :: String -> Int
		lettersCount = length . filter (== 'i')

------------------------------------------------------------
-- PROBLEM #23
--
-- На вход подаётся строка вида "N-M: W", где N и M - целые
-- числа, а W - строка. Вернуть символы с N-го по M-й из W,
-- если и N и M не больше длины строки. Гарантируется, что
-- M > 0 и N > 0. Если M > N, то вернуть символы из W в
-- обратном порядке. Нумерация символов с единицы.
prob23 :: String -> Maybe String
prob23 input = let
		(startIndex, afterStartIndex) = head (reads input :: [(Int, String)])
		(endIndex, afterEndIndex) = head (reads (drop 1 afterStartIndex) :: [(Int, String)])
		string = drop 2 afterEndIndex
		leftIndex = min startIndex endIndex
		rightIndex = max startIndex endIndex
	in if leftIndex <= length string && rightIndex <= length string
		then let
				order = if startIndex <= endIndex then (\ x -> x) else reverse
			in Just $ (order . take (rightIndex - leftIndex + 1) . drop (leftIndex - 1)) string
		else Nothing

------------------------------------------------------------
-- PROBLEM #24
--
-- Проверить, что число N - треугольное, т.е. его можно
-- представить как сумму чисел от 1 до какого-то K
-- (1 <= N <= 10^10)
prob24 :: Integer -> Bool
prob24 number = iterateTriangle 1 0
    where
        iterateTriangle :: Integer -> Integer -> Bool
        iterateTriangle currentNum currentSum
            | currentSum == number = True
            | currentSum > number = False
            | otherwise = iterateTriangle (succ currentNum) (currentSum + currentNum)


------------------------------------------------------------
-- PROBLEM #25
--
-- Проверить, что запись числа является палиндромом (т.е.
-- читается одинаково слева направо и справа налево)
prob25 :: Integer -> Bool
prob25 number = digits 10 number == reverse (digits 10 number)
	where
		digits :: Integer -> Integer -> [Integer]
		digits base number
			| number < base = [number]
			| otherwise   = number `rem` base : digits base (number `div` base)

------------------------------------------------------------
-- PROBLEM #26
--
-- Проверить, что два переданных числа - дружественные, т.е.
-- сумма делителей одного (без учёта самого числа) равна
-- другому, и наоборот
prob26 :: Integer -> Integer -> Bool
prob26 x y = sum (divider x) == y && sum (divider y) == x
  where
    divider :: Integer -> [Integer]
    divider n = [a | a <- [1 .. (n -1)], n `rem` a == 0]

------------------------------------------------------------
-- PROBLEM #27
--
-- Найти в списке два числа, сумма которых равна заданному.
-- Длина списка не превосходит 500
prob27 :: Int -> [Int] -> Maybe (Int, Int)
prob27 _ [] = Nothing
prob27 requiredSum (curHead : curTail) = withFixedCurrent curHead curTail
    where
        withFixedCurrent :: Int -> [Int] -> Maybe (Int, Int)
        withFixedCurrent _ [] = prob27 requiredSum curTail
        withFixedCurrent current (innerHead : innerTail) =
            if current + innerHead == requiredSum
            then Just (current, innerHead)
            else withFixedCurrent current innerTail

------------------------------------------------------------
-- PROBLEM #28
--
-- Найти в списке четыре числа, сумма которых равна
-- заданному.
-- Длина списка не превосходит 500
prob28 :: Int -> [Int] -> Maybe (Int, Int, Int, Int)
prob28 requiredSum inputList = do
    list <- find
            (\list -> sum list == requiredSum)
            $ subsets 4 inputList
    return (list !! 3, list !! 2, list !! 1, list !! 0)
    where
        subsets :: Int -> [a] -> [[a]]
        subsets subLength listToHandle =
            if subLength > length listToHandle
            then []
            else subsequencesBySize listToHandle !! (length listToHandle - subLength)

        subsequencesBySize [] = [[[]]]
        subsequencesBySize (curHead : curTail) =
            let next = subsequencesBySize curTail
            in zipWith (++) ([] : next) (map (map (curHead :)) next ++ [[]])

------------------------------------------------------------
-- PROBLEM #29
--
-- Найти наибольшее число-палиндром, которое является
-- произведением двух K-значных (1 <= K <= 3)
prob29 :: Int -> Int
prob29 1 = 9
prob29 2 = 9009
prob29 3 = 906609

prob29 k = fromInteger (maximum (filter prob25 ([x * y | x <- range, y <- range])))
 where
    range = [10^k - 1, 10^k - 2..10^(k-1)]

------------------------------------------------------------
-- PROBLEM #30
--
-- Найти наименьшее треугольное число, у которого не меньше
-- заданного количества делителей
prob30 :: Int -> Integer
prob30 count = minTriangular 1 2
	where
		minTriangular :: Integer -> Integer -> Integer
		minTriangular triangularNumber number
			| (length . divisors) triangularNumber >= count = triangularNumber
			| otherwise = minTriangular (triangularNumber + number) (succ number)

------------------------------------------------------------
-- PROBLEM #31
--
-- Найти сумму всех пар различных дружественных чисел,
-- меньших заданного N (1 <= N <= 10000)
prob31 :: Int -> Int
prob31 n = sum [x + y |x <- [1 .. n],y <- [x+1 .. n], prob26 (toInteger x) (toInteger y)]

------------------------------------------------------------
-- PROBLEM #32
--
-- В функцию передаётся список достоинств монет и сумма.
-- Вернуть список всех способов набрать эту сумму монетами
-- указанного достоинства
-- Сумма не превосходит 100
prob32 :: [Int] -> Int -> [[Int]]
prob32 coins coinsSum
	| coinsSum < minimum coins = []
	| otherwise = [coin : nextCoins |
		coin <- reverse coins,
		nextCoins <- [] : prob32 (filter (<= coin) coins) (coinsSum - coin),
		sum (coin : nextCoins) == coinsSum]
