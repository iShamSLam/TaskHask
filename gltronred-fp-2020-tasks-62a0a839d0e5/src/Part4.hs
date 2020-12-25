{-# LANGUAGE InstanceSigs #-}

module Part4 where

-- | 8 tasks:
-- * Functor for Parser
-- * Applicative for Parser
-- * Monad for Parser
-- * Alternative for Parser
-- * Functor for Cont
-- * Applicative for Cont
-- * Monad for Cont
-- * ??? for Cont

import Part4.Types

import Control.Applicative
import Control.Monad (msum)
import Data.Maybe (maybeToList)
import Data.Char (intToDigit, isSpace, isDigit)
import Text.Read (readMaybe)
import Data.List (dropWhileEnd)

------------------------------------------------------------
-- PROBLEM #33
--
-- Написать экземпляр класса Functor для Parser
-- (удовлетворяющий законам)
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap func (Parser parseFunc) = Parser $ (map (\(str, input) -> (str, func input))) . parseFunc

------------------------------------------------------------
-- PROBLEM #34
--
-- Написать экземпляр класса Applicative для Parser
-- (удовлетворяющий законам)
instance Applicative Parser where
    pure :: a -> Parser a
    pure value = Parser $ \str -> [(str, value)]

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser leftFunc) <*> (Parser rightFunc) = Parser parserFunc
        where
            parserFunc input = do
                (leftString, funcToApply) <- (leftFunc input)
                (rightString, item) <- (rightFunc leftString)
                return (rightString, funcToApply item)

------------------------------------------------------------
-- PROBLEM #35
--
-- Написать экземпляр класса Alternative для Parser
-- (удовлетворяющий законам)
instance Alternative Parser where

    empty :: Parser a
    empty = Parser $ \_ -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser left) <|> (Parser right) = Parser $ \str -> msum $ map ($ str) [left, right]

------------------------------------------------------------
-- PROBLEM #36
--
-- Написать экземпляр класса Monad для Parser
-- (удовлетворяющий законам)
instance Monad Parser where

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (Parser parserFunc) >>= funcToBind = Parser resultFunc
        where
            resultFunc input = do
                (stringRem, item) <- parserFunc input
                runParser (funcToBind item) stringRem

------------------------------------------------------------
-- PROBLEM #37
--
-- Написать экземпляр класса Functor для Foo
-- (удовлетворяющий законам)

instance Functor (Foo r) where

    fmap :: (a -> b) -> Foo r a -> Foo r b
    fmap = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #38
--
-- Написать экземпляр класса Applicative для Foo
-- (удовлетворяющий законам)
instance Applicative (Foo r) where

    pure :: a -> Foo r a
    pure = error "Implement me!"

    (<*>) :: Foo r (a -> b) -> Foo r a -> Foo r b
    (<*>) = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #39
--
-- Написать экземпляр класса Monad для Foo
-- (удовлетворяющий законам)
instance Monad (Foo r) where

    (>>=) :: Foo r a -> (a -> Foo r b) -> Foo r b
    (>>=) = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #40
--
-- Написать парсер для выражений вида
--
--   <var> := <number>
--
-- Здесь вместо <var> может быть произвольное имя
-- переменной (начинается со строчной буквы, состоит из
-- букв, цифр и знаков подчеркивания), вместо <number> -
-- произвольное целое число. Между именем переменной и
-- присваиванием ":=" и между присваиванием ":=" и числом
-- может быть нуль или больше пробелов
--
-- В качестве результата парсер должен вернуть пару
-- (имя переменной, присваиваемое число)
prob40 :: Parser (String, Integer)
prob40 = (,) <$> variableNameParser <*> variableValueParser

-- Парсер, вычленяющий имя переменной из выражения присвоения.
variableNameParser :: Parser String
variableNameParser = Parser parseFunc
    where
        parseFunc :: String -> [(String, String)]
        parseFunc assignmentExpr = do
            (nameInput, _) <- maybeToList $ trySplitByAssignmentOperator assignmentExpr
            True <- return $ isValidVariableName nameInput
            return (assignmentExpr, nameInput)

-- Является ли строка валидным именем переменной.
isValidVariableName :: String -> Bool
isValidVariableName [] = False
isValidVariableName (firstChar : nameTail) = elem firstChar ['a'..'z'] && all isValidChar nameTail
    where
        isValidChar :: Char -> Bool
        isValidChar = flip elem $ concat
            [
                ['a'..'z'],
                ['A'..'Z'],
                map intToDigit [0..9],
                ['_']
            ]

-- Парсер, вычленяющий значение переменной из выражения присвоения.
variableValueParser :: Parser Integer
variableValueParser = Parser parseFunc
    where
        parseFunc :: String -> [(String, Integer)]
        parseFunc assignmentExpr = do
            (_, numberInput) <- maybeToList $ trySplitByAssignmentOperator assignmentExpr

            -- Проверяем, что между ':=' и числом нет невалидных символов.
            [] <- return $ takeWhile (\char -> (not $ isDigit char) &&  char /= '-' && (not $ isSpace char)) numberInput

            -- Проверяем, что справа от ':=' есть хотя бы одна цифра.
            True <- return $ any isDigit numberInput

            -- Проверяем, что между ':=' и числом находится не более одного минуса.
            True <- return $ 
                (length $ filter (=='-')
                (takeWhile (not . isDigit) numberInput)) <= 1

            -- Проверяем, что справа от минусов и пробелов находится цифра.
            True <- return $ case (dropWhile (\char -> char == '-' || isSpace char) numberInput) of
                (x : _) -> isDigit x
                _ -> False

            return $ case readMaybe numberInput of
                Just validInteger -> ("", validInteger)
                Nothing -> (takeInvalidTail numberInput, 0)

-- Получить список невалидных символов, расположенных в конце выражения (справа от числа).
takeInvalidTail :: String -> String
takeInvalidTail = takeInvalid . trimStart
    where
        takeInvalid ('-' : digitTail) = dropWhile isDigit digitTail
        takeInvalid numberString = dropWhile isDigit numberString

-- Разбить строку на две подстроки, расположенные
-- соответственно слева и справа от самого левого оператора присвоения ':='.
trySplitByAssignmentOperator :: String -> Maybe (String, String)
trySplitByAssignmentOperator input
    | hasSingleOperator = Just
        (
            trim $ getNameString input,
            getValueString input
        )
    | otherwise = Nothing
    where
        hasSingleOperator = length (filter (==(':','=')) (pairwise input)) == 1

        -- Получить строку, содержащую имя переменной,
        -- опираясь на самый левый оператор ':='.
        getNameString :: String -> String
        getNameString = safeInit . unpairwise . takeWhile (/=(':','=')) . pairwise

        safeInit :: [a] -> [a]
        safeInit []   = []
        safeInit list = init list

        -- Получить строку, содержащую значение переменной,
        -- опираясь на самый левый оператор ':='.
        getValueString :: String -> String
        getValueString = safeTail . unpairwise . tail . dropWhile (/=(':','=')) . pairwise

        safeTail :: [a] -> [a]
        safeTail []   = []
        safeTail list = tail list

-- Удалить пробелы, находящиеся в начале и в конце строки.
trim :: String -> String
trim = trimStart . trimEnd

-- Удалить пробелы, находящиеся в начале строки.
trimStart :: String -> String
trimStart = dropWhile isSpace

-- Удалить пробелы, находящиеся в конце строки.
trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- Преобразовать список в список пар - соседних элементов.
pairwise :: [a] -> [(a, a)]
pairwise []       = []
pairwise [_]      = []
pairwise (x : xs) = (x, head xs) : pairwise xs

-- Вернуть разбитый на пары список к первоначальному виду.
unpairwise :: [(a, a)] -> [a]
unpairwise []       = []
unpairwise [(a, b)] = [a, b]
unpairwise (x : xs) = fst x : unpairwise xs
