module Part2 where

import Part2.Types

import Data.Function ((&))
import Data.List (find)
import Control.Monad (msum)

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 colorLetter = case colorLetter of
    RED   -> 'R'
    GREEN -> 'G'
    BLUE  -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 colorPart = asInt >= 0 && asInt <= 255
    where asInt = prob9 colorPart

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color colorPart = case colorPart of
    Red   redValue   -> color { red   = (color & red)   + redValue   }
    Green greenValue -> color { green = (color & green) + greenValue }
    Blue  blueValue  -> color { blue  = (color & blue)  + blueValue  }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 colorPart = case colorPart of
    Red int   -> int
    Green int -> int
    Blue int  -> int

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color
    | length getMaxValues > 1 = Nothing
    | otherwise = find (\part -> prob9 part == maximum valuesList) colorsList
    where
        colorsList =
            [
                Red   $ color & red,
                Green $ color & green,
                Blue  $ color & blue
            ]
        valuesList = map prob9 colorsList
        getMaxValues = filter (\part -> prob9 part == maximum valuesList) colorsList

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = leftSum + (tree & root) + rightSum
    where
        leftSum = maybe 0 prob11 $ tree & left
        rightSum = maybe 0 prob11 $ tree & right

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = and
    [
        leftIsSearchTree,
        leftValueIsLess,
        rightValueIsMoreOrEqual,
        rightIsSearchTree
    ]
    where
        leftIsSearchTree  = maybe True prob12 $ tree & left
        rightIsSearchTree = maybe True prob12 $ tree & right

        leftValueIsLess = maybe True
            (\leftSubTree -> (leftSubTree & root) < (tree & root))
            $ tree & left

        rightValueIsMoreOrEqual = maybe True
            (\rightSubTree -> (rightSubTree & root) >= (tree & root))
            $ tree & right

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 value tree
    | value == (tree & root) = Just tree
    | otherwise = msum
        [
            do
                leftSubTree <- tree & left
                prob13 value leftSubTree,
            do
                rightSubTree <- tree & right
                prob13 value rightSubTree
        ]

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 unitTree = traverseTree (getNodesCount unitTree) unitTree
    where
        traverseTree :: Int -> Tree () -> Tree Int
        traverseTree nodeNumber tree = Tree
            (do
                leftSubTree <- tree & left
                return $ traverseTree (pred nodeNumber) leftSubTree)
            nodeNumber
            (do
                rightSubTree <- tree & right
                return $ traverseTree (getRightDecrementFunc tree nodeNumber) rightSubTree)

        getRightDecrementFunc :: Tree a -> (Int -> Int)
        getRightDecrementFunc tree = case tree & left of
            Just leftSubTree -> subtract (getNodesCount leftSubTree + 1)
            Nothing -> pred

        getNodesCount :: Tree a -> Int
        getNodesCount tree = succ $ sum
            [
                maybe 0 getNodesCount (tree & left),
                maybe 0 getNodesCount (tree & right)
            ]

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня:
-- 4
--  \          6
--   6   =>   / \
--    \      4   8
--     8
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree leftRotation $ tree & right
    where
        leftRotation rightSubTree = rightSubTree { left = Just oldRoot }
            where
                oldRoot = tree { right = rightSubTree & left }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня:
--     8
--    /        6
--   6   =>   / \
--  /        4   8
-- 4
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rightRotation $ tree & left
    where
        rightRotation leftSubTree = leftSubTree { right = Just oldRoot }
            where
                oldRoot = tree { left = leftSubTree & right }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 tree
    | isBalanced tree = tree
    | otherwise = (prob17 . performRotations . handleSubTrees) tree
    where
        -- Выполнить рекурсивный вызов балансировки на левом и правом поддеревьях.
        handleSubTrees :: Tree a -> Tree a
        handleSubTrees currentTree = currentTree
            {
                left = do
                    leftSubTree <- currentTree & left
                    return $ prob17 leftSubTree,
                right = do
                    rightSubTree <- currentTree & right
                    return $ prob17 rightSubTree
            }

        -- Выполнить вращение дерева относительно текущего корня в зависимости от
        -- разности высот поддеревьев (LL, LR, RR, RL).
        performRotations :: Tree a -> Tree a
        performRotations currentTree
            | isBalanced currentTree = currentTree

            | getHeight (currentTree & left) - getHeight (currentTree & right) > 1 =
                if getHeight (currentTree & left >>= left) > getHeight (currentTree & left >>= right)
                then prob16 currentTree
                else leftRightRotation currentTree

            | otherwise =
                if getHeight (currentTree & right >>= left) > getHeight (currentTree & right >>= right)
                then rightLeftRotation currentTree
                else prob15 currentTree

-- Сбалансировано ли дерево.
isBalanced :: Tree a -> Bool
isBalanced tree =
    abs (getHeight (tree & left) - getHeight (tree & right)) <= 1
    && maybe True isBalanced (tree & left)
    && maybe True isBalanced (tree & right)

-- Получить высоту дерева.
getHeight :: Maybe (Tree a) -> Integer
getHeight Nothing = 0
getHeight (Just tree) = succ $ max
    (getHeight $ tree & left)
    (getHeight $ tree & right)

-- Выполнить большее правое (RL) вращение дерева.
-- 4       4
--  \       \          6
--   8  =>   6   =>   / \
--  /         \      4   8
-- 6           8
rightLeftRotation :: Tree a -> Tree a
rightLeftRotation tree = prob15 $ tree 
    { 
        right = do 
            rightSubTree <- tree & right
            return $ prob16 rightSubTree
    }

-- Выполнить большое левое (LR) вращение дерева.
--   8         8
--  /         /        6
-- 4    =>   6   =>   / \
--  \       /        4   8
--   6     4
leftRightRotation :: Tree a -> Tree a
leftRightRotation tree = prob16 $ tree
    {
        left = do
            leftSubTree <- tree & left
            return $ prob15 leftSubTree
    }
