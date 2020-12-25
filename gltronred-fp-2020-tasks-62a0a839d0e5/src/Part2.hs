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
prob7 c = prob9 c >= 0 && prob9 c <= 255

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 c n = case n of
    Red x -> c {red = red c + x}
    Green x -> c {green = green c + x}
    Blue x -> c {blue = blue c + x}

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 c = case c of
  Red x -> x
  Green x -> x
  Blue x -> x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 c | red c > green c && red c > blue c = Just  (Red (red c))
         | green c > red c && green c > blue c = Just  (Green (green c))
         | blue c > red c && blue c > green c = Just  (Blue (blue c))
prob10 c = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 t = sum (toList t)

toList :: Tree a -> [a]
toList tree = [root tree] ++ values (right tree) ++ values (left tree)
    where 
        values (Just a) = toList a
        values Nothing = []

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
