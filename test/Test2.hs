module Test2 (tests2) where

import Test.Tasty
import Test.Tasty.HUnit

import Part2.Types
import Part2

tests2 :: [TestTree]
tests2 =
  [ test6
  , test7
  , test8
  , test9
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
  , test16
  , test17
  ]

test6 :: TestTree
test6 = testGroup "P06"
  [ testCase "prob6 RED == 'R'" $ prob6 RED @?= 'R' ]

test7 :: TestTree
test7 = testGroup "P07"
  [ testCase "prob7 (Red 300) == F" $ prob7 (Red 300) @?= False
  , testCase "prob7 (Red 10)  == T" $ prob7 (Red 10)  @?= True
  ]

test8 :: TestTree
test8 = testGroup "P08"
  [ testCase "prob8 (100,100,100) (Red 10) == (110,100,100)" $
    prob8 (Color 100 100 100) (Red 10) @?= Color 110 100 100
  , testCase "prob8 (200,150,100) (Green -50) == (200,100,100)" $
    prob8 (Color 200 150 100) (Green (-50)) @?= Color 200 100 100
  ]

test9 :: TestTree
test9 = testGroup "P09"
  [ testCase "prob9 (Red 100) == 100" $ prob9 (Red 100) @?= 100
  , testCase "prob9 (Blue 50) == 50"  $ prob9 (Blue 50) @?= 50
  ]

test10 :: TestTree
test10 = testGroup "P10"
  [ testCase "prob10 (100,50,0) == Just (R 100)" $
    prob10 (Color 100 50 0) @?= Just (Red 100)
  , testCase "prob10 (100,100,100) == Nothing" $
    prob10 (Color 100 100 100) @?= Nothing
  , testCase "prob 10 (10, 100, 10) == Just (G 100)" $
    prob10 (Color 10 100 10) @?= Just (Green 100)
  ]

-- (1 2 (3 4 (5 6 nil)))
--   2
--  / \
-- 1   4
--    / \
--   3   6
--      /
--     5
tree1 :: Tree Int
tree1 = Tree (Just $ Tree Nothing 1 Nothing)
             2
             (Just $ Tree (Just $ Tree Nothing 3 Nothing)
                          4
                          (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                                  6
                                  Nothing))

-- ((1 2 3) 4 (5 6 nil))
--      4
--     / \
--    /   \
--   2     6
--  / \   /
-- 1   3 5
tree2 :: Tree Int
tree2 = Tree (Just $ Tree (Just $ Tree Nothing 1 Nothing)
                          2
                          (Just $ Tree Nothing 3 Nothing))
             4
             (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                          6
                          Nothing)

-- (2 3 1)
--   3
--  / \
-- 2   1
tree3 :: Tree Int
tree3 = Tree (Just $ Tree Nothing 2 Nothing) 3 (Just $ Tree Nothing 1 Nothing)

-- (x x x)
--   X
--  / \
-- X   X
tree4 :: Tree ()
tree4 = Tree (Just $ Tree Nothing () Nothing)
             ()
             (Just $ Tree Nothing () Nothing)

-- ((x x nil) x nil)
--     X
--    /
--   X
--  /
-- X
tree5 :: Tree ()
tree5 = Tree (Just $ Tree (Just $ Tree Nothing () Nothing)
                          ()
                          Nothing)
             ()
             Nothing

-- (nil 4 (6 8 nil))
-- 4
--  \
--   8
--  /
-- 6
tree6 :: Tree Int
tree6 = Tree
    Nothing
    4
    (Just $ Tree
        (Just $ Tree Nothing 6 Nothing)
        8
        Nothing)

-- ((nil 4 6) 8 nil)
--   8
--  /
-- 4
--  \
--   6
tree7 :: Tree Int
tree7 = Tree
    (Just $ Tree
        Nothing
        4
        (Just $ Tree Nothing 6 Nothing))
    8
    Nothing

-- (4 6 8)
--   6
--  / \
-- 4   8
tree8 :: Tree Int
tree8 = Tree
    (Just $ Tree Nothing 4 Nothing)
    6
    (Just $ Tree Nothing 8 Nothing)

-- (x x (x x (x x nil)))
--   X
--  / \
-- X   X
--    / \
--   X   X
--      /
--     X
tree9 :: Tree ()
tree9 = Tree 
    (Just $ Tree Nothing () Nothing)
    ()
    (Just $ Tree 
        (Just $ Tree Nothing () Nothing)
        ()
        (Just $ Tree 
            (Just $ Tree Nothing () Nothing)
            ()
            Nothing))

-- (5 6 (3 4 (1 2 nil)))
--   6
--  / \
-- 5   4
--    / \
--   3   2
--      /
--     1
tree10 :: Tree Int
tree10 = Tree 
    (Just $ Tree Nothing 5 Nothing)
    6
    (Just $ Tree 
        (Just $ Tree Nothing 3 Nothing)
        4
        (Just $ Tree 
            (Just $ Tree Nothing 1 Nothing)
            2
            Nothing))

-- (nil 1 (nil 2 (nil 3 (nil 4 5))))
-- 1
--  \
--   2
--    \
--     3
--      \
--       4
--        \
--         5
tree11 :: Tree Int
tree11 = Tree
    Nothing
    1
    (Just $ Tree
        Nothing
        2
        (Just $ Tree
            Nothing
            3
            (Just $ Tree
                Nothing
                4
                (Just $ Tree
                    Nothing
                    5
                    Nothing))))

--       6
--      / \
--     4   7
--    / \
--   2   5
--  / \
-- 1   3
tree12 :: Tree Int
tree12 = Tree
    (Just $ Tree
        (Just $ Tree
            (Just $ Tree Nothing 1 Nothing)
            2
            (Just $ Tree Nothing 3 Nothing))
        4
        (Just $ Tree Nothing 5 Nothing))
    6
    (Just $ Tree Nothing 7 Nothing)

-- ((1 2 (nil 3 4)) 5 6)
--     5
--    / \
--   2   6
--  / \
-- 1   3
--      \
--       4
tree13 :: Tree Int
tree13 = Tree
    (Just $ Tree
        (Just $ Tree Nothing 1 Nothing)
        2
        (Just $ Tree
            Nothing
            3
            (Just $ Tree Nothing 4 Nothing)))
    5
    (Just $ Tree Nothing 6 Nothing)

--     3
--   /   \
-- 0       5
--  \     / \
--   1   4   6
--    \
--     2
tree14 :: Tree Int
tree14 = Tree
    (Just $ Tree
        Nothing
        0
        (Just $ Tree
            Nothing
            1
            (Just $ Tree Nothing 2 Nothing)))
    3
    (Just $ Tree
        (Just $ Tree Nothing 4 Nothing)
        5
        (Just $ Tree Nothing 6 Nothing))

-- ((1 2 nil) 3 nil)
--     3
--    /
--   2
--  /
-- 1
tree15 :: Tree Int
tree15 = Tree 
    (Just $ Tree 
        (Just $ Tree Nothing 1 Nothing)
        2
        Nothing)
    3
    Nothing

-- ((x x x) x (x x x))
--      X
--    /   \
--   X     X
--  / \   / \
-- X   X X   X
tree16 :: Tree ()
tree16 = Tree
    (Just $ Tree
        (Just $ Tree Nothing () Nothing)
        ()
        (Just $ Tree Nothing () Nothing))
    ()
    (Just $ Tree
        (Just $ Tree Nothing () Nothing)
        ()
        (Just $ Tree Nothing () Nothing))

-- ((5 6 4) 7 (2 3 1))
--      7
--    /   \
--   6     3
--  / \   / \
-- 5   4 2   1
tree17 :: Tree Int
tree17 = Tree
    (Just $ Tree
        (Just $ Tree Nothing 5 Nothing)
        6
        (Just $ Tree Nothing 4 Nothing))
    7
    (Just $ Tree
        (Just $ Tree Nothing 2 Nothing)
        3
        (Just $ Tree Nothing 1 Nothing))

-- (nil 0 (nil 1 (nil 2 (nil 3 (nil 4 (nil 5 (nil 6)))))))
-- 0
--  \
--   1
--    \
--     2
--      \
--       3
--        \
--         4
--          \
--           5
--            \
--             6
tree18 :: Tree Int
tree18 = Tree
    Nothing
    0
    (Just $ Tree
        Nothing
        1
        (Just $ Tree
            Nothing
            2
            (Just $ Tree
                Nothing
                3
                (Just $ Tree
                    Nothing
                    4
                    (Just $ Tree
                        Nothing
                        5
                        (Just $ Tree
                            Nothing
                            6
                            Nothing))))))

-- ((nil 1 (2 3 nil)) 4 5)
--   4
--  / \
-- 1   5
--  \
--   3
--  /
-- 2
tree19 :: Tree Int
tree19 = Tree
    (Just $ Tree
        Nothing
        1
        (Just $ Tree
            (Just $ Tree Nothing 2 Nothing)
            3
            Nothing))
    4
    (Just $ Tree  Nothing 5 Nothing)

--     3
--    / \
--   2   4
--  /     \
-- 1       5
tree20 :: Tree Int
tree20 = Tree
    (Just $ Tree
        (Just $ Tree Nothing 1 Nothing)
        2
        Nothing)
    3
    (Just $ Tree
        Nothing
        4
        (Just $ Tree Nothing 5 Nothing))

--         7
--        /
--       6
--      /
--     3
--    / \
--   2   4
--  /     \
-- 1       5
tree22 :: Tree Int
tree22 = Tree
    (Just $ Tree
        (Just tree20)
        6
        Nothing)
    7
    Nothing

-- -1
--  \
--   0
--    \
--     3
--    / \
--   2   4
--  /     \
-- 1       5
tree23 :: Tree Int
tree23 = Tree
    Nothing
     (-1)
     (Just $ Tree
        Nothing
        0
        (Just tree20))


-- 1
--  \
--   2
--    \
--     3
--      \
--       4
--        \
--         5
--          \
--           6
--            \
--             7
--              \
--               8
tree24:: Tree Int
tree24 = Tree
    Nothing
    1
    (Just $ Tree
        Nothing
        2
        (Just $ Tree
            Nothing
            3
            (Just $ Tree
                Nothing
                4
                (Just $ Tree
                    Nothing
                    5
                    (Just $ Tree
                        Nothing
                        6
                        (Just $ Tree
                            Nothing
                            7
                            (Just $ Tree
                                Nothing
                                8
                                Nothing)))))))

test11 :: TestTree
test11 = testGroup "P11"
  [ testCase "prob11 (1 2 (3 4 (5 6 nil))) == 21" $ prob11 tree1 @?= 21
  , testCase "prob11 ((1 2 3) 4 (5 6 nil)) == 21" $ prob11 tree2 @?= 21
  , testCase "prob11 (3 1 2) == 6" $ prob11 tree3 @?= 6
  ]

test12 :: TestTree
test12 = testGroup "P12"
  [ testCase "prob12 (1 2 (3 4 (5 6 nil))) == T" $ prob12 tree1 @?= True
  , testCase "prob12 ((1 2 3) 4 (5 6 nil)) == T" $ prob12 tree2 @?= True
  , testCase "prob12 (2 3 1) == F" $ prob12 tree3 @?= False
  , testCase "prob12 (nil 4 (6 8 nil)) == T" $ prob12 tree6 @?= True
  , testCase "prob12 ((nil 4 6) 8 nil) == T" $ prob12 tree7 @?= True
  , testCase "prob12 tree19 == T" $ prob12 tree19 @?= True
  , testCase "prob12 tree23 == T" $ prob12 tree23 @?= True
  , testCase "prob12 tree24 == T" $ prob12 tree24 @?= True
  ]

test13 :: TestTree
test13 = testGroup "P13"
  [ testCase "prob13 6 (1 2 (3 4 (5 6 nil))) == Just (5 6 nil)" $
    prob13 6 tree1 @?= Just (Tree (Just $ Tree Nothing 5 Nothing) 6 Nothing)
  , testCase "prob13 7 ((1 2 3) 4 (5 6 nil)) == Nothing" $
    prob13 7 tree2 @?= Nothing
  ]

test14 :: TestTree
test14 = testGroup "P14"
  [ testCase "prob14 (x x x) == (2 3 1)" $
    prob14 tree4 @?= tree3
  , testCase "prob14 (nil x (x x x)) == (nil 4 (2 3 1))" $
    prob14 (Tree Nothing () (Just tree4)) @?= Tree Nothing 4 (Just tree3)
  , testCase "prob14 (x x (x x (x x nil))) == (5 6 (3 4 (1 2 nil)))" $
    prob14 tree9 @?= tree10
  , testCase "prob14 ((x x nil) x nil) == ((1 2 nil) 3 nil)" $
    prob14 tree5 @?= tree15
  , testCase "prob14 (nil) == (1)" $
    prob14 (Tree Nothing () Nothing) @?= Tree Nothing 1 Nothing
  , testCase "prob14 ((x x x) x (x x x)) == ((5 6 4) 7 (2 3 1))" $
    prob14 tree16 @?= tree17
  ]

test15 :: TestTree
test15 = testGroup "P15"
  [ testCase "prob15 (1 2 (3 4 (5 6 nil))) == ((1 2 3) 4 (5 6 nil))" $
    prob15 tree1 @?= tree2
  , testCase "prob15 (x x x) == ((x x nil) x nil)" $
    prob15 tree4 @?= tree5
  ]

test16 :: TestTree
test16 = testGroup "P16"
  [ testCase "prob16 ((1 2 3) 4 (5 6 nil)) == (1 2 (3 4 (5 6 nil)))" $
    prob16 tree2 @?= tree1
  , testCase "prob16 ((x x nil) x nil) == (x x x)" $
    prob16 tree5 @?= tree4
  ]

test17 :: TestTree
test17 = testGroup "P17"
  [ testCase "prob17 (1 2 (3 4 (5 6 nil)))" $
    isBalancedSearchTree (prob17 tree1) @?= True
  , testCase "prob17 ((x x nil) x nil) == (x x x)" $
    prob17 tree5 @?= tree4
  , testCase "prob17 (nil 1 (nil 2 (nil 3 (nil 4 5))))" $
    isBalancedSearchTree (prob17 tree11) @?= True
  , testCase "prob17 ((1 2 (nil 3 4)) 5 6)" $
    isBalancedSearchTree (prob17 tree13) @?= True
  , testCase "prob17 (nil 0 (nil 1 (nil 2 (nil 3 (nil 4 (nil 5 (nil 6)))))))" $
    isBalancedSearchTree (prob17 tree18) @?= True
  , testCase "prob17 ((nil 1 (2 3 nil)) 4 5)" $
    isBalancedSearchTree (prob17 tree19) @?= True
  , testCase "prob17 (nil 4 (6 8 nil)) == (4 6 8)" $
    prob17 tree6 @?= tree8
  , testCase "prob17 tree14" $
    isBalancedSearchTree (prob17 tree14) @?= True
  , testCase "prob17 tree12" $
    isBalancedSearchTree (prob17 tree12) @?= True
  , testCase "prob17 tree22" $
    isBalancedSearchTree (prob17 tree22) @?= True
  , testCase "prob17 tree23" $
    isBalancedSearchTree (prob17 tree23) @?= True
  , testCase "prob17 tree24" $
    isBalancedSearchTree (prob17 tree24) @?= True

  , testCase "prob17-RL (nil 4 (6 8 nil)) == (4 6 8)" $
    rightLeftRotation tree6 @?= tree8
  , testCase "prob17-LR ((nil 4 6) 8 nil) == (4 6 8)" $
    leftRightRotation tree7 @?= tree8

  , testCase "prob17-isBalanced (1 2 (3 4 (5 6 nil))) == False" $
    isBalanced tree1 @?= False
  , testCase "prob17-isBalanced ((1 2 3) 4 (5 6 nil)) == True" $
    isBalanced tree2 @?= True
  , testCase "prob17-isBalanced tree19" $
    isBalanced tree19 @?= False
  , testCase "prob17-isBalanced tree23" $
    isBalanced tree24 @?= False

  , testCase "prob17-getHeight (1 2 (3 4 (5 6 nil))) == 4" $
    getHeight (Just tree1) @?= 4
  , testCase "prob17-getHeight (nil 4 (6 8 nil)) == 3" $
    getHeight (Just tree6) @?= 3
  , testCase "prob17-getHeight (nil) == 0" $
    getHeight Nothing @?= 0
  , testCase "prob17-getHeight tree19 == 4" $
    getHeight (Just tree19) @?= 4
  , testCase "prob17-getHeight tree23 == 8" $
    getHeight (Just tree24) @?= 8
  ]

-- Является ли дерево сбалансированным деревом поиска.
isBalancedSearchTree :: Ord a => Tree a -> Bool
isBalancedSearchTree tree = prob12 tree && isBalanced tree
