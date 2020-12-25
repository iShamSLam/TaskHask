module Test4 (tests4) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.Hedgehog as HH
import Test.Tasty.HUnit

import Control.Applicative
import Data.Char

import Part4.Types
import Part4

tests4 :: [TestTree]
tests4 =
  [ test33
  , test34
  , test35
  , test36
  , test37
  , test38
  , test39
  , test40
  ]

test33 :: TestTree
test33 = testGroup "P33" $ let
  toDigit c = ord c - ord '0'
  in [ testCase "fmap id" $
       parse (id <$> digitP) "1" @?= Right '1'
     , testCase "fmap f . fmap g" $
       parse (toDigit <$> digitP) "1" @?= Right 1
     , testCase "fmap id can't parse" $
       parse (id <$> digitP) "not-a-char" @?= Left "Can't parse"
     , testCase "fmap id fail" $
       parse (id <$> digitP) "8not-a-char" @?= Left "Leftover: not-a-char"
     ]

test34 :: TestTree
test34 = testGroup "P34"
  [ testCase "<*> (,)" $
    parse ((,) <$> digitP <*> digitP) "12" @?= Right ('1','2')
  , testCase "<*> (,,)" $
    parse ((,,) <$> digitP <*> digitP <*> digitP) "123" @?= Right ('1','2','3')
  , testCase "<*> (,,) failure" $
    parse ((,,) <$> digitP <*> digitP <*> digitP) "12A" @?= Left "Can't parse"
  , testCase "pure" $
    parse (pure 123) "12" @?= Left "Leftover: 12"
  ]

test35 :: TestTree
test35 = testGroup "P35"
  [ testCase "<|>" $
    parse (many digitP) "123" @?= Right "123"
  ]

test36 :: TestTree
test36 = testGroup "P36"
  [ testCase ">>= 1" $
    parse p "12" @?= Right '2'
  , testCase ">>= 2" $
    parse p "23" @?= Right '2'
  ]
  where
    p :: Parser Char
    p = do
          x <- anyCharP
          y <- anyCharP
          case x of
            '1' -> pure y
            '2' -> pure x

test37 :: TestTree
test37 = testGroup "P37" []

test38 :: TestTree
test38 = testGroup "P38" []

test39 :: TestTree
test39 = testGroup "P39" []

test40 :: TestTree
test40 = testGroup "P40"
  [ testCase "trySplitByAssignmentOperator \"varName_1:=123\"" $
    trySplitByAssignmentOperator "varName_1:=123" @?= Just ("varName_1", "123")
  , testCase "trySplitByAssignmentOperator \":=123\"" $
    trySplitByAssignmentOperator ":=123" @?= Just ("", "123")
  , testCase "trySplitByAssignmentOperator \" := \"" $
    trySplitByAssignmentOperator " := " @?= Just ("", " ")
  , testCase "trySplitByAssignmentOperator \" : = \"" $
    trySplitByAssignmentOperator " : = " @?= Nothing
  , testCase "trySplitByAssignmentOperator \":==\"" $
    trySplitByAssignmentOperator ":==" @?= Just ("", "=")
  , testCase "trySplitByAssignmentOperator \"varName := --1\"" $
    trySplitByAssignmentOperator "varName := --1" @?= Just ("varName", " --1")
  , testCase "trySplitByAssignmentOperator \"var:Na:=me =\"" $
    trySplitByAssignmentOperator "var:Na:=me =" @?= Just ("var:Na", "me =")

  , testCase "isValidVariableName \"varName_1\"" $
    isValidVariableName "varName_1" @?= True
  , testCase "isValidVariableName \"a___\"" $
    isValidVariableName "a___" @?= True

  , testCase "variableValueParser \"123\"" $
    parse variableValueParser ":= 123" @?= Right 123
  , testCase "variableValueParser \"--1\"" $
    parse variableValueParser ":= --1" @?= Left "Can't parse"
  , testCase "variableValueParser \"--1-\"" $
    parse variableValueParser ":= --1-" @?= Left "Can't parse"
  , testCase "variableValueParser \"-1-\"" $
    parse variableValueParser ":= -1-" @?= Left "Leftover: -"
  , testCase "variableValueParser \"-\"" $
    parse variableValueParser ":= -" @?= Left "Can't parse"
  , testCase "variableValueParser \"1-1\"" $
    parse variableValueParser ":= 1-1" @?= Left "Leftover: -1"
  , testCase "variableValueParser \"123 -1-\"" $
    parse variableValueParser ":= 123 -1-" @?= Left "Leftover:  -1-"
  , testCase "variableValueParser \"123 123\"" $
    parse variableValueParser ":= 123 123" @?= Left "Leftover:  123"    
  , testCase "variableValueParser \"-.123\"" $
    parse variableValueParser ":= -.123" @?= Left "Can't parse"
  , testCase "variableValueParser \"-.123\"" $
    parse variableValueParser ":= -. 123" @?= Left "Can't parse"
  , testCase "variableValueParser \".123\"" $
    parse variableValueParser ":= .123" @?= Left "Can't parse"
  , testCase "variableValueParser \"-1.23\"" $
    parse variableValueParser ":= -1.23" @?= Left "Leftover: .23"

  , testCase "variableValueParser \"\"" $
    parse variableValueParser ":=" @?= Left "Can't parse"
  , testCase "variableValueParser \":= -A\"" $
    parse variableValueParser ":= -A" @?= Left "Can't parse"

  , testCase "takeInvalidTail \"123\"" $
    takeInvalidTail "123" @?= ""
  , testCase "takeInvalidTail \"123 -1-\"" $
    takeInvalidTail "123 -1-" @?= " -1-"
  , testCase "takeInvalidTail \"1-1\"" $
    takeInvalidTail "1-1" @?= "-1"

  , testCase "varName_1:=123" $
    parse prob40 "varName_1:=123" @?= Right ("varName_1", 123)
  , testCase "theVAR  :=  -3456" $
    parse prob40 "theVAR  :=  -3456" @?= Right ("theVAR", -3456)
  , testCase "varName_1234  :=  -  3456" $
    parse prob40 "varName_1234  :=  -  3456" @?= Right ("varName_1234", -3456)
  , testCase "VarName_1:=123" $
    parse prob40 "VarName_1:=-1" @?= Left "Can't parse"
  , testCase "varName:=-1" $
    parse prob40 "varName:=-1" @?= Right ("varName", -1)
  , testCase "varName:=-1" $
    parse prob40 "a:=0" @?= Right ("a", 0)
  , testCase ":=0" $
    parse prob40 ":=0" @?= Left "Can't parse"
  , testCase "varName_:=" $
    parse prob40 "varName_:=" @?= Left "Can't parse"
  , testCase " := 123 " $
    parse prob40 " := 123 " @?= Left "Can't parse"
  , testCase "varName_:=-1" $
    parse prob40 "varName_:=-1" @?= Right ("varName_", -1)
  , testCase "_ := 123 " $
    parse prob40 "_ := 123 " @?= Left "Can't parse"
  , testCase "varName := 1234_" $
    parse prob40 "varName := 1234_invalid_" @?= Left "Leftover: _invalid_"
  , testCase "varName := 1234-" $
    parse prob40 "varName := 1234-" @?= Left "Leftover: -"
  , testCase "varName := --1" $
    parse prob40 "varName := --1" @?= Left "Can't parse"
  , testCase "varName := - - 1" $
    parse prob40 "varName := - - 1" @?= Left "Can't parse"
  , testCase "varName := _1234_" $
    parse prob40 "varName := _1234_" @?= Left "Can't parse"
  , testCase "" $
    parse prob40 "" @?= Left "Can't parse"

  , testCase ":= varName := 1234" $
    parse prob40 ":= varName := 1234" @?= Left "Can't parse"
  , testCase "varName :=:= 1234" $
    parse prob40 "varName :=:= 1234" @?= Left "Can't parse"
  , testCase "varName := 1234:" $
    parse prob40 "varName := 1234:" @?= Left "Leftover: :"
  , testCase "varName := 1234 =" $
    parse prob40 "varName := 1234 = " @?= Left "Leftover:  = "
  , testCase "varName := 1234 =" $
    parse prob40 "varName := 1234 = " @?= Left "Leftover:  = "    

  , testCase "varName := 1234 := 1" $
    parse prob40 "varName := 1234 := 1" @?= Left "Can't parse"
  , testCase "varName := 1234 := 1" $
    parse prob40 "varName := 1234 := 1" @?= Left "Can't parse"
  , testCase "varName := varName := 1" $
    parse prob40 "varName := varName := 1" @?= Left "Can't parse"
  , testCase "var-1:=-1" $
    parse prob40 "var-1:=-1" @?= Left "Can't parse"
  , testCase "var:1:=-1" $
    parse prob40 "var:1:=-1" @?= Left "Can't parse"    
  , testCase "var:1:=-1" $
    parse prob40 "var1:=:-1" @?= Left "Can't parse"    

  , testCase "pairwise [1,2]"           $ pairwise   [1,2]         @?= [(1,2)]
  , testCase "unpairwise [(1,2)]"       $ unpairwise [(1,2)]       @?= [1,2]
  , testCase "pairwise [1,2,3]"         $ pairwise   [1,2,3]       @?= [(1,2),(2,3)]
  , testCase "unpairwise [(1,2),(2,3)]" $ unpairwise [(1,2),(2,3)] @?= [1,2,3]
  ]
