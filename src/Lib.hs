-- {-# LANGUAGE  Haskell2010 #-}

module Lib (
  someFunc,
) where

import Text.Parsec
import Text.Parsec.String

import Data.Coerce (coerce)

-- model open recursion by making self dependency explicit.
data Object x = MkObject (x -> x)

-- tying the knot
use :: Object x -> x
use (MkObject x) = let res = x res in res

-- ab is extend, ba is super, and this is the only impl that use everything once.
inherit :: (a -> b) -> (b -> a) -> Object a -> Object b
inherit ab ba (MkObject aa) = MkObject (ab . aa . ba)

-- an object with two virtual field
test :: Object (Int, Int)
test = MkObject $ \self -> (2, fst self + fst self)

inheritTest :: Object ((Int, Int), Int)
inheritTest = inherit (\(l, r) -> ((l + 1, r + 2), r + 1)) fst test

class AST repr where
  lit :: Int -> repr
  plus :: repr -> repr -> repr

class Var repr where
  var :: String -> repr

type WholeParser repr = Parser repr
type LitParser repr = Parser repr
type PlusParser repr = Parser repr

intP :: Parser Int
intP = read <$> many1 digit

stringP :: Parser String
stringP = many1 letter

type OriginalParser repr = ((LitParser repr, PlusParser repr), WholeParser repr)

originalParser :: (AST repr) => Object (OriginalParser repr)
originalParser = MkObject $ \(~(_, p)) ->
  let
    litP = lit <$> intP
    plusP = between (char '(') (char ')') (do l <- p; spaces; char '+'; spaces; r <- p; return $ plus l r)
    wholeP = litP <|> plusP
   in
    ((litP, plusP), wholeP)

type ExtendedParser repr = (VarParser repr, OriginalParser repr)

type VarParser repr = Parser repr
extendedParser :: (AST repr, Var repr) => Object (ExtendedParser repr)
extendedParser = inherit extend snd originalParser
 where
  extend ~((litP, plusP), wholeP) =
    let
      varP = var <$> stringP
     in
      (varP, ((litP, plusP), varP <|> wholeP))

instance AST MyString where
  lit = MyString . show
  plus =
    let f x y = "(" ++ x ++ " " ++ "+" ++ " " ++ y ++ ")" in coerce f

instance Var MyString where
  var = MyString

instance Show MyString where
  show = unMyString

-- use MyString avoid use FlexibleInstances extension
newtype MyString = MyString {unMyString :: String}

someFunc :: IO ()
someFunc = do
  print $ use test
  print $ use inheritTest
  print $ runParser (snd $ use (originalParser :: Object (OriginalParser MyString))) () "" "(1 + 2)"
  print $ runParser (snd $ use (originalParser :: Object (OriginalParser MyString))) () "" "(1 + a)"
  print $ runParser (snd $ snd $ use (extendedParser :: Object (ExtendedParser MyString))) () "" "(1 + a)"
