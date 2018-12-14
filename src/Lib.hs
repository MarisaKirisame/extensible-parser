{-# Language TupleSections, NoMonomorphismRestriction, TypeSynonymInstances, FlexibleInstances #-}
module Lib
    ( someFunc
    ) where

import Text.Parsec
import Text.Parsec.String

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

class Hole repr where
  hole :: String -> repr

type WholeParser repr = Parser repr
type LitParser repr = Parser repr
type PlusParser repr = Parser repr

intP :: Parser Int
intP = read <$> many1 digit

stringP :: Parser String
stringP = many1 letter

type OriginalParser repr = ((LitParser repr, PlusParser repr), WholeParser repr)
originalParser :: AST repr => Object (OriginalParser repr)
originalParser = MkObject $ \(~(_, p)) -> let
  litP = lit <$> intP
  plusP = between (char '(') (char ')') (do {l <- p; spaces; char '+'; spaces; r <- p; return $ plus l r})
  wholeP = litP <|> plusP in
  ((litP, plusP), wholeP)

type HoleParser repr = Parser repr
extendedParser :: (AST repr, Hole repr) => Object (HoleParser repr, OriginalParser repr)
extendedParser = inherit extend snd originalParser
  where
    extend ~((litP, plusP), wholeP) = let
      holeP = hole <$> stringP in
      (holeP, ((litP, plusP), holeP <|> wholeP))

instance AST String where
  lit = show
  plus x y = "(" ++ x ++ " " ++ "+" ++ " " ++ y ++ ")"

instance Hole String where
  hole x = x

someFunc :: IO ()
someFunc = do
  print $ use test
  print $ use inheritTest
  print $ fmap (""++) $ runParser (snd $ use originalParser) () "" "(1 + 2)"
  print $ fmap (""++) $ runParser (snd $ use originalParser) () "" "(1 + a)"
  print $ fmap (""++) $ runParser (snd $ snd $ use extendedParser) () "" "(1 + a)"
