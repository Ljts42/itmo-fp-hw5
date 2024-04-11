module HW5.Parser
  ( parse
  ) where

import Data.Void (Void)
import Text.Megaparsec.Error (ParseErrorBundle)

import HW5.Base
import Text.Megaparsec (Parsec, runParser, choice, between, sepBy)
import Text.Megaparsec.Char (string, char, space)
import Text.Megaparsec.Char.Lexer (scientific)


type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser hiExpr ""

hiExpr :: Parser HiExpr
hiExpr = space *> choice
  [ HiExprApply <$> (HiExprValue <$> (HiValueFunction <$> hiFun)) <*> hiExprList
  , (HiExprValue . HiValueNumber) . toRational <$> scientific
  ] <* space

hiFun :: Parser HiFun
hiFun = choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"]

hiExprList :: Parser [HiExpr]
hiExprList = between (char '(') (char ')') (hiExpr `sepBy` char ',')
