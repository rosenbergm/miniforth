module Parser (parseExpressions, FExp (..), FOperator (..), Directive (..)) where

import Control.Applicative
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (notFollowedBy), Parsec, manyTill, skipMany, skipSome, try)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, printChar, spaceChar, string')
import Text.Megaparsec.Char.Lexer (skipLineComment)

type FParser = Parsec Void String

data FOperator = FAdd | FMul | FSub | FDiv
  deriving (Show, Eq)

data Directive
  = Dup
  | Drop
  | Swap
  | Over
  | Rot
  | Clear
  | Dot
  | DotS
  | Emit
  | Cr
  | DotString String
  deriving (Show, Eq)

data FExp
  = FNum Integer
  | FOp FOperator
  | FDirective Directive
  deriving (Show, Eq)

symbol :: String -> FParser String
symbol s = try $ do
  result <- string' s
  notFollowedBy alphaNumChar
  return result

parseInteger :: FParser FExp
parseInteger = do
  n <- some digitChar
  return $ FNum (read n)

parseOperatorChar :: FParser FOperator
parseOperatorChar =
  (FAdd <$ char '+')
    <|> (FMul <$ char '*')
    <|> (FSub <$ char '-')
    <|> (FDiv <$ char '/')

parseOperator :: FParser FExp
parseOperator = do
  FOp <$> parseOperatorChar

parseString :: FParser String
parseString = manyTill printChar (char '"')

parsePrintString :: FParser FExp
parsePrintString = do
  _ <- try $ string' ".\""
  _ <- char ' '
  FDirective . DotString <$> parseString

parseDirectiveWord :: FParser Directive
parseDirectiveWord =
  (Dup <$ symbol "dup")
    <|> (Drop <$ symbol "drop")
    <|> (Swap <$ symbol "swap")
    <|> (Over <$ symbol "over")
    <|> (Rot <$ symbol "rot")
    <|> (Clear <$ symbol "clear")
    <|> (DotS <$ symbol ".s")
    <|> (Dot <$ symbol ".")
    <|> (Emit <$ symbol "emit")
    <|> (Cr <$ symbol "cr")

parseDirective :: FParser FExp
parseDirective = do
  FDirective <$> parseDirectiveWord

parseExpression :: FParser FExp
parseExpression = parseInteger <|> parseOperator <|> parsePrintString <|> parseDirective

sc :: FParser ()
sc = skipMany (skipSome spaceChar <|> skipLineComment "\\")

parseExpressions :: FParser [FExp]
parseExpressions = do
  sc
  many $ do
    e <- parseExpression
    sc
    return e
