module Parser (parseExpressions, FExp (..), FOperator (..), Directive (..), Definition (..)) where

import Control.Applicative
import Control.Monad (when)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (ErrorFancy (..), MonadParsec (eof, notFollowedBy), Parsec, fancyFailure, manyTill, option, skipMany, skipSome, try)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, printChar, spaceChar, string', symbolChar)
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

data Definition = Definition String [FExp]
  deriving (Show, Eq)

data FExp
  = FNum Integer
  | FOp FOperator
  | FDirective Directive
  | FWord String
  | FDefine Definition
  deriving (Show, Eq)

fFail :: String -> FParser a
fFail msg = fancyFailure $ Set.singleton $ ErrorFail msg

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

parseWordName :: FParser String
parseWordName = try $ some (alphaNumChar <|> symbolChar)

parseWord :: FParser FExp
parseWord = do
  name <- parseWordName
  if name `elem` [":", ";", "dup", "drop", "swap", "over", "rot", "clear", ".s", ".", "emit", "cr", ".\""]
    then fail $ "Unexpected keyword: " ++ name
    else return $ FWord name

parseDefinition :: FParser FExp
parseDefinition = do
  _ <- try $ symbol ":"
  sc
  name <- parseWordName
  sc

  atEnd <- option False (eof >> return True)
  when atEnd $ fFail "error: unexpected end of input after definition name"

  body <- manyTill (parseExpression' <* sc) (try $ symbol ";")

  return $ FDefine (Definition name body)

parseExpression' :: FParser FExp
parseExpression' = parseInteger <|> parseOperator <|> parsePrintString <|> parseDirective <|> parseWord

parseExpression :: FParser FExp
parseExpression = parseDefinition <|> parseExpression'

sc :: FParser ()
sc = skipMany (skipSome spaceChar <|> skipLineComment "\\")

parseExpressions :: FParser [FExp]
parseExpressions = do
  sc
  many $ do
    e <- parseExpression
    sc
    return e
