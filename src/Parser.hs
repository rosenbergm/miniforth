module Parser
  ( parseExpressions,
    FExp (..),
    FBinOperator (..),
    FUnOperator (..),
    Directive (..),
    Definition (..),
    FNode (..),
  )
where

import Control.Applicative
import Control.Monad (unless, void, when)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (ErrorFancy (..), MonadParsec (eof, lookAhead, notFollowedBy), Parsec, fancyFailure, manyTill, option, skipMany, skipSome, try)
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, printChar, spaceChar, string', symbolChar)
import Text.Megaparsec.Char.Lexer (skipLineComment)

type FParser = Parsec Void String

data FBinOperator
  = FAdd
  | FMul
  | FSub
  | FDiv
  | FMod
  | FEq
  | FLt
  | FGt
  | FAnd
  | FOr
  deriving (Show, Eq)

data FUnOperator = FNeg
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
  | If
  | Else
  | Then
  | Do
  | Loop
  | I
  deriving (Show, Eq)

data Definition = Definition String [FNode]
  deriving (Show, Eq)

data FExp
  = FNum Integer
  | FBinOp FBinOperator
  | FUnOp FUnOperator
  | FDirective Directive
  | FWord String
  deriving (Show, Eq)

data FNode
  = Literal FExp
  | WordDef Definition
  | IfThenElse FNode [FNode] [FNode]
  | DoLoop [FNode]
  | Sequence [FNode]
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

parseBinaryOperator :: FParser FBinOperator
parseBinaryOperator =
  (FAdd <$ char '+')
    <|> (FMul <$ char '*')
    <|> (FSub <$ char '-')
    <|> (FDiv <$ char '/')
    <|> (FMod <$ string' "mod")
    <|> (FEq <$ char '=')
    <|> (FLt <$ char '<')
    <|> (FGt <$ char '>')
    <|> (FAnd <$ string' "and")
    <|> (FOr <$ string' "or")

parseUnaryOperator :: FParser FUnOperator
parseUnaryOperator = FNeg <$ string' "invert"

parseOperator :: FParser FExp
parseOperator = do
  (FBinOp <$> parseBinaryOperator)
    <|> (FUnOp <$> parseUnaryOperator)

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
    <|> (If <$ symbol "if")
    <|> (Else <$ symbol "else")
    <|> (Then <$ symbol "then")
    <|> (Do <$ symbol "do")
    <|> (Loop <$ symbol "loop")
    <|> (I <$ symbol "i")

parseDirective :: FParser FExp
parseDirective = do
  FDirective <$> parseDirectiveWord

parseWordName :: FParser String
parseWordName = try $ some (alphaNumChar <|> symbolChar)

parseWord :: FParser FExp
parseWord = do
  name <- parseWordName
  if name `elem` [":", ";", "dup", "drop", "swap", "over", "rot", "clear", ".s", ".", "emit", "cr", ".\"", "if", "else", "then", "do", "loop"]
    then fail $ "unexpected keyword: " ++ name
    else return $ FWord name

parseDefinition :: FParser FNode
parseDefinition = do
  _ <- try $ symbol ":"
  sc
  name <- parseWordName
  sc

  atEnd <- option False (eof >> return True)
  when atEnd $ fFail "error: unexpected end of input after definition name"

  body <- parseBlock (symbol ";")

  return $ WordDef (Definition name body)

parseBlock :: FParser String -> FParser [FNode]
parseBlock endMark = do
  exprs <- many $ do
    notFollowedBy endMark

    expr <- try parseIfThenElse <|> try parseDoLoop <|> parseExpression
    sc

    return expr

  _ <- endMark

  return exprs

parseIfThenElse :: FParser FNode
parseIfThenElse = do
  _ <- try $ symbol "if"
  sc

  thenBranch <- many $ do
    notFollowedBy (symbol "else" <|> symbol "then")
    expr <- try parseIfThenElse <|> parseExpression
    sc
    return expr

  hasElse <- option False (symbol "else" >> return True)

  elseBranch <-
    if hasElse
      then many $ do
        notFollowedBy (symbol "then")
        expr <- try parseIfThenElse <|> parseExpression
        sc
        return expr
      else return []

  _ <- symbol "then"

  let condition = Literal (FWord "_condition_placeholder_")

  return $ IfThenElse condition thenBranch elseBranch

parseDoLoop :: FParser FNode
parseDoLoop = do
  _ <- try $ symbol "do"
  sc

  body <- many $ do
    notFollowedBy (symbol "loop")
    expr <- try parseIfThenElse <|> try parseDoLoop <|> parseExpression
    sc
    return expr

  _ <- symbol "loop"

  return $ DoLoop body

parseExpression :: FParser FNode
parseExpression =
  try parseDoLoop
    <|> try parseIfThenElse
    <|> (Literal <$> (parseInteger <|> parseOperator <|> parsePrintString <|> parseDirective <|> parseWord))

sc :: FParser ()
sc = skipMany (skipSome spaceChar <|> skipLineComment "\\")

parseNode :: FParser FNode
parseNode =
  try parseDefinition
    <|> try parseIfThenElse
    <|> try parseDoLoop
    <|> parseExpression

parseExpressions :: FParser FNode
parseExpressions = do
  sc
  nodes <- many $ do
    e <- parseNode
    sc
    return e

  return $ Sequence nodes
