module Parser
  ( parseExpressions,
    reservedKeywords,
    FExp (..),
    FBinOperator (..),
    FUnOperator (..),
    Directive (..),
    Definition (..),
    FNode (..),
  )
where

import Control.Applicative
import Control.Monad (when)
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec (ErrorFancy (..), MonadParsec (eof, notFollowedBy), Parsec, choice, fancyFailure, manyTill, option, skipMany, skipSome, try)
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
  | Exit
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
  | IfThenElse [FNode] [FNode]
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
parseString = manyTill charOrEscape (char '"')
  where
    charOrEscape = try escapedQuote <|> printChar
    escapedQuote = char '\\' >> char '"' >> return '"'

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
    <|> (Exit <$ symbol "exit")

parseDirective :: FParser FExp
parseDirective = FDirective <$> parseDirectiveWord

reservedKeywords :: [String]
reservedKeywords =
  [ ":",
    ";",
    "dup",
    "drop",
    "swap",
    "over",
    "rot",
    "clear",
    ".s",
    ".",
    "emit",
    "cr",
    ".\"",
    "if",
    "else",
    "then",
    "do",
    "loop",
    "exit"
  ]

parseWordName :: FParser String
parseWordName = some (alphaNumChar <|> symbolChar)

parseWord :: FParser FExp
parseWord = do
  name <- parseWordName
  if name `elem` reservedKeywords
    then fail $ "unexpected keyword: " ++ name
    else return $ FWord name

parseDefinition :: FParser FNode
parseDefinition = do
  _ <- symbol ":"
  sc
  name <- parseWordName
  sc

  atEnd <- option False (eof >> return True)
  when atEnd $ fFail "error: unexpected end of input after definition name"

  body <-
    try (parseBlock $ symbol ";") <|> do
      _ <- many (notFollowedBy eof >> parseNode')
      atEnd' <- option False (eof >> return True)
      if atEnd'
        then fFail "error: incomplete definition, missing ';'"
        else fFail "error: malformed definition body"

  return $ WordDef (Definition name body)

parseBlock :: FParser String -> FParser [FNode]
parseBlock endMark = do
  exprs <- many $ do
    notFollowedBy endMark
    expr <- parseNode'
    sc
    return expr

  _ <- endMark

  return exprs

parseUntil :: [FParser String] -> FParser [FNode]
parseUntil endMarks = do
  many $ do
    notFollowedBy (choice endMarks)
    expr <- try parseIfThenElse <|> try parseDoLoop <|> parseExpression
    sc
    return expr

parseIfThenElse :: FParser FNode
parseIfThenElse = do
  _ <- try $ symbol "if"
  sc

  thenBranch <- parseUntil [symbol "else", symbol "then"]

  hasElse <- option False (try $ symbol "else" >> return True)

  sc

  elseBranch <-
    if hasElse
      then parseUntil [symbol "then"]
      else return []

  _ <- symbol "then"

  return $ IfThenElse thenBranch elseBranch

parseDoLoop :: FParser FNode
parseDoLoop = do
  _ <- try $ symbol "do"
  sc

  body <- parseUntil [symbol "loop"]

  _ <- symbol "loop"

  return $ DoLoop body

parseExpression :: FParser FNode
parseExpression =
  Literal
    <$> ( parseInteger
            <|> parseOperator
            <|> parsePrintString
            <|> parseDirective
            <|> parseWord
        )

sc :: FParser ()
sc = skipMany (skipSome spaceChar <|> skipLineComment "\\")

parseNode' :: FParser FNode
parseNode' = try parseIfThenElse <|> try parseDoLoop <|> parseExpression

parseNode :: FParser FNode
parseNode =
  parseDefinition <|> parseNode'

parseExpressions :: FParser FNode
parseExpressions = do
  sc
  nodes <- many $ do
    e <- parseNode
    sc
    return e

  return $ Sequence nodes
