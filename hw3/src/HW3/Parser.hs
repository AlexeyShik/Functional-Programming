{-# LANGUAGE OverloadedStrings #-}

module HW3.Parser
  ( parse
  )
where

import HW3.Base (HiAction(..), HiExpr(..), HiFun(..), HiValue(..))

import Control.Monad.Combinators.Expr (makeExprParser, Operator, Operator(..))

import qualified Data.ByteString.Char8 (pack)
import Data.Char (chr, digitToInt, isAlpha, isSpace, isAlphaNum, isDigit)
import Data.Text (pack)
import Data.Void (Void)

import Text.Megaparsec (Parsec, choice, single, (<|>), many, runParser, between, manyTill, satisfy, notFollowedBy, try, sepBy1, eof)
import Text.Megaparsec.Char (string, char)
import Text.Megaparsec.Char.Lexer (scientific, charLiteral)
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void String

-- parse many spaces
pSpaces :: Parser [Char]
pSpaces = many $ satisfy isSpace

-- skip spaces before and after calling given parser
skipSpacesParser :: Parser a -> Parser a
skipSpacesParser p = do
  _ <- pSpaces
  x <- p
  _ <- pSpaces
  return x

-- parse left parenthesis
pLeftParenthesis :: Parser Char
pLeftParenthesis = skipSpacesParser $ single '('

-- parse right parenthesis
pRightParenthesis :: Parser Char
pRightParenthesis = skipSpacesParser $ single ')'

-- parse left bracket
pLeftBracket :: Parser Char
pLeftBracket = skipSpacesParser $ single '['

-- parse right bracket
pRightBracket :: Parser Char
pRightBracket = skipSpacesParser $ single ']'

-- parse left figure bracket
pLeftFigureBracket :: Parser Char
pLeftFigureBracket = skipSpacesParser $ single '{'

-- parse right figure bracket
pRightFigureBracket :: Parser Char
pRightFigureBracket = skipSpacesParser $ single '}'

-- parse comma
pComma :: Parser Char
pComma = skipSpacesParser $ single ','

-- parse minus
pMinus :: Parser Char
pMinus = skipSpacesParser $ single '-'

-- parse colon
pColon :: Parser Char
pColon = skipSpacesParser $ single ':'

-- parse quote
pQuote :: Parser Char
pQuote = single '"'

-- parse sharp
pSharp :: Parser Char
pSharp = skipSpacesParser $ single '#'

-- parse dot
pDot :: Parser Char
pDot = skipSpacesParser $ single '.'

-- wrap given parser in parenthesis 
pBetweenParenthesis :: Parser a -> Parser a
pBetweenParenthesis p = skipSpacesParser $ between pLeftParenthesis pRightParenthesis p

-- parse HiFun
pHiFun :: Parser HiFun
pHiFun = skipSpacesParser $ choice
  [ HiFunDiv <$ string "div"
  , HiFunMul <$ string "mul"
  , HiFunAdd <$ string "add"
  , HiFunSub <$ string "sub"
  , HiFunAnd <$ string "and"
  , HiFunOr <$ string "or"
  , HiFunLessThan <$ string "less-than"
  , HiFunGreaterThan <$ string "greater-than"
  , HiFunEquals <$ string "equals"
  , HiFunNotLessThan <$ string "not-less-than"
  , HiFunNotGreaterThan <$ string "not-greater-than"
  , HiFunNotEquals <$ string "not-equals"
  , HiFunNot <$ string "not"
  , HiFunIf <$ string "if"
  , HiFunLength <$ string "length"
  , HiFunToUpper <$ string "to-upper"
  , HiFunToLower <$ string "to-lower"
  , HiFunReverse <$ string "reverse"
  , HiFunTrim <$ string "trim"
  , HiFunList <$ string "list"
  , HiFunRange <$ string "range"
  , HiFunFold <$ string "fold"
  , HiFunPackBytes <$ string "pack-bytes"
  , HiFunUnpackBytes <$ string "unpack-bytes"
  , HiFunZip <$ string "zip"
  , HiFunUnzip <$ string "unzip"
  , HiFunEncodeUtf8 <$ string "encode-utf8"
  , HiFunDecodeUtf8 <$ string "decode-utf8"
  , HiFunSerialise <$ string "serialise"
  , HiFunDeserialise <$ string "deserialise"
  , HiFunCount <$ string "count"
  , HiFunKeys <$ string "keys"
  , HiFunValues <$ string "values"
  , HiFunInvert <$ string "invert"
  , HiFunParseTime <$ string "parse-time"
  , HiFunRand <$ string "rand"
  , HiFunEcho <$ string "echo"
  , HiFunRead <$ string "read"
  , HiFunWrite <$ string "write"
  , HiFunMkDir <$ string "mkdir"
  , HiFunChDir <$ string "cd"]

-- parse HiAction, that has no corresponding HiFun
pHiAction :: Parser HiAction
pHiAction = skipSpacesParser $ choice
  [ HiActionCwd <$ string "cwd"
  , HiActionNow <$ string "now"]

-- parse HiValueNull
pHiValueNull :: Parser HiValue
pHiValueNull = skipSpacesParser $ HiValueNull <$ string "null"

-- parse HiValueString
pHiValueString :: Parser HiValue
pHiValueString = skipSpacesParser $ do
  _ <- pQuote
  str <- manyTill charLiteral pQuote
  return $ HiValueString $ pack str

-- parse HiValueNumber
pHiValueNumber :: Parser HiValue
pHiValueNumber = skipSpacesParser $ choice
  [ HiValueNumber . toRational <$> scientific
  , pMinus >> HiValueNumber . negate . toRational <$> scientific
  , HiValueBool True <$ string "true"
  , HiValueBool False <$ string "false"]

-- parse HiValueFunction
pHiValueFunction :: Parser HiValue
pHiValueFunction = skipSpacesParser $ HiValueAction <$> pHiAction <|> HiValueFunction <$> pHiFun

-- parse arguments for list: 2, null, "expr"
pListArguments :: Parser [HiExpr]
pListArguments = do
  first <- pHiExpr
  other <- many $ pComma >> pHiExpr
  return $ first : other

-- parse HiValueList
pHiValueList :: Parser HiExpr
pHiValueList = skipSpacesParser $ do
  _ <- pLeftBracket
  args <- pListArguments <|> return []
  _ <- pRightBracket
  return $ HiExprApply (HiExprValue $ HiValueFunction HiFunList) args

-- parse one char of a byte: 0-9 or a-f
pByteChar :: Parser Char
pByteChar = satisfy (\c -> isDigit c || c >= 'a' && c <= 'f')

-- parse byte: 00..ff
pByte :: Parser String
pByte = do
  c1 <- pByteChar
  c2 <- pByteChar
  return [chr $ digitToInt c1 * 16 + digitToInt c2]

-- parse bytestring arguments: 0f f0
pHiBytesArguments :: Parser HiValue
pHiBytesArguments = skipSpacesParser $ do
  first <- pByte
  other <- many $ try $ satisfy isSpace >> pSpaces >> pByte
  _ <- pSpaces
  return $ HiValueBytes $ Data.ByteString.Char8.pack $ mconcat $ first : other

-- parse HiValueBytes
pHiValueBytes :: Parser HiExpr
pHiValueBytes = do
  _ <- pLeftBracket
  _ <- pSharp
  value <- pHiBytesArguments <|> return (HiValueBytes $ Data.ByteString.Char8.pack [])
  _ <- pSharp
  _ <- pRightBracket
  return $ HiExprValue value

-- parse HiExprValue
pHiExprValue :: Parser HiExpr
pHiExprValue = skipSpacesParser $ choice
  [ HiExprValue <$> pHiValueNumber
  , HiExprValue <$> pHiValueFunction
  , HiExprValue <$> pHiValueString
  , HiExprValue <$> pHiValueNull
  , try pHiValueList
  , pHiValueBytes]

-- parse empty arguments: ()
pHiExprApplyEmptyArguments :: HiExpr -> Parser HiExpr
pHiExprApplyEmptyArguments name = skipSpacesParser $ do
  _ <- pLeftParenthesis
  _ <- pRightParenthesis
  return $ HiExprApply name []

-- parse nonempty arguments: (1, 2)
pHiExprApplyArguments :: HiExpr -> Parser HiExpr
pHiExprApplyArguments name = skipSpacesParser $ do
  _ <- pLeftParenthesis
  first <- pHiExpr
  args <- many (pComma >> pHiExpr)
  _ <- pRightParenthesis
  return (HiExprApply name (first : args))

-- parse application of a function with given HiExpr for function
pHiExprApply' :: HiExpr -> Parser HiExpr
pHiExprApply' name = skipSpacesParser $ do
  result <- try (pHiExprApplyEmptyArguments name) <|> pHiExprApplyArguments name
  pHiExprSuffix result

-- parse dot access to a function  with given HiExpr for function
pHiDotAccess :: HiExpr -> Parser HiExpr
pHiDotAccess name = skipSpacesParser $ do
  _ <- pDot
  arg <- foldl1 (\v acc -> v ++ '-' : acc) <$> ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
  let result = HiExprApply name [HiExprValue . HiValueString . pack $ arg]
  pHiExprSuffix result

-- parse application of a function
pHiExprApply :: Parser HiExpr
pHiExprApply = do
  name <- pHiExprValue <|> pBetweenParenthesis pHiToken
  pHiExprSuffix name

-- parse something, that can continue chain of function applications
pHiExprSuffix :: HiExpr -> Parser HiExpr
pHiExprSuffix name = try (pHiExprApply' name) <|> try (pHiDotAccess name) <|> try (pHiExprRun' name) <|> return name

-- parse run of function with given HiExpr for function
pHiExprRun' :: HiExpr -> Parser HiExpr
pHiExprRun' name = skipSpacesParser $ do
  _ <- single '!'
  pHiExprSuffix $ HiExprRun name

-- parse entry of a dict: 1:"Hi"
pHiDictEntry :: Parser (HiExpr, HiExpr)
pHiDictEntry = skipSpacesParser $ do
  key <- pHiExpr
  _ <- pColon
  val <- pHiExpr
  (pComma >> return (key, val)) <|> return (key, val)

-- parse HiExprDict
pHiExprDict :: Parser HiExpr
pHiExprDict = skipSpacesParser $ do
  _ <- pLeftFigureBracket
  value <- many pHiDictEntry
  _ <- pRightFigureBracket
  pHiExprSuffix $ HiExprDict value

-- parse token for megaparsec makeExprParser
pHiToken :: Parser HiExpr
pHiToken = skipSpacesParser $ choice
  [ try pHiExprApply
  , try pHiExprDict
  , pBetweenParenthesis pHiExpr]

-- helper for building operator table
convertInfixToBase :: HiFun -> HiExpr -> HiExpr -> HiExpr
convertInfixToBase func a b = HiExprApply (HiExprValue (HiValueFunction func)) [a, b]

-- constructor for non-associative operations 
binary :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binary name f = InfixN (f <$ string name)

-- constructor for left-associative operations
binaryL :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryL name f = InfixL (f <$ string name)

-- constructor for right-associative operations
binaryR :: String -> (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr
binaryR name f = InfixR (f <$ string name) 

-- table with operators for megaparsec makeExprParser
operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
  [ [ binaryL "*" (convertInfixToBase HiFunMul)
    , InfixL $ convertInfixToBase HiFunDiv <$ try (string "/" >> notFollowedBy (string "="))]
  , [ binaryL "+" (convertInfixToBase HiFunAdd)
    , binaryL "-" (convertInfixToBase HiFunSub)]
  , [ binary "<=" (convertInfixToBase HiFunNotGreaterThan)
    , binary "<" (convertInfixToBase HiFunLessThan)
    , binary ">=" (convertInfixToBase HiFunNotLessThan)
    , binary ">" (convertInfixToBase HiFunGreaterThan)
    , binary "==" (convertInfixToBase HiFunEquals)
    , binary "/=" (convertInfixToBase HiFunNotEquals)]
  , [ binaryR "&&" (convertInfixToBase HiFunAnd)]
  , [ binaryR "||" (convertInfixToBase HiFunOr)]
  ]

-- expression without checking end of input
pHiExpr :: Parser HiExpr
pHiExpr = makeExprParser pHiToken operatorTable >>= pHiExprSuffix 

-- parse HiExpr
pHiExprWithEof :: Parser HiExpr
pHiExprWithEof = do 
  result <- pHiExpr
  _ <- eof 
  return result

-- function for parsing HiExpr from a String
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser pHiExprWithEof "HiExpressionParser"
