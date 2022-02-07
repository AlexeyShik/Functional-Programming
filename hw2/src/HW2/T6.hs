{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..),
    Parser (..),
    runP,
    parseExpr,
  )
where

import Control.Applicative (Alternative, empty, many, some, (<|>))
import Control.Monad (MonadPlus, mfilter)
import Data.Scientific (scientific, toRealFloat)
import GHC.Natural (Natural)
import HW2.T1 (Annotated (..), Except (..))
import HW2.T4
import HW2.T5

data ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) s =
  case runES es (0, s) of
    Success (a :# _) -> Success a
    Error e -> Error e

-- for empty input -- Error (ErrorAtPos 0)
-- for correct char, for example c -- Success 'c'
-- state changes from (0, []) to (1, 'c':[]) when reading a char
pChar :: Parser Char
pChar = P $ ES \(pos, s) ->
  case s of
    [] -> Error (ErrorAtPos pos)
    (c : cs) -> Success (c :# (pos + 1, cs))

pExactChar :: Char -> Parser Char
pExactChar c = mfilter (== c) pChar

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P ps) (P qs) = P $
    ES $ \s ->
      case runES ps s of
        Error (ErrorAtPos i) ->
          case runES qs s of
            Error (ErrorAtPos j) -> Error (ErrorAtPos (max i j))
            Success r -> Success r
        Success r -> Success r

instance MonadPlus Parser -- No methods.

pEof :: Parser ()
pEof = P $ ES \(pos, s) ->
  case s of
    [] -> Success (() :# (pos, []))
    _ -> Error (ErrorAtPos pos)

toPInteger :: Int -> Parser Integer
toPInteger = pure . toInteger

pDigit :: Parser Integer
pDigit = do
  c <- pChar
  case c of
    '0' -> toPInteger 0
    '1' -> toPInteger 1
    '2' -> toPInteger 2
    '3' -> toPInteger 3
    '4' -> toPInteger 4
    '5' -> toPInteger 5
    '6' -> toPInteger 6
    '7' -> toPInteger 7
    '8' -> toPInteger 8
    '9' -> toPInteger 9
    _ -> parseError

pSpace :: Parser String
pSpace = do
  many (mfilter (== ' ') pChar)

parseInteger :: [Integer] -> Int -> Integer
parseInteger [] _ = 0
parseInteger (d : ds) n =
  let i = parseInteger ds (n - 1)
   in d * (10 ^ n) + i

pDouble :: Parser Expr
pDouble = do
  _ <- pSpace
  beg <- some pDigit
  _ <- pExactChar '.'
  end <- some pDigit
  _ <- pSpace
  let ds = beg ++ end
  pure (Val (toRealFloat (scientific (parseInteger ds (length ds)) (- (length end) - 1))))

pParenthesisExpr :: Parser Expr
pParenthesisExpr = do
  _ <- pSpace
  _ <- pExactChar '('
  expr <- pHighLevel
  _ <- pSpace
  _ <- pExactChar ')'
  _ <- pSpace
  pure expr

pSuccess :: Expr -> Parser Expr
pSuccess left = P $ ES \(pos, s) -> Success (left :# (pos, s))

-- rules : F -> double or F -> (E)
pLowLevel :: Parser Expr
pLowLevel = pDouble <|> pParenthesisExpr

-- rule: T -> FT'
pMidLevel :: Parser Expr
pMidLevel = do
  _ <- pSpace
  left <- pLowLevel
  _ <- pSpace
  pMidLevel' left

-- Expr - inherited left operand of Mul
-- resulting Mul is sent to next T' non-terminal  
-- rule: T' -> *FT'
pMul :: Expr -> Parser Expr
pMul left = do
  _ <- pSpace
  _ <- pExactChar '*'
  _ <- pSpace
  right <- pLowLevel
  _ <- pSpace
  pMidLevel' (Op (Mul left right))

-- Expr - inherited left operand of Div
-- resulting Div is sent to next T' non-terminal  
-- rule: T' -> /FT'
pDiv :: Expr -> Parser Expr
pDiv left = do
  _ <- pSpace
  _ <- pExactChar '/'
  _ <- pSpace
  right <- pLowLevel
  _ <- pSpace
  pMidLevel' (Op (Div left right))

-- Expr - inherited left operand of Mul or Div
-- rules: T' -> *FT' or T' -> /FT' or T' -> eps
pMidLevel' :: Expr -> Parser Expr
pMidLevel' left = pMul left <|> pDiv left <|> pSuccess left

-- rule: E -> TE'
pHighLevel :: Parser Expr
pHighLevel = do
  _ <- pSpace
  left <- pMidLevel
  _ <- pSpace
  pHighLevel' left

-- Expr - inherited left operand of Add
-- resulting Add is sent to next E' non-terminal  
-- rule: E' -> +TE'
pAdd :: Expr -> Parser Expr
pAdd left = do
  _ <- pSpace
  _ <- pExactChar '+'
  _ <- pSpace
  right <- pMidLevel
  _ <- pSpace
  pHighLevel' (Op (Add left right))

-- Expr - inherited left operand of Sub
-- resulting Sub is sent to next E' non-terminal  
-- rule: E' -> -TE'
pSub :: Expr -> Parser Expr
pSub left = do
  _ <- pSpace
  _ <- pExactChar '-'
  _ <- pSpace
  right <- pMidLevel
  _ <- pSpace
  pHighLevel' (Op (Sub left right))

-- rules: E' -> +TE' or E' -> -TE' or E' -> eps
pHighLevel' :: Expr -> Parser Expr
pHighLevel' left = pAdd left <|> pSub left <|> pSuccess left

-- rule: E0 -> E
pExpr :: Parser Expr
pExpr = do
  result <- pHighLevel
  pEof
  pure result

-- grammar for parser
-- E0 -> E
-- E -> TE'
-- E' -> +TE'
-- E' -> -TE' 
-- E' -> eps
-- T -> FT'
-- T' -> *FT' 
-- T' -> /FT'
-- T' -> eps
-- F -> (E)
-- F -> double
parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
