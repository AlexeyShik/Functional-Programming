{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , HiAction(..)
  , HiMonad(..)
  , isBinaryFunction
  , isMultidimensionalFunction
  , isTernaryFunction
  , isUnaryFunction
  )
where

import Data.Ratio (Rational)
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Codec.Serialise (Serialise)
import Data.Map.Strict (Map)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
  
data HiFun = -- function names (e.g. div, sort, length, ...)
  HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan 
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim 
  | HiFunList
  | HiFunRange
  | HiFunFold 
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert 
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho deriving (Eq, Ord, Show, Serialise, Generic)

data HiValue = -- values (numbers, booleans, strings, ...)
  HiValueBool Bool
  | HiValueNumber Data.Ratio.Rational
  | HiValueFunction HiFun 
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString 
  | HiValueDict (Map HiValue HiValue) 
  | HiValueAction HiAction 
  | HiValueTime UTCTime deriving (Eq, Ord, Show, Serialise, Generic)

data HiExpr = -- expressions (literals, function calls, ...)
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprDict [(HiExpr, HiExpr)] 
  | HiExprRun HiExpr deriving (Eq, Ord, Show, Serialise, Generic)

data HiError = -- evaluation errors (invalid arguments, ...)
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero deriving (Eq, Ord, Show, Serialise, Generic)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text deriving (Eq, Ord, Show, Serialise, Generic)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue  

isUnaryFunction :: HiFun -> Bool
isUnaryFunction name = case name of 
  HiFunNot -> True
  HiFunLength -> True
  HiFunToUpper -> True
  HiFunToLower -> True
  HiFunReverse -> True
  HiFunTrim -> True
  HiFunPackBytes -> True
  HiFunUnpackBytes -> True
  HiFunEncodeUtf8 -> True
  HiFunDecodeUtf8 -> True
  HiFunZip -> True
  HiFunUnzip -> True
  HiFunSerialise -> True
  HiFunDeserialise -> True
  HiFunCount -> True
  HiFunKeys -> True
  HiFunValues -> True
  HiFunInvert -> True
  HiFunRead -> True
  HiFunMkDir -> True
  HiFunChDir -> True
  HiFunParseTime -> True
  HiFunEcho -> True
  _ -> False

isTernaryFunction :: HiFun -> Bool
isTernaryFunction HiFunIf = True
isTernaryFunction _ = False

isBinaryFunction :: HiFun -> Bool
isBinaryFunction name = case name of 
  HiFunDiv -> True
  HiFunMul -> True
  HiFunAdd -> True
  HiFunSub -> True
  HiFunAnd -> True
  HiFunOr -> True
  HiFunLessThan -> True
  HiFunGreaterThan -> True
  HiFunEquals -> True
  HiFunNotLessThan -> True
  HiFunNotGreaterThan -> True
  HiFunNotEquals -> True
  HiFunRange -> True
  HiFunFold -> True
  HiFunWrite -> True
  HiFunRand -> True
  _ -> False

isMultidimensionalFunction :: HiFun -> Bool
isMultidimensionalFunction name = case name of
  HiFunList -> True
  _ -> False
