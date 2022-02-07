module HW3.Pretty
 ( prettyValue
 )
where

import HW3.Base

import Data.ByteString.Internal (unpackChars)
import Data.Char (ord)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Maybe (isNothing)
import Data.Ratio (numerator, denominator)
import Data.Scientific (fromRationalRepetendUnlimited)
import qualified Data.Map.Strict (toList)
import Data.Text (unpack)

import Numeric (showFFloat)

import Prettyprinter (Doc, viaShow, pretty, (<+>), prettyList)
import Prettyprinter.Render.Terminal (AnsiStyle)

-- checks, that given Rational is Int
isInt :: Rational -> Bool
isInt x = x == fromInteger (round x)

-- translates HiValue to pretty format
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num)
  | isInt num = viaShow $ round num
  | isNothing $ snd $ fromRationalRepetendUnlimited num = pretty $ showFFloat Nothing (fromRational num) ""
  | otherwise = let
     p = numerator num
     q = denominator num
     (qu, re) = quotRem p q 
     in case qu of
       0 -> pretty (show re ++ "/" ++ show q)
       _ -> if re >= 0
         then pretty qu <+> pretty "+" <+> pretty (show re ++ "/" ++ show q)
         else pretty qu <+> pretty "-" <+> pretty (show (-re) ++ "/" ++ show q)
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue (HiValueString str) = viaShow (unpack str) 
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueList lst) = prettyList $ toList lst <&> prettyValue <&> show
prettyValue (HiValueBytes bytes) = pretty "[#" <+> pretty (prettyBytes (unpackChars bytes) ++ "#]")
prettyValue (HiValueDict dict) = pretty "{ " <> 
  foldl (\acc val -> case show acc of 
    "" -> val
    _ -> acc <> pretty ',' <+> val) (pretty "")
    (map (\(key, val) -> prettyValue key <> pretty ':' <+> prettyValue val) (Data.Map.Strict.toList dict)) 
  <> pretty " }"
prettyValue (HiValueTime time) = pretty $ "parse-time(\"" ++ show time ++ "\")"
prettyValue (HiValueFunction name) = case name of 
  HiFunDiv -> pretty "div"
  HiFunMul -> pretty "mul"
  HiFunAdd -> pretty "add"
  HiFunSub -> pretty "sub"
  HiFunNot -> pretty "not"
  HiFunAnd -> pretty "and"
  HiFunOr -> pretty "or"
  HiFunLessThan -> pretty "less-than"
  HiFunGreaterThan -> pretty "greater-than"
  HiFunEquals -> pretty "equals"
  HiFunNotLessThan -> pretty "not-less-than"
  HiFunNotGreaterThan -> pretty "not-greater-than"
  HiFunNotEquals -> pretty "not-equals"
  HiFunIf -> pretty "if"
  HiFunLength -> pretty "length"
  HiFunToUpper -> pretty "to-upper"
  HiFunToLower -> pretty "to-lower"
  HiFunReverse -> pretty "reverse"
  HiFunTrim -> pretty "trim"
  HiFunList -> pretty "list"
  HiFunRange -> pretty "range"
  HiFunFold -> pretty "fold"
  HiFunPackBytes -> pretty "pack-bytes"
  HiFunUnpackBytes -> pretty "unpack-bytes"
  HiFunEncodeUtf8 -> pretty "encode-utf8"
  HiFunDecodeUtf8 -> pretty "decode-utf8"
  HiFunZip -> pretty "zip"
  HiFunUnzip -> pretty "unzip"
  HiFunSerialise -> pretty "serialise"
  HiFunDeserialise -> pretty "deserialise"
  HiFunCount -> pretty "count"
  HiFunKeys -> pretty "keys"
  HiFunValues -> pretty "values"
  HiFunInvert -> pretty "invert"
  HiFunRead -> pretty "read"
  HiFunWrite -> pretty "write"
  HiFunMkDir -> pretty "mkdir"
  HiFunChDir -> pretty "cd"
  HiFunParseTime -> pretty "parse-time"
  HiFunRand -> pretty "rand"
  HiFunEcho -> pretty "echo"
prettyValue (HiValueAction action) = case action of
  HiActionRead file -> pretty "read(\"" <> pretty file <> pretty "\")"
  HiActionWrite file bytes -> pretty "write(\"" <> pretty file <> pretty "\", " <> prettyValue (HiValueBytes bytes) <> pretty ")"
  HiActionMkDir file -> pretty "mkdir(\"" <> pretty file <> pretty "\")"
  HiActionChDir file -> pretty "cd(\"" <> pretty file <> pretty "\")"
  HiActionCwd -> pretty "cwd"
  HiActionNow -> pretty "now"
  HiActionRand l r -> pretty "rand(" <> pretty l <> pretty ", " <> pretty r <> pretty ")"
  HiActionEcho s -> pretty "echo(\"" <> pretty s <> pretty "\")"   

-- helper, int to hex
translateTo16 :: Int -> Char
translateTo16 num = "0123456789abcdef" !! num

-- helper, for printing ByteString
prettyBytes :: String -> String
prettyBytes [] = []
prettyBytes (x:xs) = let 
  code = ord x
  i1 = div code 16
  i2 = mod code 16
  in translateTo16 i1 : translateTo16 i2 : ' ' :  prettyBytes xs
