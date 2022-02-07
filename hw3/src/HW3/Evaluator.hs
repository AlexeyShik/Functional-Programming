{-# LANGUAGE LambdaCase #-}

module HW3.Evaluator
  ( eval
  )
where

import HW3.Base (HiAction(..), HiError(..), HiExpr(..), HiFun(..), HiMonad(..), HiValue(..),
 isBinaryFunction, isTernaryFunction, isMultidimensionalFunction, isUnaryFunction)
import HW3.Action (getPermissions, runHIO)

import Codec.Compression.Zlib (CompressParams(..), bestCompression, compressWith, decompress, defaultCompressParams)
import Codec.Serialise (deserialiseOrFail, serialise)

import Control.Monad.Trans.Except (ExceptT(..), except, throwE, runExceptT)
import Control.Monad.Except (throwError)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 (drop, index, length, pack, reverse, take, unpack)
import Data.ByteString.Lazy.Char8 (fromStrict, toStrict)

import Data.Char (ord, chr)
import Data.Foldable (toList)

import Data.Map.Strict (Map, (!), elems, empty, insert, keys, lookup, member)
import qualified Data.Map.Strict (fromList, toList)

import Data.Semigroup (stimes)

import Data.Sequence (Seq(..), (><), fromList)
import qualified Data.Sequence (reverse)

import Data.Text (Text, pack, strip, toUpper, toLower, unpack)
import qualified Data.Text (length, reverse)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)

import GHC.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- evaluates expression
eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . doEval

-- helper for evaluating expression
doEval :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
doEval (HiExprRun expr) = processRun expr
doEval (HiExprApply (HiExprValue (HiValueNumber _)) _) = throwE HiErrorInvalidFunction
doEval (HiExprApply (HiExprValue (HiValueBool _)) _) = throwE HiErrorInvalidFunction
doEval (HiExprApply func args) = processApply (HiExprApply func args)
doEval (HiExprDict lst) = processDict lst
doEval (HiExprValue value) = return value

-- evaluates HiExprRun
processRun :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
processRun arg = do
  evArg <- doEval arg
  case evArg of
     HiValueAction action -> ExceptT $ Right <$> runAction action

-- evaluates HiExprDict
processDict :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m HiValue
processDict lst = mapEval lst >>= \val -> return $ HiValueDict $ Data.Map.Strict.fromList val

-- helper, evaluates list of pairs
mapEval :: HiMonad m => [(HiExpr, HiExpr)] -> ExceptT HiError m [(HiValue, HiValue)]
mapEval [] = except $ return []
mapEval ((ex, ey):other) = do
  vx <- doEval ex
  vy <- doEval ey
  mapEval other >>=
      \vz -> except $ return $ (vx, vy) : vz

-- evaluates HiExprApply
processApply :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
processApply (HiExprApply mfunc args) = do
  evFunc <- doEval mfunc
  case evFunc of
    HiValueFunction name ->
      if isUnaryFunction name then
        processUnaryOperation name args
      else if isBinaryFunction name then
        processBinaryOperation name args
      else if isTernaryFunction name then
        processIf name args
      else if isMultidimensionalFunction name then
        processMultiDimensional name args
      else
        throwE HiErrorInvalidFunction
    HiValueString str -> case length args of
      1 -> getByIndexStr str args
      2 -> do
        val <- getSlice (unpack str) args
        except $ return $ HiValueString $ pack val
      _ -> throwE HiErrorArityMismatch
    HiValueList lst -> case length args of
      1 -> getByIndexList (toList lst) args
      2 -> do
        val <- getSlice (toList lst) args
        except $ return $ HiValueList (fromList val)
      _ -> throwE HiErrorArityMismatch
    HiValueBytes bytes -> case length args of
      1 -> do
        x <- doEval $ head args
        except $ case x of
          HiValueNumber i -> if i >= 0 && isInt i && round i < Data.ByteString.Char8.length bytes
            then Right $ HiValueNumber $ toRational $ ord $ Data.ByteString.Char8.index bytes (round i)
            else Right HiValueNull
          _ -> Left HiErrorInvalidArgument
      2 -> do
        val <- getByteStringSlice bytes args
        except $ return $ HiValueBytes val
      _ -> throwE HiErrorArityMismatch
    HiValueDict dict -> case length args of
      1 -> do
        x <- doEval $ head args
        except $ Right $ if Data.Map.Strict.member x dict then dict ! x else HiValueNull
      _ -> throwE HiErrorArityMismatch
    _ -> throwE HiErrorInvalidFunction
processApply _ = throwE HiErrorInvalidArgument

-- checks, that given Rational is Int
isInt :: Rational -> Bool
isInt x = x == fromInteger (round x)

-- common getByIndex code for String and List
getByIndex :: HiMonad m => (a -> HiValue) -> [a] -> [HiExpr] -> ExceptT HiError m HiValue
getByIndex getter lst args =
  let mx = head args in do
    x <- doEval mx
    case x of
      HiValueNumber i -> if isInt i && i >= 0 && isInt i && round i < length lst
        then except $ return $ getter $ lst !! round i
        else except $ return HiValueNull
      _ -> throwE HiErrorInvalidArgument

-- getByIndex for String
getByIndexStr :: HiMonad m => Text -> [HiExpr] -> ExceptT HiError m HiValue
getByIndexStr str = getByIndex (\x -> HiValueString $ pack [x]) (unpack str)

-- getBiIndex for List
getByIndexList :: HiMonad m => [HiValue] -> [HiExpr] -> ExceptT HiError m HiValue
getByIndexList = getByIndex id

-- default slice for list
slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from) (drop from xs)

-- default slice for ByteString
sliceByteString :: Int -> Int -> ByteString -> ByteString
sliceByteString from to bytes = Data.ByteString.Char8.take (to - from) (Data.ByteString.Char8.drop from bytes)

-- common slice for String and List
getSlice :: HiMonad m => [a] -> [HiExpr] -> ExceptT HiError m [a]
getSlice lst = abstractSlice lst length slice []

-- slice for ByteString
getByteStringSlice :: HiMonad m => ByteString -> [HiExpr] -> ExceptT HiError m ByteString
getByteStringSlice bytes = abstractSlice bytes Data.ByteString.Char8.length sliceByteString (Data.ByteString.Char8.pack "")

-- common code for all slices
abstractSlice :: HiMonad m => a -> (a -> Int) -> (Int -> Int -> a -> a) -> a -> [HiExpr] -> ExceptT HiError m a
abstractSlice lst lengthFun sliceFun empty_ args
  | length args /= 2 = throwE HiErrorArityMismatch
  | otherwise =
    let
      mx = head args
      my = head $ tail args
      len = lengthFun lst
    in do
      doEval mx >>= (\case
        HiValueNumber i0 -> doEval my >>= (\case
          HiValueNumber j0 -> if isInt i0 && isInt j0
            then let i = round i0; j = round j0 in
              if i >= 0 && j >= 0 && i <= j then
                except $ return $ sliceFun i j lst
              else if i >= 0 && j < 0 && i <= fromIntegral len + j then
                except $ return $ sliceFun i (fromIntegral len + j) lst
              else if i < 0 && j >= 0 && fromIntegral len + i <= j then
                except $ return $ sliceFun (fromIntegral len + i) j lst
              else if i < 0 && j < 0 && i <= j then
                except $ return $ sliceFun (fromIntegral len + i) (fromIntegral len + j) lst
              else except $ return empty_
            else throwE HiErrorInvalidArgument
          HiValueNull -> if isInt i0
            then let i = round i0 in
              if i >= 0 then
                except $ return $ sliceFun i len lst
              else
                except $ return $ sliceFun (fromIntegral len + i) len lst
            else except $ return empty_
          _ -> throwE HiErrorInvalidArgument)
        HiValueNull -> doEval my >>= (\case
          HiValueNumber j0 -> if isInt j0
            then let j = round j0 in
              if j >= 0 then
                except $ return $ sliceFun 0 j lst
              else
                except $ return $ sliceFun 0 (fromIntegral len + j) lst
            else except $ return empty_
          HiValueNull -> except $ return lst
          _ -> throwE HiErrorInvalidArgument)
        _ -> throwE HiErrorInvalidArgument)

-- evaluates unary function applications
processUnaryOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
processUnaryOperation func args
  | length args /= 1 = throwE HiErrorArityMismatch
  | otherwise =
    let
      mx = head args
    in do
      x <- doEval mx
      case x of
        HiValueBool bool -> case func of
          HiFunNot -> return $ HiValueBool $ not bool
          HiFunSerialise -> return $ HiValueBytes $ toStrict $ Codec.Serialise.serialise $ HiValueBool bool
          _ -> throwError HiErrorInvalidArgument
        HiValueString str -> case func of
          HiFunLength -> return $ HiValueNumber $ toRational $ Data.Text.length str
          HiFunToUpper -> return $ HiValueString $ toUpper str
          HiFunToLower -> return $ HiValueString $ toLower str
          HiFunReverse -> return $ HiValueString $ Data.Text.reverse str
          HiFunTrim -> return $ HiValueString $ strip str
          HiFunEncodeUtf8 -> return $ HiValueBytes $ encodeUtf8 str
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise $ HiValueString str
          HiFunCount -> return $ HiValueDict $ countStr $ unpack str
          HiFunParseTime -> return $ maybe HiValueNull HiValueTime (readMaybe (unpack str))
          HiFunEcho -> return $ HiValueAction $ HiActionEcho str
          HiFunRead -> return $ HiValueAction $ HiActionRead $ unpack str
          HiFunMkDir -> return $ HiValueAction $ HiActionMkDir $ unpack str
          HiFunChDir -> return $ HiValueAction $ HiActionChDir $ unpack str
          _ -> throwError HiErrorInvalidArgument
        HiValueList lst -> case func of
          HiFunLength -> return $ HiValueNumber $ fromIntegral $ length lst
          HiFunReverse -> return $ HiValueList $ Data.Sequence.reverse lst
          HiFunPackBytes -> if all (\case
            HiValueNumber t -> t >= 0 && t <= 255 && isInt t
            _ -> False) lst
            then return $ HiValueBytes $ Data.ByteString.Char8.pack $ map (\(HiValueNumber num) -> chr (round num)) (toList lst)
            else throwError HiErrorInvalidArgument
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise $ HiValueList lst
          HiFunCount -> return $ HiValueDict $ countList $ toList lst
          _ -> throwError HiErrorInvalidArgument
        HiValueBytes bytes -> case func of
          HiFunUnpackBytes -> return $ HiValueList $ fromList $ map (HiValueNumber . toRational . ord) (Data.ByteString.Char8.unpack bytes)
          HiFunDecodeUtf8 -> case decodeUtf8' bytes of
            Left _ -> return HiValueNull
            Right str -> return $ HiValueString str
          HiFunZip -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict
            $ compressWith defaultCompressParams {compressLevel = bestCompression}
              (Data.ByteString.Lazy.Char8.fromStrict bytes)
          HiFunUnzip -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ decompress (Data.ByteString.Lazy.Char8.fromStrict bytes)
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise $ HiValueBytes bytes
          HiFunDeserialise -> return $ case deserialiseOrFail (Data.ByteString.Lazy.Char8.fromStrict bytes) of
            Left _ -> HiValueNull
            Right t -> t
          HiFunLength -> return $ HiValueNumber $ fromIntegral $ Data.ByteString.Char8.length bytes
          HiFunReverse -> return $ HiValueBytes $ Data.ByteString.Char8.reverse bytes
          HiFunCount -> return $ HiValueDict $ countList $ map (HiValueNumber . toRational . ord) (Data.ByteString.Char8.unpack bytes)
          _ -> throwError HiErrorInvalidArgument
        HiValueNumber num -> case func of
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise $ HiValueNumber num
          _ -> throwError HiErrorInvalidArgument
        HiValueFunction f -> case func of
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise $ HiValueFunction f
          _ -> throwError HiErrorInvalidArgument
        HiValueNull -> case func of
          HiFunSerialise -> return $ HiValueBytes $ Data.ByteString.Lazy.Char8.toStrict $ Codec.Serialise.serialise HiValueNull
          _ -> throwError HiErrorInvalidArgument
        HiValueDict dict -> case func of
          HiFunKeys -> return $ HiValueList $ fromList $ keys dict
          HiFunValues -> return $ HiValueList $ fromList $ elems dict
          HiFunInvert -> return $ HiValueDict $ invert (Data.Map.Strict.toList dict)
          _ -> throwError HiErrorInvalidArgument

-- evaluates count function for String
countStr :: String -> Map HiValue HiValue
countStr str =
  case str of
    [] -> Data.Map.Strict.empty
    c : other -> let
        cur_char = HiValueString $ pack [c]
        res = countStr other
        cur_cnt = Data.Map.Strict.lookup cur_char res
      in case cur_cnt of
        Just (HiValueNumber x) -> insert cur_char (HiValueNumber $ x + 1) res
        Just _ -> undefined
        Nothing -> insert cur_char (HiValueNumber 1) res

-- evaluates count function for List
countList :: [HiValue] -> Map HiValue HiValue
countList lst =
  case lst of
    [] -> Data.Map.Strict.empty
    k : other -> let
        res = countList other
        cur_cnt = Data.Map.Strict.lookup k res
      in case cur_cnt of
        Just (HiValueNumber x) -> insert k (HiValueNumber $ x + 1) res
        Just _ -> undefined
        Nothing -> insert k (HiValueNumber 1) res

-- evaluates invert function
invert :: [(HiValue, HiValue)] -> Map HiValue HiValue
invert lst =
  case lst of
    [] -> Data.Map.Strict.empty
    (k, v) : other -> let
        res = invert other
        cur_v = Data.Map.Strict.lookup v res
      in case cur_v of
        Just (HiValueList x) -> insert v (HiValueList $ fromList $ k : toList x) res
        Just _ -> undefined
        Nothing -> insert v (HiValueList $ fromList [k]) res

-- evaluates binary function applications
processBinaryOperation :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
processBinaryOperation func args
  | length args /= 2 = throwE HiErrorArityMismatch
  | otherwise = do
    x <- doEval $ head args
    case func of
      HiFunAnd -> do  --  так костыльно, потому что не хотелось весь работающий код переписывать на паттернмачинг сначала по функции, потом по аргументам
        case x of
          HiValueBool False -> return $ HiValueBool False
          HiValueNull -> return HiValueNull
          _ -> doEval $ head $ tail args
      HiFunOr -> do
       case x of
          HiValueBool False -> doEval $ head $ tail args
          HiValueNull -> doEval $ head $ tail args
          other -> return other
      HiFunEquals -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x == y
      HiFunNotEquals -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x /= y
      HiFunLessThan -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x < y
      HiFunNotLessThan -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x >= y
      HiFunGreaterThan -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x > y
      HiFunNotGreaterThan -> do
        y <- doEval $ head $ tail args
        return $ HiValueBool $ x <= y
      _ -> do
        y <- doEval $ head $ tail args
        case x of
          HiValueNumber ex -> case y of
            (HiValueNumber ey) -> case func of
              HiFunAdd -> return $ HiValueNumber (ex + ey)
              HiFunSub -> return $ HiValueNumber (ex - ey)
              HiFunMul -> return $ HiValueNumber (ex * ey)
              HiFunDiv -> case ey of
                0 -> throwError HiErrorDivideByZero
                ey' -> return $ HiValueNumber (ex / ey')
              HiFunRange -> return $ HiValueList $ fromList $ map HiValueNumber [ex .. ey]
              HiFunRand -> if isInt ex && isInt ey
                then return $ HiValueAction $ HiActionRand (round ex) (round ey)
                else throwError HiErrorInvalidArgument
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument

          HiValueFunction ex -> case y of
            HiValueList ey -> let ly = toList ey in case func of
              HiFunFold -> if null ly then return HiValueNull
                else foldl1 (\v_ acc_ -> do
                v_ >>= \v ->
                  acc_ >>= \acc -> do
                    doEval $ HiExprApply (HiExprValue $ HiValueFunction ex) [HiExprValue v, HiExprValue acc]
                ) (map return ly)
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument

          HiValueString ex -> case y of
            HiValueString ey -> case func of
              HiFunAdd -> return $ HiValueString (ex <> ey)
              HiFunDiv -> return $ HiValueString (ex <> pack "/" <> ey)
              HiFunWrite -> return $ HiValueAction $ HiActionWrite (unpack ex) (Data.ByteString.Char8.pack $ unpack ey)
              _ -> throwError HiErrorInvalidArgument
            HiValueNumber ey -> case func of
              HiFunMul -> if isInt ey && ey > 0
                then return $ HiValueString $ stimes (round ey) ex
                else throwError HiErrorInvalidArgument
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument


          HiValueList ex -> case y of
            HiValueList ey -> case func of
              HiFunAdd -> return $ HiValueList $ ex >< ey
              _ -> throwError HiErrorInvalidArgument
            HiValueNumber ey -> case func of
              HiFunMul -> if isInt ey
                then return $ HiValueList $ fromList $ stimes (round ey) (toList ex)
                else throwError HiErrorInvalidArgument
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument


          HiValueBytes ex -> case y of
            HiValueBytes ey -> case func of
              HiFunAdd -> return $ HiValueBytes $ ex <> ey
              _ -> throwError HiErrorInvalidArgument
            HiValueNumber ey -> case func of
              HiFunMul -> if isInt ey && ey > 0
                then return $ HiValueBytes $ stimes (round ey) ex
                else throwError HiErrorInvalidArgument
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument


          HiValueTime ex -> case y of
            HiValueNumber ey -> case func of
              HiFunAdd -> return $ HiValueTime $ addUTCTime (fromRational ey) ex
              _ -> throwError HiErrorInvalidArgument
            HiValueTime ey -> case func of
              HiFunSub -> return $ HiValueNumber $ toRational $ diffUTCTime ex ey
              _ -> throwError HiErrorInvalidArgument
            _ -> throwError HiErrorInvalidArgument
          _ -> throwError HiErrorInvalidArgument

-- evaluates if function applications
processIf :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
processIf func args
  | length args /= 3 = throwE HiErrorArityMismatch
  | otherwise = do
    x <- doEval $ head args
    case x of
      HiValueBool bool -> case func of
        HiFunIf -> if bool then doEval $ head $ tail args else doEval $ head $ tail $ tail args
        _ -> throwError HiErrorInvalidArgument
      _ -> throwError HiErrorInvalidArgument

-- evaluates list of expressions
evalHelper :: HiMonad m => [HiExpr] -> ExceptT HiError m (Seq HiValue)
evalHelper [] = except $ return Empty
evalHelper (x:xs) = do
  x_ <- doEval x
  xs_ <- evalHelper xs
  except $ return $ x_ :<| xs_

-- evaluates multidimensional function applications
processMultiDimensional :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
processMultiDimensional func margs = do
  evalHelper margs >>= \values -> case func of
    HiFunList -> except $ return $ HiValueList values
    _ -> throwE HiErrorInvalidArgument
      