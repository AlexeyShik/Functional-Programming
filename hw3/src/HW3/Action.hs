{-# LANGUAGE DeriveFunctor #-}

module HW3.Action
 ( HIO (..)
 , HiPermission (..)
 , PermissionException (..)
 , getPermissions
 )
where
  
import HW3.Base (HiAction(..), HiMonad(..), HiValue(..))

import Control.Exception.Base (Exception, throwIO)
import Control.Monad (ap)

import qualified Data.ByteString (readFile, writeFile)

import Data.Set (Set, fromList, member)
import qualified Data.Sequence (fromList)

import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time (getCurrentTime)

import System.Directory (createDirectory, createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (randomRIO)

data HiPermission =
  AllowRead
  | AllowWrite
  | AllowTime deriving(Show, Eq, Ord)

data PermissionException = PermissionRequired HiPermission deriving(Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving Functor

instance Applicative HIO where
  pure = return
  (<*>) = ap

instance Monad HIO where
  return x = HIO $ \_ -> return x
  mx >>= f = HIO $ \s -> do
    x <- runHIO mx s
    runHIO (f x) s

instance HiMonad HIO where
  runAction action = case action of
    HiActionRead path -> HIO $ \s -> if member AllowRead s 
      then do
        isFile <- doesFileExist path 
        isDirectory <- doesDirectoryExist path
        if isFile
          then do
            bytes <- Data.ByteString.readFile path
            return $ case decodeUtf8' bytes of
              Left _ -> HiValueBytes bytes  
              Right str -> HiValueString str
        else do
          files <- listDirectory path
          return $ HiValueList . Data.Sequence.fromList $ map (HiValueString . pack) files
      else throwIO $ PermissionRequired AllowRead
    HiActionWrite path bytes -> HIO $ \s -> if member AllowWrite s 
      then do
        Data.ByteString.writeFile path bytes
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite
    HiActionChDir path -> HIO $ \s -> if member AllowRead s 
      then do
        setCurrentDirectory path
        return HiValueNull
      else throwIO $ PermissionRequired AllowRead
    HiActionMkDir path -> HIO $ \s -> if member AllowWrite s 
      then do
        createDirectory path
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite
    HiActionCwd -> HIO $ \s -> if member AllowRead s
      then HiValueString . pack <$> getCurrentDirectory
      else throwIO $ PermissionRequired AllowRead
    HiActionNow -> HIO $ \s -> if member AllowTime s 
      then HiValueTime <$> getCurrentTime 
      else throwIO $ PermissionRequired AllowTime
    HiActionRand l r -> HIO $ \_ -> HiValueNumber . toRational <$> randomRIO (l, r)
    HiActionEcho str -> HIO $ \s -> if member AllowWrite s 
      then do
        putStrLn $ unpack str
        return HiValueNull
      else throwIO $ PermissionRequired AllowWrite

-- permissions for actions
getPermissions :: Set HiPermission
getPermissions = Data.Set.fromList [AllowRead, AllowWrite, AllowTime]