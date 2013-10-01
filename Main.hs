{-# LANGUAGE OverloadedStrings#-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Error
import Control.Concurrent.MVar

import System.Posix.Signals
import System.Environment

import Data.List

import DBus
import DBus.Client

config_file :: FilePath -> String
config_file home = home ++ "/.automounter.conf"

main :: IO ()
main = do
  home <- getEnv "HOME"
  conn <- connectSystem
  matchers <- readConfig (config_file home)
  runScript $ do
    devs <- getDeviceList conn
    mapM_ (mountIfMatches conn matchers) devs
    return ()

  conn `onAdded` \dev -> do
    mountIfMatches conn matchers dev

  mvar <- newEmptyMVar
  installHandler sigINT (Catch $ handler mvar) Nothing
  installHandler sigTERM (Catch $ handler mvar) Nothing
  
  readMVar mvar
  disconnect conn

handler :: MVar () -> IO ()
handler var = putMVar var ()

data DeviceMatcher = DeviceFile FilePath
                   | UUID String
  deriving (Show)

readConfig :: FilePath -> IO [DeviceMatcher]
readConfig path = do
  file <- readFile path
  return (parseConfig file)

parseConfig :: String -> [DeviceMatcher]
parseConfig = map parseLine . lines
  where parseLine line = case split line of
          ("device", path) -> DeviceFile path
          ("uuid", uuid)   -> UUID uuid
                           
        split str = let (a,b) = break whitespace str
                    in (a, dropWhile whitespace b)
        whitespace = flip elem " \t"

data Device = Device {
  objectPath :: ObjectPath,
  deviceFile :: FilePath,
  deviceUUID :: String,
  deviceMounted    :: Bool
} deriving (Show)

getDeviceList :: Client -> Script [Device]
getDeviceList conn = getDevicePathList conn >>= mapM (getDevice conn)

matches :: Device -> DeviceMatcher -> Bool
matches (Device _ path1 _ _) (DeviceFile path2) = path1 == path2
matches (Device _ _ uuid1 _) (UUID uuid2) = uuid1 == uuid2

mountIfMatches :: Client -> [DeviceMatcher] -> Device -> Script ()
mountIfMatches conn matchers dev = when (isJust $ find (matches dev) matchers) $
  mount conn dev

mount :: Client -> Device -> Script ()
mount conn (Device path _ _ mounted) = unless mounted $
  bimapEitherT show (const ()) $ EitherT $
    call conn $ (methodCall path devIFace "FilesystemMount") {
      methodCallDestination = Just udisksDest,
      methodCallBody = [toVariant ("" :: String), toVariant ([] :: [String])]
      }

onAdded :: Client -> (Device -> Script ()) -> IO ()
onAdded conn func = listen conn match callback
  where match = matchAny {
          matchPath = Just $ udisksObjPath,
          matchInterface = Just $ udisksIFace,
          matchMember = Just "DeviceAdded"
          }
        callback sig = do
          res <- runEitherT (callback' sig)
          case res of
            Left e -> err e
            Right r -> return r
        callback' sig = do
          objPath <- hoistEither $ note "Couldn't convert variant of type " $
                       fromVariant $ head $ signalBody sig
          dev <- getDevice conn objPath
          func dev
        

getDevice :: Client -> ObjectPath -> Script Device
getDevice conn path = do
  file <- prop "DeviceFile"
  uuid <- prop "IdUuid"
  mounted <- prop "DeviceIsMounted"
  return $ Device path file uuid mounted

  where prop :: (IsVariant t) => String -> Script t
        prop = fmap fromVariant' . getProperty conn path devIFace

getProperty :: Client -> ObjectPath -> InterfaceName -> String -> Script Variant
getProperty conn path iface property = EitherT $ do
  res <- call conn $ (methodCall path propIFace "Get") {
    methodCallDestination = Just udisksDest,
    methodCallBody = [toVariant iface, toVariant property]
  }
  case res of
    Left e -> return $ Left $ show e
    Right res' -> return $ Right $ fromVariant' $ head $ methodReturnBody res'

getDevicePathList :: Client -> Script [ObjectPath]
getDevicePathList conn = fmap (fromVariant'.  head . methodReturnBody) $
  EitherT $ fmapL show <$> call conn
         (methodCall udisksObjPath udisksIFace "EnumerateDevices") {
           methodCallDestination = Just udisksDest
         }

fromVariant' :: (IsVariant t) => Variant -> t
fromVariant' v = case fromVariant v of
  Just v' -> v'
  Nothing -> error $ "Couldn't convert variant of type " ++ show (variantType v)

udisksObjPath :: ObjectPath
udisksObjPath = "/org/freedesktop/UDisks"

udisksIFace :: InterfaceName
udisksIFace = "org.freedesktop.UDisks"

devIFace :: InterfaceName
devIFace = "org.freedesktop.UDisks.Device"

propIFace :: InterfaceName
propIFace = "org.freedesktop.DBus.Properties"

udisksDest :: BusName
udisksDest = "org.freedesktop.UDisks"
