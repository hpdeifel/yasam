{-# LANGUAGE OverloadedStrings#-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Error
import Control.Concurrent.MVar

import System.Posix.Signals

import qualified System.Environment.XDG.BaseDir as XDG

import Data.List

import DBus
import DBus.Client

main :: IO ()
main = do

  matchers <- XDG.getUserConfigFile "yasam" "yasam.conf" >>= readConfig
  conn <- connectSystem

  runScript $ do
    devs <- getDeviceList conn
    mapM_ (mountIfMatches conn matchers) devs

  conn `onAdded` mountIfMatches conn matchers

  mvar <- newEmptyMVar
  void $ installHandler sigINT (Catch $ handler mvar) Nothing
  void $ installHandler sigTERM (Catch $ handler mvar) Nothing

  readMVar mvar
  disconnect conn

handler :: MVar () -> IO ()
handler var = putMVar var ()

data DeviceMatcher = DeviceFile FilePath
                   | UUID String
  deriving (Show)

readConfig :: FilePath -> IO [DeviceMatcher]
readConfig path = parseConfig <$> readFile path

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
matches (Device _ _ uuid1 _) (UUID uuid2)       = uuid1 == uuid2

mountIfMatches :: Client -> [DeviceMatcher] -> Device -> Script ()
mountIfMatches conn matchers dev = when (isJust $ find (matches dev) matchers) $
  mount conn dev

mount :: Client -> Device -> Script ()
mount conn (Device path _ _ mounted) = unless mounted $ do
  call' conn path devIFace "FilesystemMount" [ toVariant ("" :: String)
                                             , toVariant ([] :: [String]) ]
  return ()

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
          getDevice conn objPath >>= func


getDevice :: Client -> ObjectPath -> Script Device
getDevice conn path = do
  file <- prop "DeviceFile"
  uuid <- prop "IdUuid"
  mounted <- prop "DeviceIsMounted"
  return $ Device path file uuid mounted

  where prop s = getProperty conn path devIFace s >>= fromVariantM

getProperty :: Client -> ObjectPath -> InterfaceName -> String -> Script Variant
getProperty conn path iface property =
  call' conn path propIFace "Get" [toVariant iface, toVariant property]
  >>= fromVariantM

getDevicePathList :: Client -> Script [ObjectPath]
getDevicePathList conn =
  call' conn udisksObjPath udisksIFace "EnumerateDevices" [] >>= fromVariantM

call' :: Client -> ObjectPath -> InterfaceName -> MemberName -> [Variant]
      -> Script Variant
call' conn path iface member args = do
  res <- EitherT $ fmapL show <$> call conn (methodCall path iface member) {
      methodCallDestination = Just udisksDest,
      methodCallBody = args
    }

  headMay (methodReturnBody res)
    ?? ("Method " ++ show member ++ " returned no value")

fromVariant' :: (IsVariant t) => Variant -> Either String t
fromVariant' v =
  note ("Couldn't cast Variant of type " ++ show (variantType v)) $ fromVariant v

fromVariantM :: (IsVariant t) => Variant -> Script t
fromVariantM = hoistEither . fromVariant'

udisksObjPath :: ObjectPath
udisksObjPath = "/org/freedesktop/UDisks"

udisksIFace, devIFace, propIFace :: InterfaceName
udisksIFace = "org.freedesktop.UDisks"
devIFace = "org.freedesktop.UDisks.Device"
propIFace = "org.freedesktop.DBus.Properties"

udisksDest :: BusName
udisksDest = "org.freedesktop.UDisks"
