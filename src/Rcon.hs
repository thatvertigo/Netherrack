module Rcon (Rcon, command, runRcon) where

import Data.Int
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Builder
import Unsafe.Coerce (unsafeCoerce)
import Data.Binary
import Data.Binary.Get (getInt32le, runGet)
import Data.Char
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString (toStrict, fromStrict)
import Control.Monad
import Data.IORef
import Control.Exception.Safe (tryAny)
data PacketType = Login | Command | Response

instance Enum PacketType where
    fromEnum Login = 3
    fromEnum Command = 2
    fromEnum Response = 0
    toEnum 3 = Login
    toEnum 2 = Command
    toEnum 0 = Response

packet :: Int32 -> PacketType -> String -> Builder
packet reqId ty text =
    int32LE reqId <>
    int32LE (unsafeCoerce $ fromEnum ty) <>
    string7 text <>
    word8 0x0 <>
    word8 0x0

packetLen :: Builder -> Int32
packetLen p =
    fromIntegral (B.length (toLazyByteString p))

constructLogin :: Int32 -> String -> B.ByteString
constructLogin reqId password =
    let p = packet reqId Login password in toLazyByteString $
        int32LE (packetLen p) <> p

constructCommand :: Int32 -> String -> B.ByteString
constructCommand reqId c =
    let p = packet reqId Command c in toLazyByteString $
        int32LE (packetLen p) <> p

parseResponse :: Get (Int32, String)
parseResponse = do
    rid <- getInt32le
    _ <- getInt32le
    s <- go ""
    pure (rid, s)
    where
        go acc = do
              b <- getWord8
              if b == 0
                then pure (reverse acc)
                else go (chr (fromIntegral b) : acc)

readPacket :: Socket -> IO (Int32, String)
readPacket s = do
    len <- runGet getInt32le . fromStrict <$> recv s 4
    runGet parseResponse . fromStrict <$> recv s (fromEnum len)

authRcon :: Socket -> String -> IO Bool
authRcon s password = do
    sendAll s $ toStrict $ constructLogin 0 password
    (== 0) . fst <$> readPacket s

data Rcon a = Pure a | RunCommand String (String -> Rcon a)

instance Functor Rcon where
    fmap f (Pure a) = Pure $ f a
    fmap f (RunCommand c g) = RunCommand c $ fmap f . g

instance Applicative Rcon where
    pure = Pure
    (<*>) (Pure f) x = f <$> x
    (<*>) (RunCommand c f) x = RunCommand c $ (<*> x) . f

instance Monad Rcon where
    (>>=) (Pure x) f = f x
    (>>=) (RunCommand c g) f = RunCommand c $ f <=< g

command :: String -> Rcon String
command c = RunCommand c pure

data RconError = AuthFailed | UnexpectedIncoming String | ConnectionError String deriving (Show)

runRcon :: String -> String -> String -> Rcon a -> IO (Either RconError a)
runRcon ip port password rcon = runTCPClient ip port $ \s -> do
        auth <- authRcon s password
        if not auth then pure $ Left AuthFailed else do
            reqCounter <- newIORef @Int32 1
            runRcon' s reqCounter rcon
        where
            runRcon' :: Socket -> IORef Int32 -> Rcon a -> IO (Either RconError a)
            runRcon' _ _ (Pure x) = pure $ Right x
            runRcon' s r (RunCommand c f) = do
                reqId <- atomicModifyIORef' r (\n -> (n + 1, n))
                sendAll s $ toStrict $ constructCommand reqId c
                tryPacket <- tryAny $ readPacket s >>= \ (resId, content) -> if resId == reqId then
                        runRcon' s r $ f content 
                    else
                        pure $ Left $ UnexpectedIncoming c 
                pure $ case tryPacket of
                    Left _ -> Left $ ConnectionError c
                    Right x -> x

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
