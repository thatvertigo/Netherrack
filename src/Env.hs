module Env (Env(..), Config(..), Discord(..), runWithEnv) where
import Data.Ini.Config
import Control.Monad.Trans.Reader (ReaderT (runReaderT))
import qualified Data.Text.IO as T
import Data.Functor ((<&>))
import UnliftIO (TChan)
import Log.Event
import UnliftIO.STM (newTChanIO)

data Env = Env
    { config :: Config
    , logChan :: TChan Event
    }

data Config = Config
    { rconHost :: String
    , rconPort :: String
    , rconPassword :: String
    , logPath :: String
    , logFilename :: String
    , discord :: Discord
    } deriving (Show)

data Discord = Discord
    { token :: String
    , webhook :: String
    , chatChannel :: String
    , consoleChannel :: Maybe String
    , serverName :: String
    , serverPfp :: String
    } deriving (Show)

parser :: IniParser Config
parser = do
    (rconHost, rconPort, rconPassword) <- section "RCON" $ (,,) <$> fieldOf "host" string <*> fieldOf "port" string <*> fieldOf "password" string
    (logPath, logFilename) <- section "LOG" $ (,) <$> fieldOf "path" string <*> fieldOf "filename" string
    discord <- section "DISCORD" $ do
        token <- fieldOf "bot-token" string
        webhook <- fieldOf "webhook-url" string
        chatChannel <- fieldOf "chat-channel" string
        consoleChannel <- fieldMbOf "console-channel" string
        serverName <- fieldOf "server-name" string
        serverPfp <- fieldOf "server-pfp" string
        pure $ Discord {..}
    pure $ Config {..}

newEnv :: TChan Event -> Config -> Env
newEnv logChan config = Env {..}

buildEnv :: IO (Either String Env)
buildEnv = do
    logChan <- newTChanIO
    T.readFile "config.ini" <&> fmap (newEnv logChan) . (`parseIniFile` parser)

runWithEnv :: Monad m => ReaderT Env m a -> IO (m (Either String a))
runWithEnv f = buildEnv <&> either (pure . Left) (fmap Right . runReaderT f)
