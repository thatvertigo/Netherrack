module Discord.Bot where

import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Env (Env(..), Discord(..), Config (..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT, asks, ask)
import Control.Monad.Trans.Class
import qualified Log.Event as LE
import Control.Monad
import Data.Text (pack, Text, unpack, intercalate)
import UnliftIO.STM
import Control.Concurrent (forkIO)
import Control.Lens
import GHC.Generics
import Network.HTTP.Simple
import Data.Maybe (fromMaybe)
import Data.Aeson (encode)
import Rcon
import Text.Emoji
import qualified Data.Text as T
import qualified Data.Map as M

launchBot :: ReaderT Env IO ()
launchBot = do
    token <- asks (token . discord . config)
    e <- ask
    userFacingError <- lift $ runDiscord $ def
             { discordToken = "Bot " <> pack token
             , discordOnStart = do
                lift $ putStrLn "Discord bot attached"
                denv <- ask
                void $ lift $ forkIO $ forever $ runReaderT (runReaderT logAction e) denv
             , discordOnEvent = (`runReaderT` e) . discordEventHandler
             , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
             }
    lift $ TIO.putStrLn userFacingError


constructUserHover :: Text -> Text -> Text
constructUserHover displayName username = "{\"action\":\"show_text\",\"value\":[{\"text\": \"" <> displayName <> " \"}, {\"text\": \"@"<> username <>"\", \"color\": \"gray\"}]}"

constructAuthorTellraw :: Text -> Text -> Text -> Text
constructAuthorTellraw nickname username displayName = "{\"text\":\"" <> nickname <> "\",\"hover_event\":" <> constructUserHover displayName username <> "}"

constructMentionTellraw :: Text -> Text -> Text -> Text
constructMentionTellraw nickname username displayName = "{\"text\":\"@" <> nickname <> "\",\"hover_event\":" <> constructUserHover displayName username <> ", \"color\":\"#7289DA\"}"

toPlainTellraw :: Text -> Text
toPlainTellraw t = "{\"text\":\"" <> t <> "\"}"

constructMsgTellraw :: [User] -> Text -> Text
constructMsgTellraw mentions msg = intercalate ",{\"text\":\" \"}," $ map (\s -> fromMaybe (toPlainTellraw s) (M.lookup s mentionsToReplace)) $ splitOut msg (M.keys mentionsToReplace)
 where
    splitOut :: Text -> [Text] -> [Text]
    splitOut input markers = go [] [] (T.words input)
      where
        go acc cur [] =
          reverse $ emit cur acc

        go acc cur (w:ws)
          | w `elem` markers =
              go (w : emit cur acc) [] ws
          | otherwise =
              go acc (w : cur) ws

        emit [] acc = acc
        emit ws acc = T.unwords (reverse ws) : acc

    mentionsToReplace :: M.Map Text Text
    mentionsToReplace = M.fromList $ map (\u -> let
                usnm = case u of
                    User { userName = x } -> x
                udn = fromMaybe usnm (userGlobalName u)
                ugn = fromMaybe udn (memberNick =<< userMember u)
            in ("<@" <> pack (show $ userId u) <> ">", constructMentionTellraw ugn usnm udn)) mentions

discordWatermark :: Text
discordWatermark = "{\"text\":\"[\",\"color\":\"gray\"},{\"text\":\"Discord\",\"color\":\"blue\"},{\"text\":\"]\",\"color\":\"gray\"}"

constructTellraw :: Text -> Text -> Text -> [User] -> Text -> Text
constructTellraw nickname username displayName mentions msg = "tellraw @a [\"\"," <> discordWatermark <> ",{\"text\":\" <\"}," <> constructAuthorTellraw nickname username displayName <> ",{\"text\": \"> \"},"<> constructMsgTellraw mentions (replaceEmojisDiscord msg) <> "]"

replaceEmojisDiscord :: Text -> Text
replaceEmojisDiscord = replaceEmojis $ const $ \case
    [] -> ":unknown_emoji:"
    alias:_ -> ":" <> alias <> ":"

discordEventHandler :: Event -> ReaderT Env DiscordHandler ()
discordEventHandler (MessageCreate m)
    | not $ userIsBot $ messageAuthor m = do
        chatChannel <- asks (chatChannel . discord . config)
        consoleChannel <- fromMaybe "0" <$> asks (consoleChannel . discord . config)
        rh <- asks (rconHost . config)
        rp <- asks (rconPort . config)
        rpswd <- asks (rconPassword . config)
        when (messageChannelId m == DiscordId (Snowflake $ read chatChannel)) $ void $ lift $ lift $ do
            let username = userName $ messageAuthor m
            let displayName = fromMaybe username $ userGlobalName $ messageAuthor m
            let nickname = fromMaybe displayName $ memberNick =<< messageMember m
            let tr = constructTellraw
                    nickname
                    username
                    displayName
                    (messageMentions m)
                    (messageContent m)
            putStrLn $ unpack tr
            runRcon rh rp rpswd $ command $ unpack tr
        when (messageChannelId m == DiscordId (Snowflake $ read consoleChannel)) $ do
            res <- lift $ lift $ runRcon rh rp rpswd $ command $ unpack $ replaceEmojisDiscord $ messageContent m
            let cmd = head $ words $ unpack $ messageContent m
            void $ lift $ restCall $
                R.CreateMessage (DiscordId $ Snowflake $ read consoleChannel) (pack $
                    case res of
                        Left e -> "**RCON Error on `" <> cmd <> "`**: " <> show e
                        Right r -> "**RCON Response on `" <> cmd <> "`**: " <> r)
    | otherwise = pure ()
discordEventHandler _ = pure ()

logAction :: ReaderT Env DiscordHandler ()
logAction = asks logChan >>= (lift . lift . atomically . readTChan) >>= handleLogEvent

newtype UuidShape = UuidShape { id :: String } deriving (Show, Generic, FromJSON)

getUuid :: String -> IO (Maybe String)
getUuid name = do
  request <- parseRequest $ "GET https://api.mojang.com/users/profiles/minecraft/" <> name
  response <- httpJSONEither request
  case getResponseStatusCode response of
    200 ->
      case getResponseBody response of
        Right (UuidShape uuid) -> pure (Just uuid)
        Left _                -> pure Nothing
    _ -> pure Nothing

getHead :: String -> IO String
getHead username = do
    uuid <- fromMaybe "c06f89064c8a49119c29ea1dbd1aab82" <$> getUuid username
    pure $ "https://mc-heads.net/avatar/" <> uuid <> "/256"

data MessageWebhook = MessageWebhook
    { username :: String
    , avatar_url :: String
    , content :: String
    } deriving (Generic, ToJSON)

sendWebhook :: String -> MessageWebhook -> IO ()
sendWebhook webhookUrl wbhk = do
    req <- parseRequest webhookUrl
        <&> setRequestMethod "POST"
        . setRequestHeader "Content-Type" ["application/json"]
        . setRequestBodyLBS (encode wbhk)
    void $ httpBS req

handleLogEvent :: LE.Event -> ReaderT Env DiscordHandler ()
handleLogEvent (LE.Line l) = asks (consoleChannel . discord . config) >>= maybe (lift $ pure ()) (\m ->
    void $ lift $ restCall (R.CreateMessage (DiscordId $ Snowflake $ read m) $ pack l))
handleLogEvent (LE.Chat username message) = do
    webhookUrl <- asks (webhook . discord . config)
    playerHead <- lift $ lift $ getHead username
    lift $ lift $ sendWebhook webhookUrl $ MessageWebhook username playerHead message
handleLogEvent (LE.PlayerJoined username _) = do
    webhookUrl <- asks (webhook . discord . config)
    serverName <- asks (serverName . discord . config)
    serverPfp <- asks (serverPfp . discord . config)
    lift $ lift $ sendWebhook webhookUrl $ MessageWebhook serverName serverPfp $ "**" <> username <> "** joined the game"
handleLogEvent (LE.PlayerLeft username _) = do
    webhookUrl <- asks (webhook . discord . config)
    serverName <- asks (serverName . discord . config)
    serverPfp <- asks (serverPfp . discord . config)
    lift $ lift $ sendWebhook webhookUrl $ MessageWebhook serverName serverPfp $ "**" <> username <> "** left the game"
handleLogEvent (LE.PlayerDied _ msg) = do
    webhookUrl <- asks (webhook . discord . config)
    serverName <- asks (serverName . discord . config)
    serverPfp <- asks (serverPfp . discord . config)
    lift $ lift $ sendWebhook webhookUrl $ MessageWebhook serverName serverPfp $ "**" <> head (words msg) <> "** " <> unwords (tail $ words msg)
