module Log.Parse (parseLine, LogLine(..), LogLevel(..), Logger(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Functor
import Data.Void

data Logger = ServerThread | UserAuth Int | Other String deriving (Eq, Show)
data LogLevel = Info | Warn deriving (Eq, Show)

data LogLine = LogLine
    { time :: String
    , logger :: Logger
    , level :: LogLevel
    , contents :: String
    } deriving (Show)

parseLine :: String -> Maybe LogLine
parseLine s = case parse parseLine' "" s of
   Left _ -> Nothing
   Right x -> Just x

parseLine' :: Parsec Void String LogLine
parseLine' = do
    time' <- parseTimestamp
    space
    (logger', level') <- parseLoggerAndLevel
    _ <- char ':'
    space
    contents' <- many $ noneOf @[] "\n"
    pure $ LogLine {
        time = time',
        logger = logger',
        level = level',
        contents = contents'
    }
    where
        parseTimestamp :: Parsec Void String String
        parseTimestamp = char '[' *> many (noneOf [']']) <* char ']'

        parseLoggerAndLevel :: Parsec Void String (Logger, LogLevel)
        parseLoggerAndLevel = char '[' *> ((,) <$> toLogger <* char '/' <*> toLogLevel <* char ']')

        toLogger :: Parsec Void String Logger
        toLogger =  string "Server thread" $> ServerThread
            <|> UserAuth . read <$> (string "User Authenticator #" *> some digitChar)
            <|> Other <$> many (noneOf @[] "/")

        toLogLevel :: Parsec Void String LogLevel
        toLogLevel = (string "INFO" $> Info) 
            <|> (string "WARN" $> Warn) 
            <|> (many (noneOf @[] "]") >>= (fail . ("Failed to parse log level: " ++)))
