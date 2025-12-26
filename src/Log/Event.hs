module Log.Event (Event(..), findEvents) where
import Log.Parse (LogLine (contents), parseLine)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

data Event
    = Line String
    | PlayerJoined String (Double, Double, Double)
    | PlayerLeft String String
    | Chat String String
    -- | ServerStopped
    -- | ServerStarted
    deriving (Show)

type Parser = Parsec Void String

findEvents :: String -> [Event]
findEvents x = Line x:maybe [] findEvents' (parseLine x)

findEvents' :: LogLine -> [Event]
findEvents' =
  extract . parse parseContentsToEvent "" . contents
  where
    extract (Right x) = [x]
    extract (Left _)  = []

parseContentsToEvent :: Parser Event
parseContentsToEvent =
      try parseChat
  <|> try parsePlayerJoined
  <|> try parsePlayerLeft
  where
    parsePlayerJoined = do
      name <-
        manyTill anySingle (char '[')
          <* manyTill anySingle (try (string "] logged in with entity id "))
          <* some digitChar
          <* string " at ("

      [x, y, z] <-
        fmap (read @Double)
          <$> some (digitChar <|> char '.' <|> char '-')
          `sepBy` (char ',' >> space)
          <* char ')'

      pure $ PlayerJoined name (x, y, z)

    parsePlayerLeft =
      PlayerLeft
        <$> manyTill anySingle (string " lost connection: ")
        <*> many anySingle

    parseChat =
      Chat
        <$> (char '<' *> manyTill anySingle (string "> "))
        <*> many anySingle
