module Log.Event (Event(..), findEvents) where
import Log.Parse (LogLine (contents), parseLine)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Text.Regex.TDFA
import Control.Monad (guard)

-- Credit https://github.com/destruc7i0n/shulker
deathMessageRegex :: String
deathMessageRegex = "^[[:alnum:]_]+ (died|drowned|blew up|fell|burned|froze|starved|suffocated|withered|walked into a cactus|experienced kinetic energy|discovered (the )?floor was lava|tried to swim in lava|hit the ground|didn't want to live|went (up in flames|off with a bang)|walked into (fire|danger)|was (killed|shot|slain|pummeled|pricked|blown up|impaled|squashed|squished|skewered|poked|roasted|burnt|frozen|struck by lightning|fireballed|stung|doomed))"

data Event
    = Line String
    | PlayerJoined String (Double, Double, Double)
    | PlayerLeft String String
    | Chat String String
    | PlayerDied String String
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
  <|> try parseDeathMessage
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

    parseDeathMessage :: Parser Event
    parseDeathMessage = do
        line <- some anySingle
        guard (line =~ deathMessageRegex)
        pure $ PlayerDied (head (words line)) line

