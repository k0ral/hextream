-- | Mid-level parsers for XML external entities.
--
-- <https://www.w3.org/TR/REC-xml/#dt-extent>
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.ExternalID
  ( ExternalID(..)
  , externalID
  ) where

import           Control.Applicative
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.XML.Parser.Low
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString


-- | External entity identifier
--
-- <https://www.w3.org/TR/REC-xml/#dt-extent>
data ExternalID = PublicID Text Text | SystemID Text
  deriving (Eq, Ord, Read, Show)


-- | <https://www.w3.org/TR/REC-xml/#NT-ExternalID>
--
-- >>> parseOnly externalID "PUBLIC '-//Textuality//TEXT Standard open-hatch boilerplate//EN' 'http://www.textuality.com/boilerplate/OpenHatch.xml'"
-- Right (PublicID "-//Textuality//TEXT Standard open-hatch boilerplate//EN" "http://www.textuality.com/boilerplate/OpenHatch.xml")
-- >>> parseOnly externalID "SYSTEM '../grafix/OpenHatch.gif'"
-- Right (SystemID "../grafix/OpenHatch.gif")
externalID :: CharParsing m => Monad m => m ExternalID
externalID = publicID <|> systemID where
  publicID = do
    string "PUBLIC"
    tokenWhitespace
    a <- systemLiteral
    tokenWhitespace
    b <- systemLiteral
    return $ PublicID a b
  systemID = string "SYSTEM" *> tokenWhitespace *> (SystemID <$> systemLiteral)
  systemLiteral = Text.pack <$> manyQuoted anyChar

quoted :: CharParsing m => Monad m => m a -> m a
quoted x = x `surroundedBy` tokenSingleQuote <|> x `surroundedBy` tokenDoubleQuote

manyQuoted :: CharParsing m => Monad m => m a -> m [a]
manyQuoted x = manyQuotedBy tokenSingleQuote x <|> manyQuotedBy tokenDoubleQuote x where
  manyQuotedBy quote x = do
    quote
    manyTill x (try quote)
