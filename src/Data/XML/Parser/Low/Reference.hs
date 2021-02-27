-- | Low-level parsers for XML references.
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Low.Reference where

import           Control.Applicative
import           Control.Arrow              ((>>>))
import           Data.Char
import           Data.Functor
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.XML.Parser.Low.Entity
import           Data.XML.Parser.Low.Name
import           Numeric
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#dt-entref>
--
-- Entity reference, or character reference.
data Reference = EntityRef Text | CharRef Char
  deriving (Eq, Ord, Read, Show)

-- | Resolve reference into raw text.
expandReference :: Alternative m => EntityDecoder -> Reference -> m Text
expandReference _ (CharRef c)      = pure $ Text.pack [c]
expandReference f (EntityRef name) = maybe empty pure $ runEntityDecoder f name

-- | Same as @expandReference decodePredefinedEntities@, provided for convenience.
expandReference' :: Reference -> Maybe Text
expandReference' = expandReference decodePredefinedEntities

-- | <https://www.w3.org/TR/REC-xml/#NT-Reference>
--
-- >>> parseOnly tokenReference "&#x3C;"
-- Right (CharRef '<')
-- >>> parseOnly tokenReference "&docdate;"
-- Right (EntityRef "docdate")
tokenReference :: CharParsing m => Monad m => m Reference
tokenReference = (EntityRef <$> entityRef) <|> (CharRef <$> decCharRef) <|> (CharRef <$> hexCharRef) where
  entityRef = char '&' *> tokenName <* char ';'
  decCharRef = between (string "&#") (char ';') $
    some digit >>= (readDec >>> liftParser "decimal") <&> chr

  hexCharRef = between (string "&#x") (char ';') $
    some hexDigit >>= (readHex >>> liftParser "hexadecimal") <&> chr
  liftParser _ ((result, _):_) = return result
  liftParser message _         = unexpected $ "Failed to parse " <> message
