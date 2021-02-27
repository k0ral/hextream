-- | Document type declaration parsers.
--
-- <https://www.w3.org/TR/REC-xml/#dt-doctype>
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.Doctype
  ( Doctype(..)
  , doctype
  ) where

import           Control.Applicative
import           Data.Maybe
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.XML.InternalSubset.Parser.High
import           Data.XML.Parser.Low
import           Data.XML.Parser.Mid.ExternalID
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#dt-doctype>
data Doctype = Doctype Text (Maybe ExternalID) InternalSubset
  deriving (Eq, Ord, Read, Show)


-- | <https://www.w3.org/TR/REC-xml/#NT-doctypedecl>
--
-- >>> parseOnly doctype "<!DOCTYPE greeting SYSTEM 'hello.dtd'>"
-- Right (Doctype "greeting" (Just (SystemID "hello.dtd")) (InternalSubset {_elementTypes = [], _attributeLists = [], _generalEntities = [], _parameterEntities = [], _notations = [], _instructions = []}))
-- >>> parseOnly doctype "<!DOCTYPE foo [ <!ENTITY x '&lt;'> ]>"
-- Right (Doctype "foo" Nothing (InternalSubset {_elementTypes = [], _attributeLists = [], _generalEntities = [GeneralEntity "x" [ContentReference (EntityRef "lt")]], _parameterEntities = [], _notations = [], _instructions = []}))
doctype :: CharParsing m => Monad m => m Doctype
doctype = do
  tokenDoctypeOpen
  tokenWhitespace
  name <- tokenName
  externalID <- optional $ tokenWhitespace >> externalID
  optional tokenWhitespace
  entities <- fromMaybe emptyInternalSubset <$> optional
    (between (char '[' >> optional tokenWhitespace) (optional tokenWhitespace >> char ']')
      internalSubset)
  tokenElementClose
  return $ Doctype name externalID entities
