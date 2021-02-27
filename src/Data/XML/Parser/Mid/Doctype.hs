-- | Document type declaration parsers.
--
-- <https://www.w3.org/TR/REC-xml/#dt-doctype>
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.Doctype
  ( GeneralEntityDeclaration(..)
  , generalEntityDeclaration
  , Doctype(..)
  , doctype
  ) where

import Control.Applicative
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import           Data.XML.Parser.Low
import           Data.XML.Parser.Mid.ExternalID
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#gen-entity>
data GeneralEntityDeclaration = GeneralEntityDeclaration Text [Content]
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-doctype>
data Doctype = Doctype Text (Maybe ExternalID) [GeneralEntityDeclaration]
  deriving (Eq, Ord, Read, Show)


-- | <https://www.w3.org/TR/REC-xml/#NT-GEDecl>
--
-- >>> parseOnly generalEntityDeclaration "<!ENTITY d '&#xD;'>"
-- Right (GeneralEntityDeclaration "d" [ContentReference (CharRef '\r')])
-- >>> parseOnly generalEntityDeclaration "<!ENTITY da '&#xD;&#xA;'>"
-- Right (GeneralEntityDeclaration "da" [ContentReference (CharRef '\r'),ContentReference (CharRef '\n')])
-- >>> parseOnly generalEntityDeclaration "<!ENTITY Pub-Status 'This is a pre-release of the specification.'>"
-- Right (GeneralEntityDeclaration "Pub-Status" [ContentText "This is a pre-release of the specification."])
generalEntityDeclaration :: CharParsing m => Monad m => m GeneralEntityDeclaration
generalEntityDeclaration = do
  tokenEntityDeclarationOpen
  tokenWhitespace
  name <- tokenName
  tokenWhitespace
  quote <- tokenQuote
  definition <- many (tokenContent $ quote:"%")
  char quote
  optional tokenWhitespace
  tokenElementClose
  return $ GeneralEntityDeclaration name definition

-- | <https://www.w3.org/TR/REC-xml/#NT-doctypedecl>
--
-- >>> parseOnly doctype "<!DOCTYPE greeting SYSTEM 'hello.dtd'>"
-- Right (Doctype "greeting" (Just (SystemID "hello.dtd")) [])
-- >>> parseOnly doctype "<!DOCTYPE foo [ <!ENTITY x '&lt;'> ]>"
-- Right (Doctype "foo" Nothing [GeneralEntityDeclaration "x" [ContentReference (EntityRef "lt")]])
doctype :: CharParsing m => Monad m => m Doctype
doctype = do
  tokenDoctypeOpen
  tokenWhitespace
  name <- tokenName
  externalID <- optional $ tokenWhitespace >> externalID
  optional tokenWhitespace
  entities <- fromMaybe mempty <$> optional
    (between (char '[' >> optional tokenWhitespace) (optional tokenWhitespace >> char ']') $
      many generalEntityDeclaration)
  tokenElementClose
  return $ Doctype name externalID entities


quoted :: CharParsing m => Monad m => m a -> m a
quoted x = x `surroundedBy` tokenSingleQuote <|> x `surroundedBy` tokenDoubleQuote

manyQuoted :: CharParsing m => Monad m => m a -> m [a]
manyQuoted x = manyQuotedBy tokenSingleQuote x <|> manyQuotedBy tokenDoubleQuote x where
  manyQuotedBy quote x = do
    quote
    manyTill x (try quote)
