{-# LANGUAGE OverloadedStrings          #-}
-- | All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.Attribute
  ( Attribute(..)
  , attribute
  ) where

import           Data.XML.Parser.Low
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#dt-attr>
data Attribute = Attribute QName [Content]
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#NT-Attribute>
--
-- >>> parseOnly attribute "name = 'value'"
-- Right (Attribute (QName {namePrefix = "", nameLocal = "name"}) [ContentText "value"])
attribute :: CharParsing m => Monad m => m Attribute
attribute = do
  key <- tokenQualifiedName
  tokenEqual
  quote <- oneOf "'\""
  value <- many $ tokenContent [quote, '<']
  char quote
  return $ Attribute key value


