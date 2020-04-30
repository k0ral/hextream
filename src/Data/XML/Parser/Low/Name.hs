{-# LANGUAGE OverloadedStrings #-}
-- | All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Low.Name
  ( QName(..)
  , tokenQualifiedName
  , tokenNCName
  , tokenName
  ) where

import           Control.Applicative
import           Data.Char
import           Data.Maybe
import           Data.String
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Text.Parser.Char

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | A qualified name.
--
-- <https://www.w3.org/TR/xml-names/#dt-qualname>
data QName = QName
  { namePrefix :: Text
  , nameLocal :: Text
  } deriving (Eq, Ord, Read, Show)

-- | Build a qualified name in a concise way. Prefix is assumed to be empty.
--
-- >>> "foo" :: QName
-- QName {namePrefix = "", nameLocal = "foo"}
instance IsString QName where
  fromString s = QName mempty $ Text.pack s

-- | <https://www.w3.org/TR/xml-names/#NT-QName>
--
-- >>> parseOnly tokenQualifiedName "price"
-- Right (QName {namePrefix = "", nameLocal = "price"})
-- >>> parseOnly tokenQualifiedName "edi:price"
-- Right (QName {namePrefix = "edi", nameLocal = "price"})
tokenQualifiedName :: CharParsing m => Monad m => m QName
tokenQualifiedName = do
  prefix <- optional $ tokenNCName <* char ':'
  value <- tokenNCName
  return $ QName (fromMaybe mempty prefix) value

-- | <https://www.w3.org/TR/REC-xml/#NT-NameStartChar>
isNameStartChar :: Char -> Bool
isNameStartChar ':' = True
isNameStartChar '_' = True
isNameStartChar c
  | isLetter c = True
  | fromEnum c > 0xC0 && fromEnum c < 0xD6 = True
  | fromEnum c > 0xD8 && fromEnum c < 0xF6 = True
  | fromEnum c > 0xF8 && fromEnum c < 0x2FF = True
  | fromEnum c > 0x370 && fromEnum c < 0x37D = True
  | fromEnum c > 0x37F && fromEnum c < 0x1FFF = True
  | fromEnum c > 0x200C && fromEnum c < 0x200D = True
  | fromEnum c > 0x2070 && fromEnum c < 0x218F = True
  | fromEnum c > 0x2C00 && fromEnum c < 0x2FEF = True
  | fromEnum c > 0x3001 && fromEnum c < 0xD7FF = True
  | fromEnum c > 0xF900 && fromEnum c < 0xFDCF = True
  | fromEnum c > 0xFDF0 && fromEnum c < 0xFFFD = True
  | fromEnum c > 0x10000 && fromEnum c < 0xEFFFF = True
  | otherwise = False

-- | <https://www.w3.org/TR/REC-xml/#NT-NameChar>
isNameChar :: Char -> Bool
isNameChar '-' = True
isNameChar '.' = True
isNameChar c
  | isDigit c = True
  | isNameStartChar c = True
  | fromEnum c == 0xB7 = True
  | fromEnum c > 0x0300 && fromEnum c < 0x036F = True
  | fromEnum c > 0x203F && fromEnum c < 0x2040 = True
  | otherwise = False

-- | <https://www.w3.org/TR/xml-names/#NT-NCName>
--
-- >>> parseOnly tokenNCName "price"
-- Right "price"
-- >>> parse tokenNCName "edi:price"
-- Done ":price" "edi"
tokenNCName :: CharParsing m => Monad m => m Text
tokenNCName = Text.pack <$> do
  c <- satisfy (\c -> isNameStartChar c && c /= ':')
  t <- many $ satisfy (\c -> isNameChar c && c /= ':')
  return $ c : t

-- | <https://www.w3.org/TR/REC-xml/#NT-Name>
--
-- >>> parseOnly tokenName "price"
-- Right "price"
-- >>> parseOnly tokenName "edi:price"
-- Right "edi:price"
tokenName :: CharParsing m => Monad m => m Text
tokenName = Text.pack <$> do
  c <- satisfy isNameStartChar
  t <- many $ satisfy isNameChar
  return $ c : t
