{-# LANGUAGE OverloadedStrings #-}
-- | Low-level XML parsers:
--
-- - parsed tokens are small and may overlap; it is not possible to tokenize XML document in a stateless way
-- - parsers are reversible: all formatting details are retained (e.g. whitespacing)
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Low
  ( module Data.XML.Parser.Low.Entity
  , module Data.XML.Parser.Low.Name
  , module Data.XML.Parser.Low.Reference
  , module Data.XML.Parser.Low
  , module Data.XML.InternalSubset.Parser.Low
  ) where

import           Control.Applicative
import           Control.Arrow                      ((>>>))
import           Control.Monad
import           Data.Char
import           Data.Functor
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.XML.InternalSubset.Parser.Low
import           Data.XML.Parser.Low.Entity
import           Data.XML.Parser.Low.Name
import           Data.XML.Parser.Low.Reference
import           Numeric
import           Text.Parser.Char
import           Text.Parser.Combinators


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | Raw text or reference.
data Content = ContentText Text | ContentReference Reference
  deriving (Eq, Ord, Read, Show)

-- | Expand content reference, if any.
expandContent :: Alternative m => EntityDecoder -> Content -> m Text
expandContent _ (ContentText t) = pure t
expandContent f (ContentReference r) = expandReference f r

-- | Same as 'expandContent', but on a list. Provided for convenience.
expandContents :: Alternative m => Monad m => EntityDecoder -> [Content] -> m Text
expandContents f contents = mconcat <$> mapM (expandContent f) contents

-- | @'@
tokenSingleQuote :: CharParsing m => m Char
tokenSingleQuote = char '\''

-- | @"@
tokenDoubleQuote :: CharParsing m => m Char
tokenDoubleQuote = char '"'

-- | Single or double quote.
tokenQuote :: CharParsing m => m Char
tokenQuote = tokenSingleQuote <|> tokenDoubleQuote

-- | <https://www.w3.org/TR/REC-xml/#NT-S>
tokenWhitespace :: CharParsing m => m String
tokenWhitespace = some (satisfy isXmlSpace) where
  isXmlSpace ' '  = True
  isXmlSpace '\t' = True
  isXmlSpace '\r' = True
  isXmlSpace '\n' = True
  isXmlSpace _    = False

-- | <https://www.w3.org/TR/REC-xml/#NT-Eq>
tokenEqual :: CharParsing m => Monad m => m ()
tokenEqual = do
  optional tokenWhitespace
  char '='
  optional tokenWhitespace
  return ()

tokenContent :: CharParsing m => Monad m => String -> m Content
tokenContent forbiddenChars = (ContentText . Text.pack <$> some (noneOf $ '&':forbiddenChars))
  <|> (ContentReference <$> tokenReference)

-- | Return processing instruction name.
--
-- >>> parseOnly tokenInstructionOpen "<?php"
-- Right "php"
tokenInstructionOpen :: CharParsing m => Monad m => m Text
tokenInstructionOpen = do
  string "<?"
  name <- tokenName
  guard $ Text.toLower name /= "xml"
  return name

-- | @?>@
tokenInstructionClose :: CharParsing m => m ()
tokenInstructionClose = void $ string "?>"

-- | @<![CDATA[@
--
-- <https://www.w3.org/TR/REC-xml/#NT-CDStart>
tokenCdataOpen :: CharParsing m => m ()
tokenCdataOpen = void $ string "<![CDATA["

-- | @]]>@
--
-- <https://www.w3.org/TR/REC-xml/#NT-CDEnd>
tokenCdataClose :: CharParsing m => m ()
tokenCdataClose = void $ string "]]>"

-- | @<!--@
tokenCommentOpen :: CharParsing m => m ()
tokenCommentOpen = void $ string "<!--"

-- | @-->@
tokenCommentClose :: CharParsing m => m ()
tokenCommentClose = void $ string "-->"

-- | @<!DOCTYPE@
tokenDoctypeOpen :: CharParsing m => m ()
tokenDoctypeOpen = void $ string "<!DOCTYPE"

-- | @<?xml@
tokenXmlDeclarationOpen :: CharParsing m => m ()
tokenXmlDeclarationOpen = void $ string "<?xml"

-- | @?>@
tokenXmlDeclarationClose :: CharParsing m => m ()
tokenXmlDeclarationClose = void $ string "?>"

-- | @/>@
tokenEmptyElementTagClose :: CharParsing m => m ()
tokenEmptyElementTagClose = void $ string "/>"

-- | Return tag name.
--
-- >>> parseOnly tokenStartTagOpen "<foo"
-- Right (QName {namePrefix = "", nameLocal = "foo"})
tokenStartTagOpen :: CharParsing m => Monad m => m QName
tokenStartTagOpen = char '<' *> tokenQualifiedName

-- | Return tag name.
--
-- >>> parseOnly tokenEndTagOpen "</foo"
-- Right (QName {namePrefix = "", nameLocal = "foo"})
tokenEndTagOpen :: CharParsing m => Monad m => m QName
tokenEndTagOpen = string "</" *> tokenQualifiedName

-- | @>@
tokenElementClose :: CharParsing m => m ()
tokenElementClose = void $ char '>'

