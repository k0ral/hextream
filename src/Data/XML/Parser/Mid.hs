{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | Mid-level XML parsers, built on top of "Data.XML.Parser.Low":
--
-- - some formatting details are abstracted away (e.g. quoting, whitespacing), therefore parsers are not reversible
-- - entities delimited by an opening and closing sequence are recognized, except for tags which need a more complex, recursive logic
-- - token parsers do not overlap, therefore XML document can be tokenized in a stateless way
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid
  ( module Data.XML.Parser.Mid.Attribute
  , module Data.XML.Parser.Mid.Doctype
  , Instruction(..)
  , XMLDeclaration(..)
  , StartTag(..)
  , EmptyElementTag(..)
  , Token(..)
  , TokenParser()
  , runTokenParser
  , tokenInstruction
  , tokenComment
  , tokenCdata
  , tokenDoctype
  , tokenXmlDeclaration
  , tokenStartTag
  , tokenEndTag
  , tokenEmptyElementTag
  , tokenData
  , anyToken
  ) where

import           Control.Applicative
import           Control.Arrow                 ((>>>))
import           Control.Monad.Compat
import           Control.Monad.Fail.Compat
import           Data.Char
import           Data.Functor
import           Data.Maybe
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.XML.Parser.Low
import           Data.XML.Parser.Mid.Attribute
import           Data.XML.Parser.Mid.Doctype
import           Numeric
import           Text.Parser.Char
import           Text.Parser.Combinators


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

data Token
  = TokenXMLDeclaration XMLDeclaration
  | TokenDoctype Doctype
  | TokenInstruction Instruction
  | TokenStartTag StartTag
  | TokenEndTag QName
  | TokenEmptyElementTag EmptyElementTag
  | TokenData [Content]
  | TokenComment Text
  | TokenCDATA Text
  deriving (Eq, Ord, Show)

-- | Processing instruction.
--
-- <https://www.w3.org/TR/REC-xml/#dt-pi>
data Instruction = Instruction Text Text
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-xmldecl>
data XMLDeclaration = XMLDeclaration Text (Maybe Text) (Maybe Bool)
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-stag>
data StartTag = StartTag QName [Attribute]
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-eetag>
data EmptyElementTag = EmptyElementTag QName [Attribute]
  deriving (Eq, Ord, Read, Show)

-- | A parser that consumes whole 'Token's.
newtype TokenParser m a = TokenParser { runTokenParser :: m a }

deriving instance Functor m => Functor (TokenParser m)
deriving instance Applicative m => Applicative (TokenParser m)
deriving instance Alternative m => Alternative (TokenParser m)
deriving instance Monad m => Monad (TokenParser m)

instance (Parsing m, Monad m) => MonadFail (TokenParser m) where
  fail = TokenParser . unexpected


-- | <https://www.w3.org/TR/REC-xml/#NT-doctypedecl>
--
-- >>> parseOnly (runTokenParser tokenDoctype) "<!DOCTYPE greeting SYSTEM 'hello.dtd'>"
-- Right (Doctype "greeting" (Just (SystemID "hello.dtd")) [])
-- >>> parseOnly (runTokenParser tokenDoctype) "<!DOCTYPE foo [ <!ENTITY x '&lt;'> ]>"
-- Right (Doctype "foo" Nothing [GeneralEntityDeclaration "x" [ContentReference (EntityRef "lt")]])
tokenDoctype :: CharParsing m => Monad m => TokenParser m Doctype
tokenDoctype = TokenParser doctype

-- | <https://www.w3.org/TR/REC-xml/#dt-pi>
--
-- >>> parseOnly (runTokenParser tokenInstruction) "<?xml-stylesheet type='text/xsl' href='style.xsl'?>"
-- Right (Instruction "xml-stylesheet" "type='text/xsl' href='style.xsl'")
tokenInstruction :: CharParsing m => Monad m => TokenParser m Instruction
tokenInstruction = TokenParser $ do
  name <- tokenInstructionOpen
  tokenWhitespace
  content <- manyTill anyChar $ try tokenInstructionClose
  return $ Instruction name $ Text.pack content

-- | <https://www.w3.org/TR/REC-xml/#NT-Comment>
--
-- >>> parseOnly (runTokenParser tokenComment) "<!-- declarations for <head> & <body> -->"
-- Right " declarations for <head> & <body> "
-- >>> parseOnly (runTokenParser tokenComment) "<!-- B+, B, or B--->"
-- Right " B+, B, or B-"
tokenComment :: CharParsing m => Monad m => TokenParser m Text
tokenComment = TokenParser $ do
  tokenCommentOpen
  content <- manyTill anyChar $ try tokenCommentClose
  return $ Text.pack content

-- | <https://www.w3.org/TR/REC-xml/#dt-cdsection>
--
-- >>> parseOnly (runTokenParser tokenCdata) "<![CDATA[<greeting>Hello, world!</greeting>]]>"
-- Right "<greeting>Hello, world!</greeting>"
tokenCdata :: CharParsing m => Monad m => TokenParser m Text
tokenCdata = TokenParser $ do
  tokenCdataOpen
  content <- manyTill anyChar $ try tokenCdataClose
  return $ Text.pack content

-- | <https://www.w3.org/TR/REC-xml/#NT-XMLDecl>
--
-- >>> parseOnly (runTokenParser tokenXmlDeclaration) "<?xml version='1.0' encoding='UTF-8' standalone='yes'?>"
-- Right (XMLDeclaration "1.0" (Just "UTF-8") (Just True))
tokenXmlDeclaration :: CharParsing m => Monad m => TokenParser m XMLDeclaration
tokenXmlDeclaration = TokenParser $ do
  tokenXmlDeclarationOpen
  tokenWhitespace

  Attribute key value <- attribute
  guard $ key == QName "" "version"
  version <- expandContents decodePredefinedEntities value

  encoding <- optional $ do
    tokenWhitespace
    Attribute key value <- attribute
    guard $ key == QName "" "encoding"
    expandContents decodePredefinedEntities value

  standalone <- optional $ do
    tokenWhitespace
    Attribute key value <- attribute
    guard $ key == QName "" "standalone"
    boolean <- expandContents decodePredefinedEntities value
    case boolean of
      "yes" -> return True
      "no"  -> return False
      _     -> empty

  optional tokenWhitespace
  tokenXmlDeclarationClose
  return $ XMLDeclaration version encoding standalone

-- | <https://www.w3.org/TR/REC-xml/#NT-STag>
--
-- >>> parseOnly (runTokenParser tokenStartTag) "<termdef id='dt-dog' term='dog'>"
-- Right (StartTag (QName {namePrefix = "", nameLocal = "termdef"}) [Attribute (QName {namePrefix = "", nameLocal = "id"}) [ContentText "dt-dog"],Attribute (QName {namePrefix = "", nameLocal = "term"}) [ContentText "dog"]])
-- >>> parse (runTokenParser tokenStartTag) "<updated>2003-12-13T18:30:02Z</updated>"
-- Done "2003-12-13T18:30:02Z</updated>" (StartTag (QName {namePrefix = "", nameLocal = "updated"}) [])
tokenStartTag :: CharParsing m => Monad m => TokenParser m StartTag
tokenStartTag = TokenParser $ do
  name <- tokenStartTagOpen
  attributes <- many (tokenWhitespace >> attribute)
  optional tokenWhitespace
  tokenElementClose
  return $ StartTag name attributes

-- | <https://www.w3.org/TR/REC-xml/#NT-ETag>
--
-- >>> parseOnly (runTokenParser tokenEndTag) "</termdef>"
-- Right (QName {namePrefix = "", nameLocal = "termdef"})
tokenEndTag :: CharParsing m => Monad m => TokenParser m QName
tokenEndTag = TokenParser $ do
  name <- tokenEndTagOpen
  optional tokenWhitespace
  tokenElementClose
  return name

-- | <https://www.w3.org/TR/REC-xml/#NT-EmptyElemTag>
--
-- >>> parseOnly (runTokenParser tokenEmptyElementTag) "<IMG align='left' src='http://www.w3.org/Icons/WWW/w3c_home' />"
-- Right (EmptyElementTag (QName {namePrefix = "", nameLocal = "IMG"}) [Attribute (QName {namePrefix = "", nameLocal = "align"}) [ContentText "left"],Attribute (QName {namePrefix = "", nameLocal = "src"}) [ContentText "http://www.w3.org/Icons/WWW/w3c_home"]])
tokenEmptyElementTag :: CharParsing m => Monad m => TokenParser m EmptyElementTag
tokenEmptyElementTag = TokenParser $ do
  name <- tokenStartTagOpen
  attributes <- optional $ do
    tokenWhitespace
    attribute `sepBy` tokenWhitespace
  optional tokenWhitespace
  tokenEmptyElementTagClose
  return $ EmptyElementTag name $ fromMaybe mempty attributes

-- | <https://www.w3.org/TR/REC-xml/#NT-CharData>
--
-- >>> parseOnly (runTokenParser tokenData) "Rock &amp; roll"
-- Right [ContentText "Rock ",ContentReference (EntityRef "amp"),ContentText " roll"]
tokenData :: CharParsing m => Monad m => TokenParser m [Content]
tokenData = TokenParser $ some (tokenContent "<")

-- | Parse any 'Token'.
anyToken :: CharParsing m => Monad m => TokenParser m Token
anyToken = TokenDoctype <$> tokenDoctype
  <|> TokenInstruction <$> tokenInstruction
  <|> TokenComment <$> tokenComment
  <|> TokenCDATA <$> tokenCdata
  <|> TokenXMLDeclaration <$> tokenXmlDeclaration
  <|> TokenStartTag <$> tokenStartTag
  <|> TokenEndTag <$> tokenEndTag
  <|> TokenEmptyElementTag <$> tokenEmptyElementTag
  <|> TokenData <$> tokenData
