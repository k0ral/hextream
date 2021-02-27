{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
-- | Mid-level parsers for doctype's internal subset, built on top of "Data.XML.InternalSubset.Parser.Low":
--
-- - some formatting details are abstracted away (e.g. quoting, whitespacing), therefore parsers are not reversible
-- - entities delimited by an opening and closing sequence are recognized
-- - token parsers do not overlap, therefore XML document can be tokenized in a stateless way
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.InternalSubset.Parser.Mid
  ( module Data.XML.Parser.Mid.Comment
  , module Data.XML.Parser.Mid.ExternalID
  , module Data.XML.Parser.Mid.Instruction
  , ElementType(..)
  , AttributeList(..)
  , GeneralEntity(..)
  , ParameterEntity(..)
  , Notation(..)
  , Token(..)
  , TokenParser()
  , runTokenParser
  , tokenElementType
  , tokenAttributeList
  , tokenGeneralEntity
  , tokenParameterEntity
  , tokenNotation
  , tokenInstruction
  , tokenComment
  , anyToken
  ) where

import           Control.Applicative
import           Control.Monad.Fail.Compat
import           Data.Functor
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.XML.InternalSubset.Parser.Low
import           Data.XML.Parser.Low
import           Data.XML.Parser.Mid.Comment
import           Data.XML.Parser.Mid.ExternalID
import           Data.XML.Parser.Mid.Instruction
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

data Token
  = TokenElementType ElementType
  | TokenAttributeList AttributeList
  | TokenGeneralEntity GeneralEntity
  | TokenParameterEntity ParameterEntity
  | TokenNotation Notation
  | TokenInstruction Instruction
  | TokenComment Text
  deriving (Eq, Ord, Show)

-- | A parser that consumes whole 'Token's.
newtype TokenParser m a = TokenParser { runTokenParser :: m a }

deriving instance Functor m => Functor (TokenParser m)
deriving instance Applicative m => Applicative (TokenParser m)
deriving instance Alternative m => Alternative (TokenParser m)
deriving instance Monad m => Monad (TokenParser m)

instance (Parsing m, Monad m) => MonadFail (TokenParser m) where
  fail = TokenParser . unexpected

-- | <https://www.w3.org/TR/REC-xml/#dt-eldecl>
data ElementType = ElementType Text ElementTypeContent
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#NT-elementdecl>
--
-- Missing: mixed | children
data ElementTypeContent = ContentEmpty | ContentAny
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#NT-AttlistDecl>
--
-- Missing: attribute definition
newtype AttributeList = AttributeList Text
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#gen-entity>
data GeneralEntity = GeneralEntity Text [Content]
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-PE>
data ParameterEntity = ParameterEntity Text [EntityValue]
  deriving (Eq, Ord, Read, Show)

-- | <https://www.w3.org/TR/REC-xml/#dt-notation>
data Notation = Notation Text ExternalID
  deriving (Eq, Ord, Read, Show)


-- | <https://www.w3.org/TR/REC-xml/#NT-contentspec>
elementTypeContent :: CharParsing m => m ElementTypeContent
elementTypeContent = (tokenElementTypeContentEmpty $> ContentEmpty)
  <|> (tokenElementTypeContentAny $> ContentAny)

-- | <https://www.w3.org/TR/REC-xml/#NT-elementdecl>
--
-- >>> parseOnly (runTokenParser tokenElementType) "<!ELEMENT br EMPTY>"
-- Right (ElementType "br" ContentEmpty)
-- >>> parseOnly (runTokenParser tokenElementType) "<!ELEMENT container ANY>"
-- Right (ElementType "container" ContentAny)
tokenElementType :: CharParsing m => Monad m => TokenParser m ElementType
tokenElementType = TokenParser $ do
  tokenElementTypeOpen
  tokenWhitespace
  name <- tokenName
  tokenWhitespace
  content <- elementTypeContent
  optional tokenWhitespace
  tokenElementClose
  pure $ ElementType name content

-- | <https://www.w3.org/TR/REC-xml/#NT-AttlistDecl>
--
-- >>> parseOnly (runTokenParser tokenAttributeList) "<!ATTLIST termdef>"
-- Right (AttributeList "termdef")
tokenAttributeList :: CharParsing m => Monad m => TokenParser m AttributeList
tokenAttributeList = TokenParser $ do
  tokenAttributeListOpen
  tokenWhitespace
  name <- tokenName
  optional tokenWhitespace
  tokenElementClose
  pure $ AttributeList name

-- | <https://www.w3.org/TR/REC-xml/#NT-GEDecl>
--
-- >>> parseOnly (runTokenParser tokenGeneralEntity) "<!ENTITY d '&#xD;'>"
-- Right (GeneralEntity "d" [ContentReference (CharRef '\r')])
-- >>> parseOnly (runTokenParser tokenGeneralEntity) "<!ENTITY da '&#xD;&#xA;'>"
-- Right (GeneralEntity "da" [ContentReference (CharRef '\r'),ContentReference (CharRef '\n')])
-- >>> parseOnly (runTokenParser tokenGeneralEntity) "<!ENTITY Pub-Status 'This is a pre-release of the specification.'>"
-- Right (GeneralEntity "Pub-Status" [ContentText "This is a pre-release of the specification."])
tokenGeneralEntity :: CharParsing m => Monad m => TokenParser m GeneralEntity
tokenGeneralEntity = TokenParser $ do
  tokenEntityOpen
  tokenWhitespace
  name <- tokenName
  tokenWhitespace
  quote <- tokenQuote
  definition <- many (tokenContent $ quote:"%")
  char quote
  optional tokenWhitespace
  tokenElementClose
  return $ GeneralEntity name definition

-- | <https://www.w3.org/TR/REC-xml/#NT-PEDecl>
--
-- >>> parseOnly (runTokenParser tokenParameterEntity) "<!ENTITY % YN '\"Yes\"' >"
-- Right (ParameterEntity "YN" [ValueText "\"Yes\""])
tokenParameterEntity :: CharParsing m => Monad m => TokenParser m ParameterEntity
tokenParameterEntity = TokenParser $ do
  tokenEntityOpen
  tokenWhitespace
  tokenPercent
  tokenWhitespace
  name <- tokenName
  tokenWhitespace
  quote <- tokenQuote
  definition <- many (tokenEntityValue [quote])
  char quote
  optional tokenWhitespace
  tokenElementClose
  pure $ ParameterEntity name definition

-- | <https://www.w3.org/TR/REC-xml/#NT-NotationDecl>
--
-- >>> parseOnly (runTokenParser tokenNotation) "<!NOTATION foo SYSTEM \"identifier\">"
-- Right (Notation "foo" (SystemID "identifier"))
tokenNotation :: CharParsing m => Monad m => TokenParser m Notation
tokenNotation = TokenParser $ do
  tokenNotationOpen
  tokenWhitespace
  name <- tokenName
  tokenWhitespace
  externalEntity <- externalID -- TODO: support PUBLIC ID
  optional tokenWhitespace
  tokenElementClose
  pure $ Notation name externalEntity

-- | <https://www.w3.org/TR/REC-xml/#dt-pi>
tokenInstruction :: CharParsing m => Monad m => TokenParser m Instruction
tokenInstruction = TokenParser instruction

-- | <https://www.w3.org/TR/REC-xml/#NT-Comment>
tokenComment :: CharParsing m => Monad m => TokenParser m Text
tokenComment = TokenParser comment

-- | Parse any 'Token'.
anyToken :: CharParsing m => Monad m => TokenParser m Token
anyToken = TokenElementType <$> tokenElementType
  <|> TokenAttributeList <$> tokenAttributeList
  <|> TokenGeneralEntity <$> tokenGeneralEntity
  <|> TokenParameterEntity <$> tokenParameterEntity
  <|> TokenNotation <$> tokenNotation
  <|> TokenInstruction <$> tokenInstruction
  <|> TokenComment <$> tokenComment
