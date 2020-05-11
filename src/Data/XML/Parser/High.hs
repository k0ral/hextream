{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
-- | High-level XML parsers, built on top of "Data.XML.Parser.Mid":
--
-- - entity references are expanded
-- - CDATAs are abstracted away
-- - comments are ignored
-- - whitespace between tokens is ignored
-- - duplicate attributes are ignored
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.High
  ( module Data.XML.Parser.High.AttrParser
  , module Data.XML.Parser.High.NameParser
  , Prolog(..)
  , Token(..)
  , TokenParser()
  , ContentParser()
  , noContent
  , withContent
  , anyContent
  , runTokenParser
  , prolog
  , instruction
  , textContent
  , textContent'
  , tag
  , tag'
  , anyTag
  , anyToken
  , anyToken'
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Compat
import           Control.Monad.Fail.Compat
import           Data.Function
import           Data.Map                        (Map)
import qualified Data.Map                        as Map
import           Data.Maybe
import           Data.String
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.XML.Parser.High.AttrParser
import           Data.XML.Parser.High.NameParser
import           Data.XML.Parser.Low
import qualified Data.XML.Parser.Mid             as L1
import           Data.XML.Parser.Mid.Attribute
import           Prelude                         ()
import           Prelude.Compat
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.ParserCombinators.ReadP    (readP_to_S)


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | XML document prolog.
data Prolog = Prolog
  { prologXmlDeclaration :: Maybe L1.XMLDeclaration
  , prologInstructions   :: [L1.Instruction]
  , prologDoctype        :: Maybe L1.Doctype
  } deriving (Eq, Ord, Read, Show)

data Token
  = TokenProlog Prolog
  | TokenInstruction L1.Instruction
  | TokenTag QName (Map QName Text) [Token]
  | TokenTextContent Text
  deriving (Eq, Ord, Read, Show)


-- | A parser that consumes whole 'Token's.
newtype TokenParser m a = TokenParser { runTokenParser :: m a }

deriving instance Functor m => Functor (TokenParser m)
deriving instance Applicative m => Applicative (TokenParser m)
deriving instance Alternative m => Alternative (TokenParser m)
deriving instance Monad m => Monad (TokenParser m)

instance (Parsing m, Monad m) => MonadFail (TokenParser m) where
  fail = TokenParser . unexpected

-- | How to parse tag content.
data ContentParser m a
  = NoContent (m a)
  | AnyContent ([Token] -> m a)
  | WithContent (TokenParser m a)

deriving instance Functor m => Functor (ContentParser m)

-- | Assert that content is not empty, and parse it using given token parser.
--
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr (withContent textContent')) "<tag key='value'>body</tag>"
-- Right "body"
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr (withContent textContent')) "<tag key='value'></tag>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr (withContent textContent')) "<tag key='value'/>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr (withContent $ pure ())) "<tag key='value'/>"
-- Left ...
withContent :: TokenParser m a -> ContentParser m a
withContent = WithContent

-- | Assert that content is empty.
--
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr noContent) "<tag key='value'>body</tag>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr noContent) "<tag key='value'></tag>"
-- Right ()
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr noContent) "<tag key='value'/>"
-- Right ()
noContent :: Applicative m => ContentParser m ()
noContent = NoContent $ pure ()

-- | Accept any content, including empty content.
--
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr anyContent) "<tag key='value'>body</tag>"
-- Right ()
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr anyContent) "<tag key='value'></tag>"
-- Right ()
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr anyContent) "<tag key='value'/>"
-- Right ()
anyContent :: CharParsing m => Monad m => ContentParser m ()
anyContent = AnyContent $ const $ pure ()

-- | Parse a processing instruction.
--
-- <https://www.w3.org/TR/REC-xml/#dt-pi>
--
-- >>> parseOnly (runTokenParser instruction) "<?php echo 'Hello World!'; ?>"
-- Right (Instruction "php" "echo 'Hello World!'; ")
-- >>> parseOnly (runTokenParser instruction) "<!-- comments and whitespace are ignored -->  <?php echo 'Hello World!'; ?>"
-- Right (Instruction "php" "echo 'Hello World!'; ")
instruction :: CharParsing m => Monad m => TokenParser m L1.Instruction
instruction = TokenParser $ do
  skipCommentsWhitespace
  L1.runTokenParser L1.tokenInstruction

-- | <https://www.w3.org/TR/REC-xml/#NT-prolog>
--
-- >>> parseOnly (runTokenParser prolog) "<?xml version='1.0'?><!DOCTYPE greeting>"
-- Right (Prolog {prologXmlDeclaration = Just (XMLDeclaration "1.0" ...), prologInstructions = [], prologDoctype = Just (Doctype "greeting" ...)})
-- >>> parseOnly (runTokenParser prolog) "<?xml version='1.0'?>  <!-- comments and whitespace are ignored --><!DOCTYPE greeting>"
-- Right (Prolog {prologXmlDeclaration = Just (XMLDeclaration "1.0" ...), prologInstructions = [], prologDoctype = Just (Doctype "greeting" ...)})
prolog :: CharParsing m => Monad m => TokenParser m Prolog
prolog = TokenParser $ do
  xmlDeclaration <- optional $ L1.runTokenParser L1.tokenXmlDeclaration
  skipCommentsWhitespace
  instructions <- runTokenParser $ many instruction
  doctype <- optional $ do
    skipCommentsWhitespace
    L1.runTokenParser L1.tokenDoctype

  when (isNothing xmlDeclaration && null instructions && isNothing doctype)
    $ unexpected "Expected XML prolog"

  return $ Prolog xmlDeclaration instructions doctype


-- | Parse textual content of a tag, including CDATA.
textContent :: CharParsing m => Monad m => EntityDecoder -> TokenParser m Text
textContent entityDecoder = TokenParser $ mconcat <$> do
  skipComments
  (textualData <|> L1.runTokenParser L1.tokenCdata) `sepBy1` L1.runTokenParser L1.tokenComment
  where textualData = expandContents entityDecoder =<< L1.runTokenParser L1.tokenData

-- | Same as @textContent (decodePredefinedEntities <> decodeHtmlEntities)@, provided for convenience.
--
-- >>> parseOnly (runTokenParser $ tag' anyName anyAttr (withContent textContent')) "<tag>body<!-- Ignored comment --><![CDATA[<innertag>innerbody</innertag>]]></tag>"
-- Right "body<innertag>innerbody</innertag>"
textContent' :: CharParsing m => Monad m => TokenParser m Text
textContent' = textContent decodeStandardEntities


normalizeAttributes :: EntityDecoder -> [Attribute] -> Map QName Text
normalizeAttributes entityDecoder attributes = Map.fromList $ do
  Attribute name contents <- attributes
  value <- maybeToList $ expandContents entityDecoder contents
  return (name, value)

-- | Generic tag parser.
tag :: CharParsing m => Monad m
    => EntityDecoder             -- ^ How to expand entity references
    -> NameParser a              -- ^ How to parse tag name
    -> (a -> AttrParser b)       -- ^ How to parse tag attributes
    -> (b -> ContentParser m c)  -- ^ How to parse tag content
    -> TokenParser m c
tag entityDecoder parseName parseAttributes parseContent = parseStartToEnd <|> parseEmptyElement where
  parseStartToEnd = TokenParser $ do
    skipCommentsWhitespace
    L1.StartTag name attributes <- L1.runTokenParser L1.tokenStartTag
    a <- processName name
    b <- processAttributes a attributes
    c <- case parseContent b of
      NoContent f        -> f
      AnyContent f       -> f =<< runTokenParser (many $ anyToken entityDecoder)
      WithContent parser -> runTokenParser parser
    skipCommentsWhitespace
    L1.runTokenParser $ do
      name' <- L1.tokenEndTag
      when (name /= name') $ fail "Invalid end tag name"
    return c
  parseEmptyElement = TokenParser $ do
    skipCommentsWhitespace
    L1.EmptyElementTag name attributes <- L1.runTokenParser L1.tokenEmptyElementTag
    a <- processName name
    b <- processAttributes a attributes
    case parseContent b of
      NoContent f        -> f
      AnyContent f       -> f mempty
      WithContent parser -> unexpected "Expected non-empty tag"
  processName name = runNameParser parseName name
    & either unexpected return
  processAttributes state attributes = runAttrParser (parseAttributes state) (normalizeAttributes entityDecoder attributes)
    & either unexpected return

-- | Simplified version of 'tag':
--
-- - no state forwarding between name, attributes and content parsers
-- - uses @decodePredefinedEntities <> decodeHtmlEntities@ to expand entity references
tag' :: CharParsing m => Monad m
     => NameParser a       -- ^ How to parse tag name
     -> AttrParser b       -- ^ How to parse tag attributes
     -> ContentParser m c  -- ^ How to parse tag content
     -> TokenParser m c
tag' parseName parseAttributes parseBody = tag decodeStandardEntities parseName (const parseAttributes) (const parseBody)

-- | Parse a tag with any name, any attributes and any content.
--
-- >>> parseOnly (runTokenParser anyTag) "<tag key='value'>body</tag>"
-- Right ()
-- >>> parseOnly (runTokenParser anyTag) "<tag key='value'/>"
-- Right ()
-- >>> parseOnly (runTokenParser anyTag) "<!-- ignored comment --><tag key='value'/>"
-- Right ()
-- >>> parseOnly (runTokenParser anyTag) "<tag key='value'>body<!-- ignored comment --></tag>"
-- Right ()
anyTag :: CharParsing m => Monad m => TokenParser m ()
anyTag = tag' anyName anyAttr anyContent


-- | Parse any 'Token'.
anyToken :: CharParsing m => Monad m => EntityDecoder -> TokenParser m Token
anyToken entityDecoder = (TokenProlog <$> prolog)
  <|> (TokenInstruction <$> instruction)
  <|> tokenTag
  <|> (TokenTextContent <$> textContent entityDecoder)
  where tokenTag = tag entityDecoder anyName (\name -> (name,) <$> forwardAttrs) $ \(name, attributes) ->
          TokenTag name attributes <$> AnyContent pure
        forwardAttrs = AttrParser Right

-- | Same as @anyToken (decodePredefinedEntities <> decodeHtmlEntities)@, provided for convenience.
anyToken' :: CharParsing m => Monad m => TokenParser m Token
anyToken' = anyToken decodeStandardEntities


-- * Private functions

skipComments :: CharParsing m => Monad m => m ()
skipComments = void $ many $ L1.runTokenParser L1.tokenComment

skipCommentsWhitespace :: CharParsing m => Monad m => m ()
skipCommentsWhitespace = void $ many $ void (L1.runTokenParser L1.tokenComment) <|> void tokenWhitespace

decodeStandardEntities :: EntityDecoder
decodeStandardEntities = decodePredefinedEntities <> decodeHtmlEntities
