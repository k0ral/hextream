-- | Low-level parsers for doctype's internal subset:
--
-- - parsed tokens are small and may overlap; it is not possible to tokenize doctype internal-subset in a stateless way
-- - parsers are reversible: all formatting details are retained (e.g. whitespacing)
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.InternalSubset.Parser.Low
  ( module Data.XML.Parser.Low.Name
  , module Data.XML.Parser.Low.Reference
  , module Data.XML.InternalSubset.Parser.Low
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Data.XML.Parser.Low.Name
import           Data.XML.Parser.Low.Reference
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#NT-EntityValue>
data EntityValue = ValueText Text | ValueParameterEntityRef ParameterEntityRef | ValueReference Reference
  deriving (Eq, Ord, Read, Show)

-- | Parameter entity reference
newtype ParameterEntityRef = ParameterEntityRef Text
  deriving (Eq, Ord, Read, Show)

-- | @<!ENTITY@
tokenEntityOpen :: CharParsing m => m ()
tokenEntityOpen = void $ string "<!ENTITY"

-- | @<!ELEMENT@
tokenElementTypeOpen :: CharParsing m => m ()
tokenElementTypeOpen = void $ string "<!ELEMENT"

-- | @EMPTY@
tokenElementTypeContentEmpty :: CharParsing m => m ()
tokenElementTypeContentEmpty = void $ string "EMPTY"

-- | @ANY@
tokenElementTypeContentAny :: CharParsing m => m ()
tokenElementTypeContentAny = void $ string "ANY"

-- | @<!ATTLIST@
tokenAttributeListOpen :: CharParsing m => m ()
tokenAttributeListOpen = void $ string "<!ATTLIST"

-- | @%@
tokenPercent :: CharParsing m => m ()
tokenPercent = void $ char '%'

-- | @<!NOTATION@
tokenNotationOpen :: CharParsing m => m ()
tokenNotationOpen = void $ string "<!NOTATION"

-- | @PUBLIC@
tokenPublic :: CharParsing m => m ()
tokenPublic = void $ string "PUBLIC"

-- | <https://www.w3.org/TR/REC-xml/#NT-PEReference>
--
-- >>> parseOnly tokenParameterEntityRef "%foo;"
-- Right (ParameterEntityRef "foo")
tokenParameterEntityRef :: CharParsing m => Monad m => m ParameterEntityRef
tokenParameterEntityRef = ParameterEntityRef <$> (char '%' *> tokenName <* char ';')

-- | <https://www.w3.org/TR/REC-xml/#NT-EntityValue>
tokenEntityValue :: CharParsing m => Monad m => String -> m EntityValue
tokenEntityValue forbiddenChars = (ValueText . Text.pack <$> some (noneOf $ '%':'&':forbiddenChars))
  <|> (ValueParameterEntityRef <$> tokenParameterEntityRef)
  <|> (ValueReference <$> tokenReference)
