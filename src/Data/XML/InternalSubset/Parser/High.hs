-- | High-level parsers for doctype's internal subset, built on top of "Data.XML.InternalSubset.Parser.Mid":
--
-- - comments are ignored
-- - whitespace between tokens is ignored
module Data.XML.InternalSubset.Parser.High where

import           Data.XML.InternalSubset.Parser.Mid as Mid
import           Data.XML.Parser.Low
import           Data.XML.Parser.Mid.Comment
import           Data.XML.Parser.Mid.Instruction
import           Text.Parser.Char
import           Text.Parser.Combinators


-- | <https://www.w3.org/TR/REC-xml/#NT-intSubset>
data InternalSubset = InternalSubset
  { _elementTypes      :: [ElementType]
  , _attributeLists    :: [AttributeList]
  , _generalEntities   :: [GeneralEntity]
  , _parameterEntities :: [ParameterEntity]
  , _notations         :: [Notation]
  , _instructions      :: [Instruction]
  } deriving(Eq, Ord, Read, Show)

emptyInternalSubset :: InternalSubset
emptyInternalSubset = InternalSubset mempty mempty mempty mempty mempty mempty

-- | <https://www.w3.org/TR/REC-xml/#NT-intSubset>
internalSubset :: CharParsing m => Monad m => m InternalSubset
internalSubset = do
  tokens <- Mid.runTokenParser Mid.anyToken `sepBy` tokenWhitespace
  pure $ foldr insert emptyInternalSubset tokens
  where insert (TokenElementType e) is     = is { _elementTypes = e : _elementTypes is }
        insert (TokenAttributeList a) is   = is { _attributeLists = a : _attributeLists is }
        insert (TokenGeneralEntity g) is   = is { _generalEntities = g : _generalEntities is }
        insert (TokenParameterEntity p) is = is { _parameterEntities = p : _parameterEntities is }
        insert (TokenNotation n) is        = is { _notations = n : _notations is }
        insert (TokenInstruction i) is     = is { _instructions = i : _instructions is }
        insert (TokenComment c) is         = is
