-- | Mid-level parsers for XML comments
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.Comment where

import           Control.Monad.Compat
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.XML.Parser.Low
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | <https://www.w3.org/TR/REC-xml/#NT-Comment>
--
-- >>> parseOnly comment "<!-- declarations for <head> & <body> -->"
-- Right " declarations for <head> & <body> "
-- >>> parseOnly comment "<!-- B+, B, or B--->"
-- Right " B+, B, or B-"
comment :: CharParsing m => Monad m => m Text
comment = do
  tokenCommentOpen
  content <- manyTill anyChar $ try tokenCommentClose
  return $ Text.pack content
