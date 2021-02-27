-- | Mid-level parsers for XML instructions.
--
-- All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
module Data.XML.Parser.Mid.Instruction where

import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.XML.Parser.Low
import           Text.Parser.Char
import           Text.Parser.Combinators

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString

-- | Processing instruction.
--
-- <https://www.w3.org/TR/REC-xml/#dt-pi>
data Instruction = Instruction Text Text
  deriving (Eq, Ord, Read, Show)


-- | <https://www.w3.org/TR/REC-xml/#dt-pi>
--
-- >>> parseOnly instruction "<?xml-stylesheet type='text/xsl' href='style.xsl'?>"
-- Right (Instruction "xml-stylesheet" "type='text/xsl' href='style.xsl'")
instruction :: CharParsing m => Monad m => m Instruction
instruction = do
  name <- tokenInstructionOpen
  tokenWhitespace
  content <- manyTill anyChar $ try tokenInstructionClose
  return $ Instruction name $ Text.pack content
