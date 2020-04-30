{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString.Streaming
import           Data.ByteString.Streaming
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.XML.Parser.High                 as XML
import           Data.XML.Parser.Low.Entity           as XML
import           Prelude                              hiding (readFile)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parser.Combinators

main :: IO ()
main = defaultMain $ testGroup "Unit tests"
  [ feedCase
  ]

feedCase :: TestTree
feedCase = testCase "Feed" $ do
  (result, _) <- runResourceT $ readFile "test/feed.xml" & parse (runTokenParser parser)
  result @?= Right "http://example.org/2003/12/13/atom03"

  where
  parser = do
    prolog
    tag' "feed" anyAttr $ withContent $ do
      many $ tag' (anyNameExcept "entry") anyAttr anyContent
      url <- tag' "entry" anyAttr $ withContent $ do
        many $ tag' (anyNameExcept "link") anyAttr anyContent
        url <- tag decodeHtmlEntities "link" (const $ attrValue "href") (anyContent $>)
        many anyTag
        return url
      many anyTag
      return url
