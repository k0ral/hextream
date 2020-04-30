{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import           Control.Monad.Trans.Resource
import           Data.Attoparsec.ByteString.Streaming
import           Data.ByteString.Streaming
import           Data.Default
import           Data.Function
import qualified Data.Text.Lazy.Encoding              as Lazy
import           Data.XML.Parser.High                 as High
import           Data.XML.Parser.Mid                  as Mid
import           Prelude                              hiding (readFile)
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.Golden                    (findByExtension, goldenVsString)
import           Text.Pretty.Simple

main :: IO ()
main = defaultMain =<< do
  xmlFiles <- findByExtension [".xml"] "."

  return $ testGroup "Golden tests" $ do
    xmlFile <- xmlFiles
    return $ testGroup xmlFile [ testMidToken xmlFile, testHighToken xmlFile ]

testMidToken xmlFile = goldenVsString xmlFile goldenFile $ f xmlFile where
 goldenFile = addExtension xmlFile ".mid.golden"
 f file = do
   (result, _) <- runResourceT $ readFile file & parse (Mid.runTokenParser $ some Mid.anyToken)
   return $ Lazy.encodeUtf8 $ pShowNoColor result

testHighToken xmlFile = goldenVsString xmlFile goldenFile $ f xmlFile where
 goldenFile = addExtension xmlFile ".high.golden"
 f file = do
   (result, _) <- runResourceT $ readFile file & parse (High.runTokenParser $ some High.anyToken')
   return $ Lazy.encodeUtf8 $ pShowNoColor result
