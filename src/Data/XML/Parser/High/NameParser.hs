{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
-- | All documentation examples assume the following setup:
--
-- > :set -XOverloadedStrings
-- > import Data.Attoparsec.ByteString
-- > import Data.XML.Parser.High
module Data.XML.Parser.High.NameParser
  ( NameParser(..)
  , anyName
  , anyNameExcept
  ) where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Compat
import           Control.Monad.Fail.Compat
import           Data.String
import           Data.XML.Parser.Low
import           Prelude.Compat

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.ByteString
-- >>> import Data.XML.Parser.High

-- | How to parse tag names.
newtype NameParser a = NameParser { runNameParser :: QName -> Either String a }

deriving instance Functor NameParser
deriving via (WrappedArrow (Kleisli (Either String)) QName) instance Applicative NameParser

-- | Can be combined with @\<|\>@
deriving via (WrappedArrow (Kleisli (Either String)) QName) instance Alternative NameParser

-- | Match a single 'QName' in a concise way.
--
-- >>> parseOnly (runTokenParser $ tag' "foo" anyAttr anyContent) "<foo></foo>"
-- Right ()
instance (a ~ ()) => IsString (NameParser a) where
  fromString s = anyName >>= \(QName _ name) ->
    when (fromString s /= name) $ fail $ "Expected tag named " <> show s <> ", instead got: " <> show name

-- | Can be combined with @>>=@. Qualified name is forwarded without change.
instance Monad NameParser where
  (NameParser f) >>= g = NameParser $ \name -> do
    a <- f name
    let NameParser g' = g a
    g' name

instance MonadFail NameParser where
  fail message = NameParser $ const $ Left message

-- | Match any qualified name.
anyName :: NameParser QName
anyName = NameParser Right

-- | Match any qualified name, except for the given value.
--
-- >>> parseOnly (runTokenParser $ tag' (anyNameExcept "foo") anyAttr anyContent) "<foo></foo>"
-- Left ...
-- >>> parseOnly (runTokenParser $ tag' (anyNameExcept "foo") anyAttr anyContent) "<bar></bar>"
-- Right ()
anyNameExcept :: QName -> NameParser QName
anyNameExcept name = NameParser $ \name' -> if name == name'
  then Left $ "Expected any tag name except " <> show name
  else Right name'
