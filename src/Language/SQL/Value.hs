{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Value (
    Value (..),
    Param (..)
) where

import qualified Data.ByteString    as B
import           Data.Tagged

import           Language.SQL.Types

-- |
class Value v where
    -- | A function that produces placeholder code for a given index. Indices start at 0.
    placeholderCode :: Tagged v (Word -> B.ByteString)

-- |
class Param v a where
    -- | Generate 'SQL' for a static param.
    staticParam :: a -> SQL v '[]
    staticParam a = Static (toValue a) Nil

    -- | Generate 'SQL' for a dynamic param.
    dynamicParam :: SQL v '[a]
    dynamicParam = Param toValue Nil

    -- | Convert to value.
    toValue :: a -> v
