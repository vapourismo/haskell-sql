{-# LANGUAGE TypeFamilies #-}

module Language.SQL.Backend where

import qualified Data.ByteString as B
import           Data.Tagged

-- | Backend type cerification
class Backend b where
    -- | A data type which gets passed along with query statements to fill the placeholder within
    -- the query statement.
    data Value b

    -- | A function that produces placeholder code for a given index. Indices start at 0.
    placeholderCode :: Tagged b (Word -> B.ByteString)
