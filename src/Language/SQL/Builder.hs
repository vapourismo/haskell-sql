{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Builder (
    buildCode,
    buildCodeSegments,
    buildParams,
    buildQuery,
    buildPrepQuery,

    ParamsFunc,
    WithValues (..),
    IsolateParams (..)
) where

import qualified Data.ByteString    as B
import           Data.Tagged

import           Language.SQL.Types
import           Language.SQL.Value

-- | Gather the code segments from the SQL syntax tree.
buildCodeSegments :: (Word -> B.ByteString) -> Word -> SQL v ts -> [B.ByteString]
buildCodeSegments placeholder index segment =
    case segment of
        Code   code rest -> code              : buildCodeSegments placeholder index       rest
        Static _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Param  _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Nil              -> []

-- | Gather the code from the SQL syntax tree.
buildCode :: forall v ts. Value v => SQL v ts -> B.ByteString
buildCode segments =
    B.concat (buildCodeSegments (untag (placeholderCode @v)) 0 segments)

-- | @buildParams sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildParams :: WithValues ts => SQL v ts -> ParamsFunc ts [v]
buildParams =
    withValues id

-- | @buildQuery sql@ produces a function which collects all the parameters need by @sql@ in order
-- instantiate a 'Query'.
buildQuery :: (Value v, WithValues ts) => SQL v ts -> ParamsFunc ts (Query v)
buildQuery segment =
    withValues (Query (buildCode segment)) segment

-- | Build a 'PrepQuery' instance using the given query.
buildPrepQuery :: (Value v, IsolateParams ts) => SQL v ts -> PrepQuery v ts
buildPrepQuery segment =
    PrepQuery (buildCode segment) (isolateParams segment)
