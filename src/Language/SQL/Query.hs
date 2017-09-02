{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Query (
    SQL (..),
    buildCode,
    buildCodeSegments,
    buildParams,

    Query (..),
    buildQuery,

    PrepQuery (..),
    buildPrepQuery,

    ParamsFunc,
    WithValues (..),
    IsolateParams (..)
) where

import qualified Data.ByteString      as B
import           Data.Tagged

import           Language.SQL.Backend

-- | Basic SQL syntax tree
data SQL b ts where
    Code   :: B.ByteString   -> SQL b ts -> SQL b ts
    Static :: Value b        -> SQL b ts -> SQL b ts
    Param  :: (t -> Value b) -> SQL b ts -> SQL b (t : ts)
    Nil    :: SQL b '[]

-- | Gather the code segments from the SQL syntax tree.
buildCodeSegments :: (Word -> B.ByteString) -> Word -> SQL b ts -> [B.ByteString]
buildCodeSegments placeholder index segment =
    case segment of
        Code   code rest -> code              : buildCodeSegments placeholder index       rest
        Static _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Param  _    rest -> placeholder index : buildCodeSegments placeholder (index + 1) rest
        Nil              -> []

-- | Gather the code from the SQL syntax tree.
buildCode :: forall b ts. Backend b => SQL b ts -> B.ByteString
buildCode segments =
    B.concat (buildCodeSegments (untag (placeholderCode @b)) 0 segments)

-- | A 'ParamFunc' collects all parameters necessary to produce a set of values that need to go with
-- a query.
type family ParamsFunc ts r where
    ParamsFunc '[]      r = r
    ParamsFunc (t : ts) r = t -> ParamsFunc ts r

class WithValues ts where
    -- | Do something with the values that need to be passed alongside a given query.
    withValues :: ([Value b] -> r) -> SQL b ts -> ParamsFunc ts r

instance WithValues '[] where
    withValues ret segment =
        case segment of
            Static value rest -> withValues (ret . (value :)) rest
            Code   _     rest -> withValues ret               rest
            Nil               -> ret []

instance WithValues ts => WithValues (t : ts) where
    withValues ret segment param =
        case segment of
            Static value   rest -> withValues (ret . (value         :)) rest param
            Param  toValue rest -> withValues (ret . (toValue param :)) rest
            Code   _       rest -> withValues ret                       rest param

-- | @buildParams sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildParams :: WithValues ts => SQL b ts -> ParamsFunc ts [Value b]
buildParams =
    withValues id

-- | Query
data Query b =
    Query { queryCode   :: B.ByteString
          , queryParams :: [Value b] }

-- | @buildQuery sql@ produces a function which collects all the parameters need by @sql@ in order
-- instantiate a 'Query'.
buildQuery :: (Backend b, WithValues ts) => SQL b ts -> ParamsFunc ts (Query b)
buildQuery segment =
    withValues (Query (buildCode segment)) segment

class IsolateParams ts where
    -- | Isolate the parameters from the query.
    isolateParams :: SQL b ts -> SQL b ts

instance IsolateParams '[] where
    isolateParams segment =
        case segment of
            Code   _     rest -> isolateParams rest
            Static value rest -> Static value (isolateParams rest)
            Nil               -> Nil

instance IsolateParams (t : ts) where
    isolateParams segment =
        case segment of
            Code   _       rest -> isolateParams rest
            Static value   rest -> Static value   rest
            Param  toValue rest -> Param  toValue rest


-- | PrepQuery
data PrepQuery b ts =
    PrepQuery { prepQueryCode   :: B.ByteString
              , prepQueryParams :: SQL b ts }

-- | Build a 'PrepQuery' instance using the given query.
buildPrepQuery :: (Backend b, IsolateParams ts) => SQL b ts -> PrepQuery b ts
buildPrepQuery segment =
    PrepQuery (buildCode segment) (isolateParams segment)
