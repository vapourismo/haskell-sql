{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}

module Language.SQL.Types (
    SQL (..),

    Query (..),
    PrepQuery (..),

    AppendSQL (..),
    ParamsFunc,
    IsolateParams (..),
    WithValues (..)
) where

import qualified Data.ByteString      as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.String

-- | Basic SQL syntax tree
data SQL v ts where
    Code   :: B.ByteString -> SQL v ts -> SQL v ts
    Static :: v            -> SQL v ts -> SQL v ts
    Param  :: (t -> v)     -> SQL v ts -> SQL v (t : ts)
    Nil    ::                             SQL v '[]

instance IsString (SQL v ts -> SQL v ts) where
    fromString str = Code (UTF8.fromString str)

instance IsString (SQL v '[]) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (SQL v '[]) where
    mempty = Code B.empty Nil

    mappend = appendSql

type family (++) ts us where
    (++) '[]      us = us
    (++) (t : ts) us = t : ts ++ us

class AppendSQL ts where
    appendSql :: SQL v ts -> SQL v us -> SQL v (ts ++ us)

instance AppendSQL '[] where
    appendSql segment rhs =
        case segment of
            Code   code  lhs -> Code   code  (appendSql lhs rhs)
            Static value lhs -> Static value (appendSql lhs rhs)
            Nil              -> rhs

instance AppendSQL ts => AppendSQL (t : ts) where
    appendSql segment rhs =
        case segment of
            Code   code    lhs -> Code   code    (appendSql lhs rhs)
            Static value   lhs -> Static value   (appendSql lhs rhs)
            Param  toValue lhs -> Param  toValue (appendSql lhs rhs)

class IsolateParams ts where
    -- | Isolate the parameters from the query.
    isolateParams :: SQL v ts -> SQL v ts

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

-- | A 'ParamFunc' collects all parameters necessary to produce a set of values that need to go with
-- a query.
type family ParamsFunc ts r where
    ParamsFunc '[]      r = r
    ParamsFunc (t : ts) r = t -> ParamsFunc ts r

class WithValues ts where
    -- | Do something with the values that need to be passed alongside a given query.
    withValues :: ([v] -> r) -> SQL v ts -> ParamsFunc ts r

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

-- | Query
data Query v =
    Query { queryCode   :: B.ByteString
          , queryParams :: [v] }

deriving instance (Show v) => Show (Query v)
deriving instance (Eq v)   => Eq (Query v)
deriving instance (Ord v)  => Ord (Query v)

-- | Preparable query
data PrepQuery v ts =
    PrepQuery { prepQueryCode   :: B.ByteString
              , prepQueryParams :: SQL v ts }
