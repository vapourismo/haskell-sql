{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Language.SQL.Builder (
    Builder (..),
    buildCode,
    buildCodeSegments,
    buildParams,

    type (++),
    Function,
    BuilderAux,
    append,
    unnest,
    isolateParams,
    withParams
) where

import           Control.Monad.RWS.Strict

import qualified Data.ByteString          as B
import qualified Data.ByteString.UTF8     as UTF8
import           Data.String

-- | Query builder
data Builder ts p where
    Code  :: B.ByteString         -> Builder ts p        -> Builder ts       p
    Param :: p                    -> Builder ts p        -> Builder ts       p
    Nest  ::                         Builder ts (t -> p) -> Builder (t : ts) p
    Nil   ::                                                Builder '[]      p

-- | Unnest 'Builder'.
unnest :: BuilderAux ts => Builder (t : ts) p -> Builder ts (t -> p)
unnest segment =
    case segment of
        Code  code  rest -> Code  code          (unnest rest)
        Param param rest -> Param (const param) (unnest rest)
        Nest  inner      -> inner

-- | Append two type lists.
type family (++) ts us where
    (++) '[]      us = us
    (++) (t : ts) us = t : ts ++ us

-- | @Function '[p1, p2, ... pn] r@ constructs a type signature @p1 -> p2 -> ... pn -> r@.
type family Function ts r where
    Function '[]      r = r
    Function (t : ts) r = t -> Function ts r

-- | Auxiliary 'Builder'-related functions
class BuilderAux ts where
    -- | Show sub-components of the builder.
    showBuilder :: Show a => Builder ts a -> [String]

    -- | Map inner parameters.
    fmapBuilder :: (a -> b) -> Builder ts a -> Builder ts b

    -- | Append builders.
    append :: BuilderAux us => Builder ts p -> Builder us p -> Builder (ts ++ us) p

    -- | Retain only parameters.
    isolateParams :: Builder ts p -> Builder ts p

    -- | Do something with parameters inside the builder.
    withParams :: ([p] -> r) -> Builder ts p -> Function ts r

instance BuilderAux '[] where
    showBuilder segment =
        case segment of
            Code  code  rest -> UTF8.toString code                : showBuilder rest
            Param param rest -> ("<param: " ++ show param ++ ">") : showBuilder rest
            Nil              -> []

    fmapBuilder f segment =
        case segment of
            Code  code  rest -> Code  code      (f <$> rest)
            Param param rest -> Param (f param) (f <$> rest)
            Nil              -> Nil

    append segment rhs =
        case segment of
            Code  code  lhs -> Code  code  (append lhs rhs)
            Param param lhs -> Param param (append lhs rhs)
            Nil             -> rhs

    isolateParams segment =
        case segment of
            Code  _     rest -> isolateParams rest
            Param param rest -> Param param (isolateParams rest)
            Nil              -> Nil

    withParams ret segment =
        case segment of
            Code  _     rest -> withParams ret               rest
            Param param rest -> withParams (ret . (param :)) rest
            Nil              -> ret []

data DerivedStatic = DerivedStatic deriving Show

instance BuilderAux ts => BuilderAux (t : ts) where
    showBuilder segment =
        case segment of
            Code  code  rest -> UTF8.toString code                 : showBuilder rest
            Param param rest -> ("<static: " ++ show param ++ ">") : showBuilder rest
            Nest  inner      -> showBuilder (DerivedStatic <$ inner)

    fmapBuilder f segment =
        case segment of
            Code  code  rest -> Code  code      (f <$> rest)
            Param param rest -> Param (f param) (f <$> rest)
            Nest  inner      -> Nest (fmap f <$> inner)

    append segment rhs =
        case segment of
            Code  code  lhs -> Code  code  (append lhs rhs)
            Param param lhs -> Param param (append lhs rhs)
            Nest  inner     -> Nest (append inner (const <$> rhs))

    isolateParams segment =
        case segment of
            Code  _     rest -> isolateParams rest
            Param param rest -> Param param (isolateParams rest)
            Nest  inner      -> Nest (isolateParams inner)

    withParams ret segment input =
        case segment of
            Code  _     rest -> withParams ret               rest                  input
            Param value rest -> withParams (ret . (value :)) rest                  input
            Nest  inner      -> withParams ret               (($ input) <$> inner)

instance BuilderAux ts => Functor (Builder ts) where
    fmap = fmapBuilder

instance (BuilderAux ts, Show p) => Show (Builder ts p) where
    show builder = concat (showBuilder builder)

instance IsString (Builder ts p -> Builder ts p) where
    fromString str = Code (UTF8.fromString str)

instance IsString (Builder '[] p) where
    fromString str = Code (UTF8.fromString str) Nil

instance Monoid (Builder '[] p) where
    mempty = Code B.empty Nil

    mappend = append

-- | Read-write-state-transformer which gathers the built code.
buildCodeSegments :: Builder ts p -> RWS (Word -> B.ByteString) B.ByteString Word ()
buildCodeSegments segment =
    case segment of
        Code code rest -> do
            tell code
            buildCodeSegments rest

        Param _ rest -> do
            index <- get
            code <- asks ($ index)
            tell code
            put (index + 1)
            buildCodeSegments rest

        Nest inner -> buildCodeSegments inner

        Nil -> pure ()

-- | Gather the code from the Builder syntax tree.
buildCode :: (Word -> B.ByteString) -> Builder ts p -> B.ByteString
buildCode placeholder segment =
    snd (evalRWS (buildCodeSegments segment) placeholder 0)

-- | @buildParams sql@ produces a function which collects all the necessary parameters for the
-- query described in @sql@.
buildParams :: BuilderAux ts => Builder ts p -> Function ts [p]
buildParams =
    withParams id
