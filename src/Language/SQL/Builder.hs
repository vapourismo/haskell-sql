module Language.SQL.Builder (
    Element (..),
    mapHole,
    mapCode,

    Builder (..),
    hole,
    code,
    mapHoles,
    mapCodes,

    flatten
) where

import           Prelude          hiding (foldr, id, (.))

import           Control.Arrow
import           Control.Category
import           Control.Monad

import           Data.Foldable
import           Data.Profunctor
import qualified Data.Sequence    as Seq
import           Data.String

-- | Builder element
data Element code arr input output
    = Hole (arr input output)
    | Code code

instance Functor (arr input) => Functor (Element code arr input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Hole gen)  = Hole (mapOutput <$> gen)
    fmap _         (Code code) = Code code

instance Profunctor arr => Profunctor (Element code arr) where
    {-# INLINE dimap #-}

    dimap mapInput mapOutput (Hole gen)  = Hole (dimap mapInput mapOutput gen)
    dimap _        _         (Code code) = Code code

    {-# INLINE lmap #-}

    lmap mapInput (Hole gen)  = Hole (lmap mapInput gen)
    lmap _        (Code code) = Code code

    {-# INLINE rmap #-}

    rmap mapOutput (Hole gen)  = Hole (rmap mapOutput gen)
    rmap _         (Code code) = Code code

{-# INLINE mapHole #-}

-- | Transform the 'Hole', if it is one.
mapHole
    :: (arr input output -> arr' input' output')
    -> Element code arr  input  output
    -> Element code arr' input' output'
mapHole trans (Hole gen)  = Hole (trans gen)
mapHole _     (Code code) = Code code

{-# INLINE mapCode #-}

-- | Transform the 'Code', if it is one.
mapCode :: (code -> code') -> Element code arr input output -> Element code' arr input output
mapCode _     (Hole hole) = Hole hole
mapCode trans (Code code) = Code (trans code)

-- | Builder
newtype Builder code arr input output =
    Builder { unBuilder :: Seq.Seq (Element code arr input output) }

instance (Monoid code, Show code, IsString code) => Show (Builder code arr input output) where
    show builder =
        show (fst (flatten (\ index _ -> fromString ('$' : show (index + 1))) builder))

instance Monoid (Builder code arr input output) where
    {-# INLINE mempty #-}

    mempty = Builder Seq.empty

    {-# INLINE mappend #-}

    mappend (Builder lhs) (Builder rhs) = Builder (lhs Seq.>< rhs)

    {-# INLINE mconcat #-}

    mconcat builders = Builder (mconcat (unBuilder <$> builders))

instance Functor (arr input) => Functor (Builder code arr input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Builder elements) = Builder (fmap mapOutput <$> elements)

instance Profunctor arr => Profunctor (Builder code arr) where
    {-# INLINE dimap #-}

    dimap mapInput mapOutput (Builder elements) = Builder (dimap mapInput mapOutput <$> elements)

    {-# INLINE lmap #-}

    lmap mapInput (Builder elements) = Builder (lmap mapInput <$> elements)

    {-# INLINE rmap #-}

    rmap mapOutput (Builder elements) = Builder (rmap mapOutput <$> elements)

instance Category arr => Category (Builder code arr) where
    {-# INLINE id #-}

    id = Builder (Seq.singleton (Hole id))

    Builder lhs . Builder rhs =
        Builder (join (fillHole <$> rhs))
        where
            fillHole (Hole gen)  = mapHole (. gen) <$> lhs
            fillHole (Code code) = Seq.singleton (Code code)

instance Arrow arr => Arrow (Builder code arr) where
    {-# INLINE arr #-}

    arr f = hole (arr f)

    {-# INLINE first #-}

    first = mapHoles first

    {-# INLINE second #-}

    second = mapHoles second

instance IsString code => IsString (Builder code arr input output) where
    fromString str = code (fromString str)

{-# INLINE hole #-}

-- | Produce a 'Builder' that only consists of a 'Hole'.
hole :: arr input output -> Builder code arr input output
hole gen = Builder (Seq.singleton (Hole gen))

{-# INLINE code #-}

-- | Produce a 'Builder' that only consists of 'Code'.
code :: code -> Builder code arr input output
code code = Builder (Seq.singleton (Code code))

{-# INLINE mapHoles #-}

-- | Map all the 'Hole's inside the 'Builder'.
mapHoles
    :: (arr input output -> arr' input' output')
    -> Builder code arr  input  output
    -> Builder code arr' input' output'
mapHoles trans (Builder elements) = Builder (mapHole trans <$> elements)

{-# INLINE mapCodes #-}

-- | Map all the 'Code's inside the 'Builder'.
mapCodes :: (code -> code') -> Builder code arr input output -> Builder code' arr input output
mapCodes trans (Builder elements) = Builder (mapCode trans <$> elements)

-- | Flatten 'Builder'.
flatten
    :: Monoid code
    => (Word -> arr input output -> code)
    -> Builder code arr input output
    -> (code, [arr input output])
flatten holeCode (Builder elements) =
    (code, toList gens)
    where
        (_, code, gens) = foldl' f (0, mempty, Seq.empty) elements

        f (index, codes, gens) elem =
            case elem of
                Hole gen ->
                    ( index + 1
                    , mappend (holeCode index gen) codes
                    , gens Seq.|> gen )

                Code code ->
                    ( index
                    , mappend code codes
                    , gens )
