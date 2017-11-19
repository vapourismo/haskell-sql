module Language.SQL.Builder (
    Builder,
    hole,
    code,
    combineHoles,
    mapHoles,
    mapCodes,

    flatten
) where

import           Prelude          hiding (foldr, id, (.))

import           Control.Arrow
import           Control.Category
import           Control.Monad

import           Data.Profunctor
import qualified Data.Sequence    as Seq
import           Data.String

-- | Builder element
data Element code cat input output
    = Hole (cat input output)
    | Code code

instance Functor (cat input) => Functor (Element code cat input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Hole gen)  = Hole (mapOutput <$> gen)
    fmap _         (Code code) = Code code

instance Profunctor cat => Profunctor (Element code cat) where
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
    :: (cat input output -> cat' input' output')
    -> Element code cat  input  output
    -> Element code cat' input' output'
mapHole trans (Hole gen)  = Hole (trans gen)
mapHole _     (Code code) = Code code

{-# INLINE mapCode #-}

-- | Transform the 'Code', if it is one.
mapCode :: (code -> code') -> Element code cat input output -> Element code' cat input output
mapCode _     (Hole hole) = Hole hole
mapCode trans (Code code) = Code (trans code)

-- | Builder
newtype Builder code cat input output =
    Builder { unBuilder :: Seq.Seq (Element code cat input output) }

instance (Monoid code, Show code, IsString code) => Show (Builder code cat input output) where
    show builder =
        show (flatten id (\ _ -> fromString "?") builder)

instance Monoid (Builder code cat input output) where
    {-# INLINE mempty #-}

    mempty = Builder Seq.empty

    {-# INLINE mappend #-}

    mappend (Builder lhs) (Builder rhs) = Builder (lhs Seq.>< rhs)

    {-# INLINE mconcat #-}

    mconcat builders = Builder (mconcat (unBuilder <$> builders))

instance Functor (cat input) => Functor (Builder code cat input) where
    {-# INLINE fmap #-}

    fmap mapOutput (Builder elements) = Builder (fmap mapOutput <$> elements)

instance Profunctor cat => Profunctor (Builder code cat) where
    {-# INLINE dimap #-}

    dimap mapInput mapOutput (Builder elements) = Builder (dimap mapInput mapOutput <$> elements)

    {-# INLINE lmap #-}

    lmap mapInput (Builder elements) = Builder (lmap mapInput <$> elements)

    {-# INLINE rmap #-}

    rmap mapOutput (Builder elements) = Builder (rmap mapOutput <$> elements)

instance Category cat => Category (Builder code cat) where
    {-# INLINE id #-}

    id = Builder (Seq.singleton (Hole id))

    {-# INLINE (.) #-}

    (.) = combineHoles (.)

instance Arrow cat => Arrow (Builder code cat) where
    {-# INLINE arr #-}

    arr f = hole (arr f)

    {-# INLINE first #-}

    first = mapHoles first

    {-# INLINE second #-}

    second = mapHoles second

instance ArrowZero cat => ArrowZero (Builder code cat) where
    {-# INLINE zeroArrow #-}

    zeroArrow = hole zeroArrow

instance ArrowPlus cat => ArrowPlus (Builder code cat) where
    {-# INLINE (<+>) #-}

    (<+>) = combineHoles (<+>)

instance ArrowChoice cat => ArrowChoice (Builder code cat) where
    left = mapHoles left

    right = mapHoles right

    (+++) = combineHoles (+++)

    (|||) = combineHoles (|||)

instance IsString code => IsString (Builder code cat input output) where
    fromString str = code (fromString str)

{-# INLINE hole #-}

-- | Produce a 'Builder' that only consists of a 'Hole'.
hole :: cat input output -> Builder code cat input output
hole gen = Builder (Seq.singleton (Hole gen))

{-# INLINE code #-}

-- | Produce a 'Builder' that only consists of 'Code'.
code :: code -> Builder code cat input output
code code = Builder (Seq.singleton (Code code))

-- | Combine the holes of two 'Builder's.
combineHoles
    :: (arr0 input0 output0 -> arr1 input1 output1 -> arr2 input2 output2)
    -> Builder code arr0 input0 output0
    -> Builder code arr1 input1 output1
    -> Builder code arr2 input2 output2
combineHoles morph (Builder lhs) (Builder rhs) =
    Builder (join (fillHole <$> rhs))
    where
        fillHole (Hole gen)  = mapHole (`morph` gen) <$> lhs
        fillHole (Code code) = Seq.singleton (Code code)

{-# INLINE mapHoles #-}

-- | Map all the 'Hole's inside the 'Builder'.
mapHoles
    :: (cat input output -> cat' input' output')
    -> Builder code cat  input  output
    -> Builder code cat' input' output'
mapHoles trans (Builder elements) = Builder (mapHole trans <$> elements)

{-# INLINE mapCodes #-}

-- | Map all the 'Code's inside the 'Builder'.
mapCodes :: (code -> code') -> Builder code cat input output -> Builder code' cat input output
mapCodes trans (Builder elements) = Builder (mapCode trans <$> elements)

-- | Flatten 'Builder'.
flatten
    :: Monoid m
    => (code -> m)
    -> (cat input output -> m)
    -> Builder code cat input output
    -> m
flatten fromCode fromHole (Builder elements) =
    foldMap from elements
    where
        from (Code code) = fromCode code
        from (Hole hole) = fromHole hole
