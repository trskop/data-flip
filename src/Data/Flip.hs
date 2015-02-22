{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#ifdef LANGUAGE_DERIVE_GENERIC
{-# LANGUAGE DeriveGeneric #-}
#endif

#ifdef LANGUAGE_POLY_KINDS
{-# LANGUAGE PolyKinds #-}
#endif

-- |
-- Module:       $HEADER$
-- Description:  Flip arguments of a type.
-- Copyright:    (c) 2013-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  CPP, FlexibleInstances, FlexibleContexts, NoImplicitPrelude,
--               and optionally: DeriveDataTypeable, DeriveGeneric, and
--               PolyKinds
--
-- Flip arguments of a type, this enables to create instances like
-- @'Functor' ('Flip' (,) a)@. Unfortunately it requires few language
-- extensions, namely @FlexibleInstances@ and @FlexibleContexts@.
module Data.Flip
    (
    -- * Flip Data Type
    --
    -- | Newtype that allows functor instances with flipped last two type
    -- variables. Unfortunately it requires @FlexibleInstances@ and sometimes
    -- also @FlexibleContexts@ language extensions.
    --
    -- Note that @'Flip' 'Const' b a@ is isomorphic to @'Tagged' b a@ where
    -- 'Tagged' is from @Data.Tagged@ from /tagged/
    -- <http://hackage.haskell.org/package/tagged> package.
      Flip(..)
    , flipmap
    , (>$<)
    , (>$$<)

    -- * Utility functions
    --
    -- | Properties that hold:
    --
    -- * @'unwrapFlip' . 'mapFlip' = 'id'@
    --
    -- * @'mapFlip' . 'unwrapFlip' = 'id'@
    --
    -- Analogic properties hold for 'mapFlip2' and 'unwrapFlip2'.
    , mapFlip
    , mapFlip2
    , unwrapFlip
    , unwrapFlip2
    , withFlip
    , withFlip2
    )
    where

import Control.Applicative (Applicative(..), Const(..))
import Control.Monad (Monad((>>), (>>=), return))
import Data.Either (Either(Left, Right))
import Data.Eq (Eq)
import Data.Function ((.), flip)
import Data.Functor (Functor(fmap))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord)
import Data.Tuple (fst, snd)
import Text.Read (Read)
import Text.Show (Show)

#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
import Data.Typeable (Typeable)
#endif

#ifdef LANGUAGE_DERIVE_GENERIC
import GHC.Generics (Generic)
#endif

#ifdef WITH_COMONAD
import Control.Comonad (Comonad(..))
#endif

#ifdef WITH_DEEPSEQ
import Control.DeepSeq (NFData(rnf))
#endif


import Data.Function.Between ((~@~))


-- | Flip type variables.
newtype Flip f a b = Flip {unFlip :: f b a}
  deriving
    ( Eq, Ord, Read, Show
#ifdef LANGUAGE_DERIVE_DATA_TYPEABLE
    , Typeable
#endif
#ifdef LANGUAGE_DERIVE_GENERIC
    , Generic
#endif
    )

#ifdef WITH_DEEPSEQ
instance NFData (f b a) => NFData (Flip f a b) where
    rnf (Flip x) = rnf x
#endif

instance Functor (Flip Either a) where
    fmap f (Flip (Left x))  = Flip (Left (f x))
    fmap _ (Flip (Right x)) = Flip (Right x)
    {-# INLINE fmap #-}

instance Functor (Flip (,) a) where
    fmap f (Flip (x, y)) = Flip (f x, y)
    {-# INLINE fmap #-}

instance Monoid a => Applicative (Flip (,) a) where
    pure x = Flip (x, mempty)
    Flip (f, u) <*> Flip (x, v) = Flip (f x, u `mappend` v)

instance Applicative (Flip Either a) where
    pure = Flip . Left
    Flip (Right x) <*> _ = Flip (Right x)
    Flip (Left  f) <*> x = fmap f x

instance Monad (Flip Either a) where
    return = Flip . Left

    -- :: Flip Either a b -> (b -> Flip Either a c) -> Flip Either a c
    Flip (Right x) >>= _ = Flip (Right x)
    Flip (Left x)  >>= f = f x

instance Functor (Flip Const s) where
    -- :: (a -> b) -> Flip Const s a -> Flip Const s b
    fmap = mapFlip . (Const ~@~ getConst)

instance Applicative (Flip Const s) where
    -- :: a -> Flip Const s a
    pure = Flip . Const

    -- :: Flip Const s (a -> b) -> Flip Const s a -> Flip Const s b
    Flip (Const f) <*> x = fmap f x

instance Monad (Flip Const s) where
    -- :: a -> Flip Const s a
    return = Flip . Const
    {-# INLINE return #-}

    -- :: Flip Const s a -> (a -> Flip Const s b) -> Flip Const s b
    Flip (Const x) >>= f = f x
    {-# INLINE (>>=) #-}

    -- :: Flip Const s a -> Flip Const s b -> Flip Const s b
    _ >> x = x
    {-# INLINE (>>) #-}

#ifdef WITH_COMONAD
instance Comonad (Flip (,) a) where
    duplicate (Flip p) = Flip (Flip p, snd p)
    {-# INLINE duplicate #-}
    extract (Flip p) = fst p
    {-# INLINE extract #-}

instance Comonad (Flip Const s) where
    -- :: Flip Const s a -> Flip Const s (Flip Const s a)
    duplicate c = Flip (Const c)
    {-# INLINE duplicate #-}

    -- :: Flip Const s a -> a
    extract (Flip (Const x)) = x
    {-# INLINE extract #-}
#endif

-- | Lift transformation of inner functor in to transofrmation of 'Flip'
-- functor.
mapFlip :: (f a b -> g c d) -> Flip f b a -> Flip g d c
mapFlip = Flip ~@~ unFlip
{-# INLINE mapFlip #-}

-- | Variant of 'mapFlip' for functions with arity two.
mapFlip2
    :: (f1 a1 b1 -> f2 a2 b2 -> f3 a3 b3)
    -> Flip f1 b1 a1 -> Flip f2 b2 a2 -> Flip f3 b3 a3
mapFlip2 = mapFlip ~@~ unFlip
{-# INLINE mapFlip2 #-}

-- | Inverse function to 'mapFlip'.
unwrapFlip :: (Flip f a b -> Flip g c d) -> f b a -> g d c
unwrapFlip = unFlip ~@~ Flip
{-# INLINE unwrapFlip #-}

-- | Inverse function to 'mapFlip2'.
unwrapFlip2
    :: (Flip f1 b1 a1 -> Flip f2 b2 a2 -> Flip f3 b3 a3)
    -> f1 a1 b1 -> f2 a2 b2 -> f3 a3 b3
unwrapFlip2 = unwrapFlip ~@~ Flip
{-# INLINE unwrapFlip2 #-}

-- | Take function that expects @'Flip' f a b@ as its argument and return
-- function that takes unwrapped @f b a@ instead.
--
-- This function can be used in situations when we expect function to use
-- instance for @'Flip' f a b@ instad of instance for @f b a@, in example:
--
-- > withFlip extract :: Comonad (Flip f a) => f c a -> c
withFlip :: (Flip f a b -> c) -> f b a -> c
withFlip = (. Flip) -- = id ~@~ Flip
{-# INLINE withFlip #-}

-- | As 'withFlip', but unwraps first two arguments for specified function.
withFlip2 :: (Flip f a b -> Flip g c d -> e) -> f b a -> g d c -> e
withFlip2 = withFlip ~@~ Flip
{-# INLINE withFlip2 #-}

-- | Like 'fmap', but uses flipped Functor instance.  Short hand for
-- @'unwrapFlip' . 'fmap'@.
flipmap :: Functor (Flip f a) => (b -> c) -> f b a -> f c a
flipmap = unwrapFlip . fmap
{-# INLINE flipmap #-}

-- | Infix variant of 'flipmap'.
--
-- This colides with:
--
-- > (>$<) :: Contravariant f => (a -> b) -> f b -> f a
--
-- Defined in /contravariant/
-- <http://hackage.haskell.org/package/contravariant> package as of version
-- 0.1.3.
(>$<) :: Functor (Flip f a) => (b -> c) -> f b a -> f c a
(>$<) = flipmap
infixl 4 >$<
{-# INLINE (>$<) #-}
-- Same fixity as (<$>) = fmap.

-- | Infix variant of 'flipmap' with arguments reversed.
--
-- This colides with:
--
-- > (>$$<) :: Contravariant f => f b -> (a -> b) -> f a
--
-- Defined in /contravariant/
-- <http://hackage.haskell.org/package/contravariant> package as of version
-- 0.1.3.
(>$$<) :: Functor (Flip f a) => f b a -> (b -> c) -> f c a
(>$$<) = flip flipmap
infixl 4 >$$<
{-# INLINE (>$$<) #-}
-- Same fixity as (<$>) = fmap.
