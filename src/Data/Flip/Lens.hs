{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module:       $HEADER$
-- Description:  Flip arguments of a type.
-- Copyright:    (c) 2013-2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude, depends on non-portable module
--
-- Extend "Data.Flip" module with lenses.
module Data.Flip.Lens
    ( module Data.Flip

    -- * Lenses
    , flipped
    , unflipped
    )
    where

import Data.Functor (Functor)

import Data.Function.Between ((<~@~))

import Data.Flip


-- | Lens for 'Flip'. Using type definition from /lens/ package this function
-- would have type:
--
-- > flip :: Lens (Flip g b a) (Flip h d c) (g a b) (h c d)
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details.
unflipped :: Functor f => (g a b -> f (h c d)) -> Flip g b a -> f (Flip h d c)
unflipped = Flip <~@~ unFlip
{-# INLINEABLE unflipped #-}

-- | Lens for 'Flip'. Using type definition from /lens/ package this function
-- would have type:
--
-- > flip :: Lens (Flip g b a) (Flip h d c) (g a b) (h c d)
--
-- See /lens/ <http://hackage.haskell.org/package/lens> package for details.
flipped :: Functor f => (Flip g b a -> f (Flip h d c)) -> g a b -> f (h c d)
flipped = unFlip <~@~ Flip
{-# INLINEABLE flipped #-}
