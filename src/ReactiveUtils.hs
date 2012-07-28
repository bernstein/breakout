{-# LANGUAGE ExistentialQuantification
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , FlexibleInstances 
           , TypeFamilies
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ReactiveUtils
-- Copyright   :  (c) 2012 Andreas-C. Bernstein
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  andreas.bernstein@gmail.com
--
-- Reactive banana utilities. Most of them very similar to the ones from
-- the Reactive library.
--
-----------------------------------------------------------------------------

module ReactiveUtils 
       (
         integral
       , sumB
       , withPrevE
       , withPrevEWith
       , diffE
       , unique
       , once
       ) where

import Reactive.Banana
import Data.VectorSpace
import Data.AffineSpace
import Data.Active (fromDuration, Time)

integral :: (VectorSpace v, Scalar v ~ Double) => Event t Time -> Behavior t v -> Behavior t v
integral t b = sumB $ (\v dt -> fromDuration dt *^ v) <$> b <@> diffE t

sumB :: AdditiveGroup a => Event t a -> Behavior t a
sumB = accumB zeroV . fmap (^+^)

withPrevE :: Event t a -> Event t (a,a)
withPrevE = withPrevEWith (,)

withPrevEWith :: (a -> a -> b) -> Event t a -> Event t b
withPrevEWith f e = filterJust . fst . mapAccum Nothing $ g <$> e
    where
    g y Nothing  = (Nothing     , Just y)
    g y (Just x) = (Just (f y x), Just y)

diffE :: AffineSpace a => Event t a -> Event t (Diff a)
diffE = withPrevEWith (.-.)

unique :: Eq a => Event t a -> Event t a
unique = filterJust . accumE Nothing . fmap (\a acc -> if Just a == acc then Nothing else Just a)

once :: Event t a -> Event t a
once e = whenE (True `stepper` (False <$ e)) e
