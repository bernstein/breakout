{-# LANGUAGE TypeFamilies
           , FlexibleContexts
           , ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  UnitBox
-- Copyright   :  (c) 2012 Andreas-C. Bernstein
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  andreas.bernstein@gmail.com
--
-- Unit box.
--
-----------------------------------------------------------------------------

module UnitBox
       (
         UnitBox(..)
       , unitBox
       ) where

--import Graphics.Rendering.Diagrams
import Diagrams.Core
import Diagrams.TwoD.Types
import Diagrams.TwoD.Shapes (unitSquare)
import Diagrams.Path (Path)
import Data.Semigroup

data UnitBox = UnitBox T2

type instance V UnitBox = R2

instance Transformable UnitBox where
  transform t1 (UnitBox t2) = UnitBox (t1 <> t2)

unitBox :: (Backend b R2, Renderable UnitBox b) => Diagram b R2
unitBox = mkQD (Prim $ UnitBox mempty)
                (getEnvelope r)
                mempty
                mempty
                (Query quadQuery)
  where quadQuery (unp2 -> (x,y)) = Any $ abs x <= 1 && abs y <= 1
        r :: Path R2
        r = unitSquare
