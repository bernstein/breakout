{-# LANGUAGE ScopedTypeVariables
           , FlexibleInstances
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) 2012 Andreas-C. Bernstein
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  andreas.bernstein@gmail.com
--
-- Main.
--
-----------------------------------------------------------------------------

module Main (main)
where

import GlutAdapter
import Rendering
import UnitBox
import ReactiveUtils

import Control.Arrow (first,second)
import System.Exit (exitSuccess)
import qualified Data.Active as Active
import Diagrams.Prelude
import Reactive.Banana as R
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL.Raw as GL

main :: IO ()
main = do
  _ <- GLUT.getArgsAndInitialize

  GLUT.initialDisplayMode GLUT.$= [ GLUT.DoubleBuffered
                                , GLUT.RGBAMode
                                , GLUT.WithDepthBuffer
                                , GLUT.WithAlphaComponent ] 
  GLUT.initialWindowSize GLUT.$= GLUT.Size 800 600
  _ <- GLUT.createWindow "paddleball"
  GL.glViewport 0 0 800 600
  let aspect = 600/800
  GL.glOrtho (-1) 1 (-aspect) aspect (-1) 1
  GL.glClearColor 1 1 1 1
  GL.glClearDepth 1
  GL.glDepthFunc GL.gl_LESS
  GL.glEnable GL.gl_BLEND
  GL.glBlendFunc GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA

  ctx <- createCtx
  adapt (game ctx)

type Velocity = R2

instance Semigroup (Behavior t Picture) where
  (<>) = liftA2 (<>)

instance Monoid (Behavior t Picture) where
  mempty = pure mempty
  mappend = (<>)

game :: forall t. Ctx -> Behavior t Active.Time -> UI t -> Behavior t (IO ())
game ctx time ui =
  let quit = exitSuccess <$ filterE (\(_,k) -> (k == GlutAdapter.Char '\27')) (key ui)

      scene :: Behavior t Picture
      scene = mconcat [paddleB ui, ball, pure wallPic]

      -- ball
      ballPos = (p2 (0,0.5) .+^) <$> integral (time <@ frame ui) ballVel
      ballVel = r2 (0.4,0.5) `accumB` collision
      ball    = moveTo <$> ballPos <*> pure ballPic

      collision = R.unions [wallHit ballPos (frame ui)]
  in  (>>) <$> (display ctx <$> scene) <*> stepper (return ()) quit

display :: Ctx -> Picture -> IO ()
display ctx dia = renderPic dia ctx

fitMouseX :: GLUT.Size -> GLUT.Position -> Double
fitMouseX (GLUT.Size w _) (GLUT.Position x _) = 2*(fromIntegral x / fromIntegral w) - 1
  
paddlePic :: Picture
paddlePic = unitBox # fc blue # scaleY 0.02 # scaleX 0.1

ballPic :: Picture
ballPic = unitBox # fc gray # scale 0.04

paddleB :: UI t -> Behavior t Picture
paddleB ui = translateX <$> (fitMouseX <$> windowSize ui <*> mouse ui) <*> pure paddlePic

brickPic :: Picture
brickPic = unitBox # scaleY 0.03 # scaleX 0.15

wallPic :: Picture
wallPic = moveTo (p2 (0,1.02)) ceilingPic 
       <> moveTo (p2 (-1.0,0.5)) sidePic
       <> moveTo (p2 (1.0,0.5)) sidePic

ceilingPic :: Picture
ceilingPic = unitBox # scaleX 2 # scaleY 0.04 # fc grey

sidePic :: Picture
sidePic = unitBox # scaleX 0.04  # fc grey

wallHit :: Behavior t P2 -> Event t () -> Event t (Velocity -> Velocity)
wallHit ballPos frame = R.unions [top,left,right,bottom]
  where
    top    = checkWall ((>1.0).snd) (second (negate.abs))
    left   = checkWall ((< -1.0).fst) (first abs)
    right  = checkWall ((>1.0).fst) (first (negate.abs))
    bottom = checkWall ((<0.0).snd) (second abs)
    checkWall check mod = (r2.mod.unr2) <$ whenE (check.unp2 <$> ballPos) frame

positionX :: GLUT.Position -> Double
positionX (GLUT.Position x _) = fromIntegral x
