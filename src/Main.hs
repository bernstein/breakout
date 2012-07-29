{-# LANGUAGE ScopedTypeVariables
           , FlexibleInstances
           , ViewPatterns
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

import Data.Maybe (isJust)
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
      scene = mconcat [ paddle, ball
                      , pure wallPic
                      , bricks smashed]

      -- ball
      ballPos = (p2 (0,0.5) .+^) <$> integral (time <@ frame ui) ballVel
      ballVel = r2 (0.4,0.5) `accumB` collision
      ball    = moveTo <$> ballPos <*> pure ballPic

      paddle :: Behavior t Picture
      paddle = moveTo <$> paddlePos <*> pure paddlePic

      paddlePos :: Behavior t P2
      paddlePos = curry p2 <$> (fitMouseX <$> windowSize ui <*> mouse ui) <*> pure 0

      smashed = smash ballPos (frame ui)

      collision = R.unions [ wallHit ballPos (frame ui)
                           , paddleHit paddlePos ballPos (frame ui)
                           , R.unions smashed
                           ]

  in  (>>) <$> (display ctx <$> scene) <*> stepper (return ()) quit

display :: Ctx -> Picture -> IO ()
display ctx dia = renderPic dia ctx

fitMouseX :: GLUT.Size -> GLUT.Position -> Double
fitMouseX (GLUT.Size w _) (GLUT.Position x _) = 2*(fromIntegral x / fromIntegral w) - 1
  
paddlePic :: Picture
paddlePic = unitBox # fc blue # scaleY 0.02 # scaleX 0.1

ballPic :: Picture
ballPic = unitBox # fc gray # scale 0.04

paddleHit :: Behavior t P2 -> Behavior t P2 -> Event t () 
          -> Event t (Velocity -> Velocity)
paddleHit paddlePos ballPos frame =
  let c = colliding paddlePic <$> paddlePos <*> pure ballPic <*> ballPos
      response v@(unr2 -> (vx,vy)) = if vy < 0 then r2 (vx,-vy) else v
  in  response <$ whenE c frame

brickPic :: Picture
brickPic = unitBox # scaleY 0.04 # scaleX 0.2 # fc yellow

brick :: P2 -> Event t (Velocity -> Velocity) -> Behavior t Picture
brick p e = moveTo p brickPic `stepper` (mempty <$ e)

brickPositions :: [P2]
brickPositions = map (p2 . flip (,) 0.98) [-0.9,-0.7..0.9]

brickHit :: P2 -> Behavior t P2 -> Event t () -> Event t (Velocity -> Velocity)
brickHit p ballPos frame = once response
  where
    c   = whenE (colliding brickPic p ballPic <$> ballPos) frame
    response = (r2.second (negate.abs).unr2) <$ c

smash :: Behavior t P2 -> Event t () -> [Event t (Velocity -> Velocity)]
smash ballPos frame = map (\p -> brickHit p ballPos frame) brickPositions

bricks :: [Event t (Velocity -> Velocity)] -> Behavior t Picture
bricks smashed = mconcat $ zipWith brick brickPositions smashed

ceilingPic :: Picture
ceilingPic = unitBox # scaleX 2 # scaleY 0.04 # fc grey

sidePic :: Picture
sidePic = unitBox # scaleX 0.04  # fc grey

wallPic :: Picture
wallPic = moveTo ceilingPos ceilingPic 
       <> moveTo wallLeftPos sidePic
       <> moveTo wallRightPos sidePic

ceilingPos,wallLeftPos,wallRightPos :: P2
ceilingPos = p2 (0,1.02)
wallLeftPos = p2 (-1.0,0.5)
wallRightPos = p2 (1.0,0.5)

wallHit :: Behavior t P2 -> Event t () -> Event t (Velocity -> Velocity)
wallHit ballPos frame = R.unions [top,left,right]
  where
    cTop   = colliding ceilingPic ceilingPos ballPic <$> ballPos
    top    = (r2.second (negate.abs).unr2) <$ whenE cTop frame

    cLeft  = colliding sidePic wallLeftPos ballPic <$> ballPos
    left   = (r2.first abs.unr2) <$ whenE cLeft frame

    cRight = colliding sidePic wallRightPos ballPic <$> ballPos
    right  = (r2.first (negate.abs).unr2) <$ whenE cRight frame

positionX :: GLUT.Position -> Double
positionX (GLUT.Position x _) = fromIntegral x

-- work around for bug in boundingBox, for more info:
-- http://code.google.com/p/diagrams/issues/detail?id=87
-- will be removed!
colliding :: Picture -> P2 -> Picture -> P2 -> Bool
colliding d1 p1 d2 p2 = isJust $ intersection (bbox d1 p1) (bbox d2 p2)

bbox :: Picture -> P2 -> BoundingBox R2
bbox pic p = moveTo p (boundingBox pic)
