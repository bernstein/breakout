{-# LANGUAGE ScopedTypeVariables
  #-}

module Main
where

import GlutAdapter
import Rendering
import UnitBox

import System.Exit (exitSuccess)
import qualified Data.Active as Active
import Diagrams.Prelude
import Reactive.Banana
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

game :: forall t. Ctx -> Behavior t Active.Time -> UI t -> Behavior t (IO ())
game ctx time ui =
  let quit = exitSuccess <$ filterE (\(_,k) -> (k == GlutAdapter.Char '\27')) (key ui)
      scene :: Behavior t Picture
      scene = paddleB ui
  in  (>>) <$> (display ctx <$> scene) <*> stepper (return ()) quit

display :: Ctx -> Picture -> IO ()
display ctx dia = renderPic dia ctx

fitMouse :: GLUT.Size -> GLUT.Position -> Double
fitMouse (GLUT.Size w _) (GLUT.Position x _) = 2*(fromIntegral x / fromIntegral w) - 1
  
paddlePic :: Picture
paddlePic = unitBox # fc blue # scaleY 0.02 # scaleX 0.1

paddleB :: UI t -> Behavior t Picture
paddleB ui = translateX <$> (fitMouse <$> windowSize ui <*> mouse ui) <*> pure paddlePic

brickPic :: Picture
brickPic = unitBox # scaleY 0.03 # scaleX 0.15

positionX :: GLUT.Position -> Double
positionX (GLUT.Position x _) = fromIntegral x
