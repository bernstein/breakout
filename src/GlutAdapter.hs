{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  GLUTAdapter
-- Copyright   :  (c) Andreas-Christoph Bernstein 2012
-- License     :  BSD3
-- 
-- Maintainer  :  andreas.bernstein@googlemail.com
-- Stability   :  experimental
-- 
-- Connect Reactive.Banana + GLUT
----------------------------------------------------------------------

module GlutAdapter
  ( adapt
  , UI(..)
  , Key(..)
  ) where

import Reactive.Banana
import qualified Graphics.UI.GLUT as GLUT
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Data.Time as T
import qualified Data.Active as Active

data UI t = UI {
    mouse :: Behavior t GLUT.Position
  , key   :: Event t (GLUT.KeyState, Key)
  , frame :: Event t ()
  , windowSize :: Behavior t GLUT.Size
}

data Key = Char Char | SpecialKey GLUT.SpecialKey
    deriving (Eq, Ord, Show)

--adapt :: (forall t. Behavior t Active.Time -> UI t -> NetworkDescription t (Behavior t (IO ()))) -> IO ()
adapt :: (forall t. Behavior t Active.Time -> UI t -> Behavior t (IO ())) -> IO ()
adapt f = do
  start <- T.getCurrentTime
  (tickHandler,    tickSink)    <- newAddHandler
  (mouseHandler,   mouseSink)   <- newAddHandler
  (keyHandler,     keySink)     <- newAddHandler

  let getTime = Active.toTime . flip T.diffUTCTime start <$> T.getCurrentTime

  network <- compile $ do
    eTick    <- fromAddHandler tickHandler
    bMouse   <- fromChanges (GLUT.Position 0 0) mouseHandler
    bTime    <- fromPoll getTime
    eKey     <- fromAddHandler keyHandler
    bWinWize <- fromPoll (GLUT.get GLUT.windowSize)
    -- beh    <- f eTick bTime (UI bMouse eKey)
    let beh = withClearAndSwap <$> f bTime (UI bMouse eKey eTick bWinWize)
    reactimate (beh <@ eTick)
  actuate network
  GLUT.passiveMotionCallback GLUT.$= Just mouseSink
  GLUT.motionCallback        GLUT.$= Just mouseSink
  GLUT.keyboardMouseCallback GLUT.$= Just ( \k ks _ _ ->
    case (k,ks) of
      (GLUT.Char       c,GLUT.Down) -> keySink (GLUT.Down, Char       c)
      (GLUT.SpecialKey s,GLUT.Down) -> keySink (GLUT.Down, SpecialKey s)
      (GLUT.Char       c,GLUT.Up  ) -> keySink (GLUT.Up  , Char       c)
      (GLUT.SpecialKey s,GLUT.Up  ) -> keySink (GLUT.Up  , SpecialKey s)
      _ -> return ()
      )
  let resizeScene :: GLUT.Size -> IO ()
      resizeScene (GLUT.Size w 0) = resizeScene (GLUT.Size w 1)
      resizeScene (GLUT.Size w h) = do
        GL.glViewport 0 0 w h
        GL.glMatrixMode GL.gl_PROJECTION
        GL.glLoadIdentity
        let (w',h') = (fromIntegral w, fromIntegral h)
            aspect = h'/w'
        GL.glOrtho (-1) 1 (-aspect) aspect (-1) 1

  GLUT.reshapeCallback GLUT.$= Just resizeScene
                                        
  timer 10 (tickSink ())
  GLUT.mainLoop
  
timer :: GLUT.Timeout -> IO () -> IO ()
timer ms act = GLUT.addTimerCallback ms (act >> timer ms act)

withClearAndSwap :: IO () -> IO ()
withClearAndSwap act = 
  do  GL.glClear . sum . map fromIntegral $ [GL.gl_COLOR_BUFFER_BIT, GL.gl_DEPTH_BUFFER_BIT]
      act
      GLUT.swapBuffers
