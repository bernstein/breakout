{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  SdlAdapter
-- Copyright   :  (c) Andreas-Christoph Bernstein 2012
-- License     :  BSD3
-- 
-- Maintainer  :  andreas.bernstein@gmail.com
-- Stability   :  experimental
-- 
-- Connect Reactive.Banana + SDL
--
-- adapted GameLoop by Heinrich Apfelmus
----------------------------------------------------------------------

module SdlAdapter
  ( adapt
  , Input(..)
  , Key
  , KeyState(..)
  , GameNetworkDescription
  ) where

import Control.Monad (unless, replicateM_)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.OpenGL.Raw as GL
--import qualified Data.Time as T
import qualified Data.Active as Active

data Input t = Input {
    mouse :: Behavior t (Int, Int)
  , mouseButton   :: Event t (KeyState, MouseButton)
  , key   :: Event t (KeyState, Key)
}

data KeyState = Press | Release deriving (Eq, Ord, Show)

--data Key = Char Char | SpecialKey SDL.SDLKey deriving (Eq, Ord, Show)
type Key = SDL.SDLKey

type MouseButton = SDL.MouseButton
--  deriving (Eq, Ord, Show)

fps :: Integer
fps = 30

type Duration = Integer
type Time     = Integer

type GameNetworkDescription t
    = Event t ()             -- ^ physics timer
    -> Behavior t Active.Time -- ^ clock
    -> Input t               -- ^ user input
    -> NetworkDescription t (Behavior t (IO ())) -- ^ graphics to be sampled

adapt :: Duration 
    -> (forall t. GameNetworkDescription t)
    -> IO ()
adapt dt game = do
  let getRealTime = fromIntegral <$> SDL.getTicks

  -- set up events
  (gfxHandler,    gfxSink)     <- newAddHandler
  (tickHandler,   tickSink)    <- newAddHandler
  (mouseHandler,  mouseSink)   <- newAddHandler
  (buttonHandler, buttonSink)  <- newAddHandler
  (keyHandler,    keySink)     <- newAddHandler
  clock <- newIORef 0

  let 
    fireInput SDL.NoEvent = return ()
    fireInput (SDL.KeyDown (SDL.Keysym k [] _)) = keySink (Press, k)
    fireInput (SDL.KeyUp   (SDL.Keysym k [] _)) = keySink (Release,   k)
    fireInput (SDL.MouseMotion x y _ _) = mouseSink (fromIntegral x, fromIntegral y)
    fireInput (SDL.MouseButtonDown _ _ b) = buttonSink (Press,b)
    fireInput (SDL.MouseButtonUp _ _ b)   = buttonSink (Release,b)
    fireInput _ = return ()

    processEvents = do
        input <- SDL.pollEvent
        unless (input == SDL.NoEvent) $ do
          fireInput input
          processEvents
 
    go clock' acc old = do
      new <- getRealTime
      processEvents

      let (n,acc2) = (new - old + acc) `divMod` dt
      replicateM_ (fromIntegral n) $ do
            modifyIORef clock' (+dt)
            tickSink ()
      -- SDL.delay (fromIntegral (dt `div` 3))

      tempclock <- readIORef clock'
      modifyIORef clock' (+acc2)
      gfxSink ()
      writeIORef clock' tempclock
      go clock' acc2 new

  -- compile and run event network
  network <- compile $ do
    bMouse   <- fromChanges (0,0) mouseHandler
    eLbp     <- fromAddHandler buttonHandler
    eKey     <- fromAddHandler keyHandler
    bTime    <- fromPoll (((/1000) . Active.toTime) <$> readIORef clock)
    eGfx     <- fromAddHandler gfxHandler
    eTick    <- fromAddHandler tickHandler

    bGfx     <- game eTick bTime (Input bMouse eLbp eKey)
    reactimate ((withClearAndSwap <$> bGfx) <@ eGfx)
  actuate network

  go clock 0 =<< getRealTime

withClearAndSwap :: IO () -> IO ()
withClearAndSwap act = 
  do  GL.glClear . sum . map fromIntegral $ [GL.gl_COLOR_BUFFER_BIT, GL.gl_DEPTH_BUFFER_BIT]
      act
      SDL.glSwapBuffers
