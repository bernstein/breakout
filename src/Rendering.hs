{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , DeriveDataTypeable
           , ViewPatterns
  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Rendering
-- Copyright   :  (c) 2012 Andreas-C. Bernstein
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  andreas.bernstein@gmail.com
--
-- Rendering.
--
-----------------------------------------------------------------------------

module Rendering
       (
         Ctx(..)
       , createCtx
       , renderPic
       , Picture
       , GameGfx(..)
       ) where

import UnitBox
import GLutils
import Foreign.Ptr (nullPtr)
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.Typeable
import Control.Monad.Reader
import Diagrams.TwoD.Types
import Diagrams.Prelude
import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GL

data GameGfx = GameGfx
  deriving (Eq,Ord,Read,Show,Typeable)

data Ctx = Ctx {
    setFillColor :: (GL.GLfloat,GL.GLfloat,GL.GLfloat,GL.GLfloat) -> IO ()
  , drawBox :: T2 -> IO ()
  }

instance Monoid (Render GameGfx R2) where
  mempty  = G $ return ()
  (G rd1) `mappend` (G rd2) = G (rd1 >> rd2)

type RenderM a = ReaderT Ctx IO a

instance Backend GameGfx R2 where
  data Render GameGfx R2 = G (RenderM ())
  type Result GameGfx R2 = Ctx -> IO ()
  data Options GameGfx R2 = GameGfxOptions

  withStyle _ s _ (G r) = G $ do
    glStyle s
    r
  doRender _ _ (G r) = runReaderT r

glStyle :: Style v -> RenderM ()
glStyle s = do
  ctx <- ask
  sequence_ . catMaybes $ [ handle (fColor ctx) ]
  where handle :: AttributeClass a => (a -> RenderM ()) -> Maybe (RenderM ())
        handle f = f `fmap` getAttr s
        fColor ctx c = lift $ setFillColor ctx (toVec4 (getFillColor c))

        toVec4 :: Color c => c -> (GL.GLfloat,GL.GLfloat,GL.GLfloat,GL.GLfloat)
        toVec4 c = 
            let (r,g,b,a) = colorToRGBA c
                a'        = case getOpacity <$> getAttr s of
                              Nothing -> a
                              Just d  -> a * d
            in  (realToFrac r,realToFrac g,realToFrac b,realToFrac a')

instance Renderable UnitBox GameGfx where
  render _ (UnitBox t) = G $ do
    ctx <- ask
    lift $ drawBox ctx t

createCtx :: IO Ctx
createCtx = do
  p <- GL.glCreateProgram
  let vs = "uniform mat4 modelViewMatrix;\nuniform mat4 projectionMatrix;\nvoid main(void) { gl_Position = gl_ProjectionMatrix * modelViewMatrix * gl_Vertex; }"
      fs = "uniform vec4 fillColor;\nvoid main(void) { gl_FragColor = fillColor; }"
  v <- shader GL.gl_VERTEX_SHADER vs
  -- vs <- GL.get (compileStatus v)
  -- vi <- GL.get (shaderInfoLog v)
  f <- shader GL.gl_FRAGMENT_SHADER fs
  -- fs <- GL.get (compileStatus f)
  -- fi <- GL.get (shaderInfoLog f)
  GL.glAttachShader p v 
  GL.glAttachShader p f 
  GL.glLinkProgram  p
  -- ps <- GL.get linkStatus
  -- pi <- GL.get (programInfoLog p)

  GL.glUseProgram p
  fcLoc <- uniformLoc p "fillColor"
  mvLoc <- uniformLoc p "modelViewMatrix"
  pjLoc <- uniformLoc p "projectionMatrix"

  vao <- createVAO

  return Ctx {
    setFillColor = \(r,g,b,a) -> GL.glUniform4f fcLoc r g b a
  , drawBox = \t ->
      let (unr2 -> (a1,a2)) = apply t unitX
          (unr2 -> (b1,b2)) = apply t unitY
          (unr2 -> (c1,c2)) = transl t
          m = [realToFrac a1,realToFrac a2,0,realToFrac c1
              ,realToFrac b1,realToFrac b2,0,realToFrac c2
              ,0,0,1,0 
              ,0,0,0,1]
      in do
        uniformMatrix4fv mvLoc True m
        GL.glDrawElements GL.gl_TRIANGLES 6 GL.gl_UNSIGNED_BYTE nullPtr
  }

createVAO :: IO GL.GLuint
createVAO =
  let vertices :: [GL.GLfloat]
      vertices = [ -0.5, -0.5, 0.0, 1.0 ,
                   -0.5,  0.5, 0.0, 1.0 ,
                    0.5,  0.5, 0.0, 1.0 ,
                    0.5, -0.5, 0.0, 1.0 ]
      indices :: [GL.GLubyte]
      indices = [0,1,2,0,2,3]
  in do
    vao <- gen GL.glGenVertexArrays
    GL.glBindVertexArray vao

    vbo <- buffer GL.gl_ARRAY_BUFFER GL.gl_STATIC_DRAW vertices

    GL.glEnableVertexAttribArray 0
    GL.glVertexAttribPointer 0 4 GL.gl_FLOAT (fromIntegral GL.gl_FALSE) 0 nullPtr

    ibo <- buffer GL.gl_ELEMENT_ARRAY_BUFFER GL.gl_STATIC_DRAW indices 
   
    errorCheckValue <- GL.glGetError
      --when (errorCheckValue /= GL.gl_NO_ERROR)
      --  GLUT.reportErrors
    return vao

renderPic :: Diagram GameGfx R2 -> Ctx -> IO ()
renderPic = renderDia GameGfx GameGfxOptions

type Picture = Diagram GameGfx R2

symmetricFrustum :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat 
                -> [GL.GLfloat]
symmetricFrustum l b n f = 
  let r = -l
      t = -b
      fnInv = 1/(f-n)
  in  [ n/r, 0, 0, 0
      , 0, n/t, 0, 0
      , 0, 0, -(f+n)*fnInv, -1
      , 0, 0, -2*f*n*fnInv, 0]
