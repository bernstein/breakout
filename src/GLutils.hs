module GLutils
  (
    gens, gen
  , uniformMatrix4fv
  , buffer

  -- Shader
  , setShaderSource
  , shader
  , compileStatus
  , shaderInfoLog

  -- Program
  , uniformLoc
  , linkStatus
  , programInfoLog
  , getProgrami
  , getActiveUniformsi
  , getActiveUniformsiv

  , getIntegerv
  ) where

import qualified Graphics.Rendering.OpenGL.Raw.Core31 as GL

import Foreign.Marshal (allocaArray, peekArray, withArray, withMany)
import Foreign.Ptr (castPtr, Ptr, nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.C.String (withCAStringLen, withCAString, peekCAString)

type GLProgram = GL.GLuint
type GLShader = GL.GLuint

-- -----------------------------------------------------------------------------

gens :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> Int -> IO GL.GLuint
gens what n = fmap head $ allocaArray n $ \buf -> what (fromIntegral n) buf >> peekArray n buf

gen :: (GL.GLsizei -> Ptr GL.GLuint -> IO ()) -> IO GL.GLuint
gen what = gens what 1

uniformMatrix4fv :: GL.GLint -> Bool -> [GL.GLfloat] -> IO ()
uniformMatrix4fv loc b m = withArray m (GL.glUniformMatrix4fv loc 1 (fromIntegral . fromEnum $ b))

size :: (Num b, Storable a) => [a] -> b
size as = fromIntegral (length as * sizeOf (head as))

buffer :: (Storable a) => GL.GLenum -> GL.GLenum -> [a] -> IO GL.GLuint
buffer target usage xs = do 
  b <- gen GL.glGenBuffers
  GL.glBindBuffer target b
  withArray xs $ \ptr -> GL.glBufferData target (size xs) ptr usage
  return b

type GLStringLen = (Ptr GL.GLchar, GL.GLsizei)

withGLStringLen :: String -> (GLStringLen -> IO a) -> IO a
withGLStringLen s act = withCAStringLen s $ 
  \(p,len) -> act (castPtr p, fromIntegral len)

-- | 'setShaderSource' is a wrapper around glShaderSource.
setShaderSource :: GL.GLuint -> [String] -> IO ()
setShaderSource sh srcs = do
   let len = fromIntegral . length $ srcs
   withMany withGLStringLen srcs $ \charBufsAndLengths -> do
      let (charBufs, lengths) = unzip charBufsAndLengths
      withArray charBufs $ \charBufsBuf ->
         withArray (map fromIntegral lengths) $ \lengthsBuf ->
            GL.glShaderSource sh len charBufsBuf lengthsBuf

-- | 'shader'
shader :: GL.GLenum -> String -> IO GL.GLuint
shader ty src = do
  s <- GL.glCreateShader ty
  setShaderSource s [src]
  GL.glCompileShader s
  return s

-- | Was the shader successfully compiled?
compileStatus :: GLShader -> IO Bool
compileStatus s = 
  fmap ((==fromIntegral GL.gl_TRUE). head) $ allocaArray 1 $ \buf -> 
    GL.glGetShaderiv s GL.gl_COMPILE_STATUS buf >> peekArray 1 buf

-- | Returns the information log for a shader object.
-- The function 'shaderInfoLog' is a wrapper around 'glGetShaderInfoLog'.
shaderInfoLog :: GLShader -> IO String
shaderInfoLog sh = do
  maxLength <- fmap (fromIntegral . head) $ allocaArray 1 $ \buf -> 
    GL.glGetShaderiv sh GL.gl_INFO_LOG_LENGTH buf >> peekArray 1 buf
  allocaArray (fromIntegral maxLength) $ \infoLogPtr -> do
      GL.glGetShaderInfoLog sh maxLength nullPtr infoLogPtr
      peekCAString (castPtr infoLogPtr)

-- | returns the uniform location within a program object.
uniformLoc :: GLProgram -> String -> IO GL.GLint
uniformLoc p name = withCAString name (GL.glGetUniformLocation p . castPtr)

-- | Was the program successfully linked?
linkStatus :: GLProgram -> IO Bool
linkStatus p = 
  fmap ((==fromIntegral GL.gl_TRUE). head) $ allocaArray 1 $ \buf -> 
    GL.glGetProgramiv p GL.gl_LINK_STATUS buf >> peekArray 1 buf

-- | Returns the information log for a program object.
-- The function 'programInfoLog' is a wrapper around 'glGetProgramInfoLog'.
programInfoLog :: GLProgram -> IO String
programInfoLog program = do
  maxLength <- fmap (fromIntegral . head) $ allocaArray 1 $ \buf -> 
    GL.glGetProgramiv program GL.gl_INFO_LOG_LENGTH buf >> peekArray 1 buf
  allocaArray (fromIntegral maxLength) $ \infoLogPtr -> do
      GL.glGetProgramInfoLog program maxLength nullPtr infoLogPtr
      peekCAString (castPtr infoLogPtr)

getProgrami :: GLProgram -> GL.GLenum -> IO GL.GLint
getProgrami p e =
  fmap (fromIntegral . head) $ allocaArray 1 $ \buf -> 
    GL.glGetProgramiv p e buf >> peekArray 1 buf

getActiveUniformsi :: GLProgram -> GL.GLuint -> GL.GLenum -> IO GL.GLint
getActiveUniformsi p i e =
    fmap (fromIntegral . head) $ 
      withArray [i] $ \is ->
        allocaArray 1 $ \buf -> 
          GL.glGetActiveUniformsiv p 1 is e buf >> peekArray 1 buf

getActiveUniformsiv :: GLProgram -> [GL.GLuint] -> GL.GLenum -> IO [GL.GLint]
getActiveUniformsiv _ [] _ = return []
getActiveUniformsiv p i@(_:_) e =
  let len = length i
      count = fromIntegral len
  in  withArray i $ \is ->
        allocaArray len $ \buf -> 
          GL.glGetActiveUniformsiv p count is e buf >> peekArray len buf

getIntegerv :: GL.GLenum -> IO GL.GLint
getIntegerv e = fmap head $ allocaArray 1 $ \buf -> 
      GL.glGetIntegerv e buf >> peekArray 1 buf
