{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graphics.Liveplot.Line where
import Data.Vinyl
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Linear (V1(..))
import Control.Concurrent.STM

import Graphics.Liveplot.Shaders
import Graphics.Liveplot.Types

type XPos = '("xCoord", V1 GLfloat)

xcoord :: SField XPos
xcoord = SField

monoTex :: Int -> IO TextureObject
monoTex len = do
  t <- freshTextureFloat len 1 TexMono
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  texture2DWrap $= (Repeated, ClampToEdge)
  return t

line :: GraphInfo -> IO (Maybe [GLfloat] -> GraphInfo -> IO ())
line gi =
  do s <- uncurry simpleShaderProgramBS lineShaders

     let nsamples = graph_samples gi
         xResolution = graph_resolution gi
         pResolution = graph_points gi

         isamples = fromIntegral nsamples
         xCoords :: [GLfloat]
         xCoords = take xResolution $ iterate (+ 2 / fromIntegral xResolution) (-1)
         _pointCoords :: [GLfloat]
         _pointCoords = take pResolution $ iterate (+ 2 / fromIntegral pResolution) (-1)
         yCoords :: [GLfloat]
         yCoords = replicate nsamples 0

     vb <- bufferVertices . map (xcoord =:) $ V1 <$> xCoords -- <*> [0.0]-- [-1.0,1.0] <*> [-1.0,1.0]
     --_vp <- bufferVertices . map (xcoord =:) $ V1 <$> pointCoords
     t <- monoTex nsamples
     reloadTexture t (TexInfo isamples 1 TexMono yCoords)
     -- need to set current program here or setUniforms fails
     currentProgram $= Just (program s)
     setUniforms s (texSampler =: 0)

     --let vp = withViewport (Position 10 10) (Size 1024 60)

     -- no idea why this can't use vp
     pointsVAO <- makeVAO $ do enableVertices' s vb
                               bindVertices vb

     linesVAO  <- makeVAO $ do enableVertices' s vb
                               bindVertices vb
     return $ \d GraphInfo{..} -> do
       currentProgram $= Just (program s)
       setUniforms s graph_appinfo
       let withVP = withVP' graph_viewport graph_scale graph_offset
       case d of
          Just dat -> reloadTexture t (TexInfo isamples 1 TexMono dat)
          Nothing -> return ()
       withVP $ withVAO linesVAO . withTextures2D [t] $
         drawArrays LineStrip 0 (fromIntegral xResolution)

       -- XXX: use point texture
       withVP $ withVAO pointsVAO . withTextures2D [t] $
         drawArrays Points 0 (fromIntegral pResolution)

  where
    texSampler = SField :: SField '("tex", GLint)
    withVP' (Position x y, Size w h) (xsc, ysc) (xoff, yoff) = withViewport
      (Position (x + (fromIntegral yoff) * w')
                (y + (fromIntegral xoff) * h'))
      (Size w' h')
      where
        w' = floor $ fromIntegral w / ysc
        h' = floor $ fromIntegral h / xsc

instance Plottable GLfloat where
  initplot gi = do
    tvar <- atomically $ newTVar Nothing
    draw <- line gi
    return (tvar, bufferTVar (graph_name gi) (graph_samples gi) tvar, draw)
