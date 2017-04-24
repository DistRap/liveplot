module Graphics.Liveplot.Shaders where

import Data.Vec ((:.)(..), Vec3, Vec4)

import qualified Data.Vector.Storable as V

import qualified Data.ByteString.Char8 as BS

import Andromeda.Simple.Expr
import Andromeda.Simple.Type
import Andromeda.Simple.GLSL
import Andromeda.Simple.StdLib
import Andromeda.Simple.Render.Compile

compileBS :: Statement () -> Statement () -> (BS.ByteString, BS.ByteString)
compileBS vert frag =
  let vStr = version ++ toGLSL vert
      fStr = version ++ toGLSL frag
  in (BS.pack vStr, BS.pack fStr)
  where
    version :: String
    version = "#version 330 core\n"

test :: IO ()
test =
    let (vs, fs) = compileBS lineVertShader lineFragShader
    in do
      BS.putStrLn vs
      BS.putStrLn fs

lineShaders :: (BS.ByteString, BS.ByteString)
lineShaders = compileBS lineVertShader lineFragShader

positionColor :: (Expr (Vec4 Float), Expr (Vec4 Float))
positionColor =
  let xCoord = fetch "xCoord" (ScalarT SFloat)
      cam = uniform "cam" (Matrix3T SFloat)
      tex = uniform "tex" Sampler2DT
      yCoord = (((texture tex
                  (flt ((xCoord + 1)/2) +: flt 0.0)) ~> "r") - (flt 0.5)) * (flt 2.0)

      col = yCoord / 2.0 + 0.5

  in (((cam #* (flt xCoord +: flt yCoord +: flt 1.0)) +: flt 1.0)
     ,(flt (yCoord/2 + 0.5) +: flt (1 - col) +: flt 0.0 +: flt 1.0))

lineVertShader :: Statement ()
lineVertShader = do
    "gl_Position" =: fst positionColor
    out "fColor" $ snd positionColor

outColor :: Expr (Vec4 Float)
outColor =
    let f_color = fetch "fColor" (Vec4T SFloat)
    in f_color

lineFragShader :: Statement ()
lineFragShader = out "fragColor" outColor
