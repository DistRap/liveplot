{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Graphics.Liveplot.Types where

import Linear (V2, M33)
import Data.Vinyl
import Graphics.Rendering.OpenGL (GLfloat, Position(..), Size(..))
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW
import MVC (Pipe)
import Data.Set (Set)
import Control.Concurrent.STM
import qualified Pipes.Prelude as Pipes

import Graphics.Liveplot.Utils

-- OpenGL events
data Event =
    Timestep Double
  | Keys     (Set Key)
  | Buttons  (Set MouseButton)
  | MousePos (V2 Double)
  | WinSize  (V2 Int)
  | Quit
  deriving Show

-- A record each drawing function will receive.
type Viewport = (Position, Size)
type AppInfo = FieldRec '[ '("cam", M33 GLfloat) ]

data GLApp = GLApp AppInfo Viewport
  deriving Show

data SensorReading a = Reading String a
  deriving (Eq, Show)

instance Functor SensorReading where
  fmap f (Reading s v) = (Reading s (f v))

type PlotInit a = (TVar (Maybe [a]), (SensorReading a) -> IO (), Maybe [a] -> GraphInfo -> IO ())

class Plottable a where
  initplot :: GraphInfo -> IO (PlotInit a)

accepts :: String -> SensorReading t -> Bool
accepts n (Reading s' _) = n == s'

named :: Monad m => String -> Pipe a (SensorReading a) m r
named n = Pipes.map (\x -> Reading n x)

-- buffer value and values in TVar
bufferTVar :: Fractional a =>
             String -> Int -> TVar (Maybe [a]) -> SensorReading a -> IO ()
bufferTVar name buflen tvar sample@(Reading _ val) = do
  case accepts name sample of
    True -> atomically $ do
        mtvar <- readTVar tvar
        case mtvar of
          Just cval -> writeTVar tvar $ Just $ rpad buflen 0.0 $ take buflen $ val:cval
          Nothing -> writeTVar tvar $ Just $ replicate buflen 0.0
    _ -> return ()

data GraphColor = Red | Green
  deriving (Show, Eq, Ord)

data GraphInfo = GraphInfo {
      graph_name :: String
    , graph_appinfo :: AppInfo
    , graph_viewport :: Viewport
    , graph_samples :: Int
    , graph_resolution :: Int
    , graph_color :: GraphColor
    , graph_points :: Int
    , graph_scale :: (Float, Float)
    , graph_offset :: (Int, Int)
    }
  deriving (Show, Eq, Ord)

defaultCam :: Camera GLfloat
defaultCam = camera2D

defaultViewport :: Viewport
defaultViewport = (Position 0 0, Size 1920 1080)

defaultAppInfo :: AppInfo
defaultAppInfo = SField =: camMatrix defaultCam

defGI :: GraphInfo
defGI = GraphInfo {
      graph_name = "unnamed"
    , graph_appinfo = defaultAppInfo
    , graph_viewport = defaultViewport
    , graph_samples = 100
    , graph_resolution = 100
    , graph_color = Red
    , graph_points = 100
    , graph_scale = (1, 1)
    , graph_offset = (0, 0)
   }
