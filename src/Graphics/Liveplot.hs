module Graphics.Liveplot (
    runLiveplot
  , named
  , initGraph
  , lineGraph
  , SensorReading(..)
  , GLApp
  , Event
  , GLfloat
  , rpad
  , ogl) where

import MVC
import Graphics.Liveplot.Window
import Graphics.Liveplot.Types
import Graphics.Liveplot.Utils
import Graphics.Rendering.OpenGL (GLfloat)

runLiveplot :: Plottable a => Managed (View (Either (SensorReading a) GLApp), Controller (Either (SensorReading a) Event))  -> IO ()
runLiveplot app = runMVC () (asPipe defaultPipe) app

