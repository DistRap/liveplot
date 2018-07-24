module Graphics.Liveplot (
    runLiveplot
  , named
  , SensorReading(..)
  , GLApp
  , Event
  , GLfloat
  , ogl
  , module Graphics.Liveplot.Utils
  , module Graphics.Liveplot.Window
  , module MVC
  , module MVC.Prelude
  ) where

import MVC
import MVC.Prelude
import Graphics.Liveplot.Window
import Graphics.Liveplot.Types
import Graphics.Liveplot.Utils
import Graphics.Rendering.OpenGL (GLfloat)

runLiveplot :: Plottable a => Managed (View (Either (SensorReading a) GLApp), Controller (Either (SensorReading a) Event))  -> IO ()
runLiveplot app = runMVC () (asPipe defaultPipe) app

