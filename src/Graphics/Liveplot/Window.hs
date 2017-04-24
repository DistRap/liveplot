{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Liveplot.Window where

import Prelude hiding (init)
import Control.Monad
import Control.Lens ((^.), contains, _Left, _Right)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Time.Clock
import Graphics.UI.GLFW
import Linear
-- moi
import MVC
import qualified MVC.Prelude as MVC
import Graphics.Rendering.OpenGL
import Graphics.GLUtil.Camera2D
import Data.Vinyl

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

import Pipes.Extras ((+++))

import Graphics.Liveplot.Types
import Graphics.Liveplot.Utils
--- XXX: this has something to do with orphan instance in G.L.Line
import Graphics.Liveplot.Line

initGraph :: String -> (Float, Float) -> (Int, Int)-> GraphInfo
initGraph name scale' offset = defGI {
      graph_name = name
    , graph_offset = offset
    , graph_scale = scale'
    }

lineGraph :: String -> (Float, Float) -> (Int, Int) -> (GraphInfo, GLfloat)
lineGraph name scale' offset = (defGI
  { graph_name = name
  , graph_offset = offset
  , graph_scale = scale'
  }, 0)

-- add scroll input callback
-- http://www.glfw.org/docs/latest/input_guide.html#scrolling
ogl :: (Plottable a) => [(GraphInfo, a)]
       -> Managed (View (Either (SensorReading a) GLApp), Controller Event)
ogl parts = join $ managed $ \k -> do
  let simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
  let width = 600
      height = 300
      windowTitle = "lala"
  setErrorCallback $ Just simpleErrorCallback
  r <- init
  when (not r) (error "Error initializing GLFW!")

  windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
  windowHint $ WindowHint'OpenGLForwardCompat True
  windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
  windowHint $ WindowHint'ContextVersionMajor 3
  windowHint $ WindowHint'ContextVersionMinor 3

  m@(~(Just w)) <- createWindow width height windowTitle Nothing Nothing
  when (isNothing m) (error "Couldn't create window!")

  makeContextCurrent m

  kbState <- newIORef S.empty
  mbState <- newIORef S.empty
  mpState <- getCursorPos w >>= newIORef . uncurry V2
  wsState <- getWindowSize w >>= newIORef . uncurry V2
  lastTick <- getCurrentTime >>= newIORef
  setKeyCallback w (Just $ keyCallback kbState)
  setMouseButtonCallback w (Just $ mbCallback mbState)
  setCursorPosCallback w (Just $ mpCallback mpState)
  setWindowSizeCallback w $ Just $ \win x y -> do
    wsCallback wsState win x y
    viewport $= (Position 0 0, Size (fromIntegral x) (fromIntegral y))

  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

  redraw <- atomically $ newTVar $ Just True

  inits <- flip mapM parts $ \(gi, (_init :: a)) -> initplot gi :: IO (PlotInit a)
  graphInfoTVars <- flip mapM parts $ \(gi, (_init :: a)) -> atomically $ newTVar gi -- :: IO (PlotInit a)
  --graphInfoTVars <- mapM (pure $ atomically $ newTVar defGI) parts
  let (dataTVars, storefns, drawfns) = unzip3 inits
  let tvars = zip dataTVars graphInfoTVars

  let setRedraw =  atomically $ writeTVar redraw $ Just True
  let resetRedraw =  atomically $ writeTVar redraw $ Nothing

  let tick = do pollEvents
                t <- getCurrentTime
                dt <- realToFrac . diffUTCTime t <$> readIORef lastTick
                writeIORef lastTick t

                keys <- readIORef kbState
                buttons <- readIORef mbState
                pos <- readIORef mpState
                wsize <- readIORef wsState

                mredraw <- readTVarIO redraw
                case mredraw of
                  Nothing -> threadDelay 100 >> return ()
                  Just _ -> do
                    clear [ColorBuffer, DepthBuffer]
                    zipWithM_ (\draw (tvar, gtvar) -> do
                      mdat <- readTVarIO tvar
                      gi <- readTVarIO gtvar
                      draw mdat gi
                      ) drawfns tvars
                    swapBuffers w
                    threadDelay 10
                    resetRedraw


                -- XXX: emit only changing values except for timestep?
                return $ [
                    Timestep dt
                  , Keys keys
                  , Buttons buttons
                  , MousePos pos
                  , WinSize wsize
                  ]

  let steptick :: Producer Event IO ()
      steptick = forever $ lift tick >>= mapM_ yield

  let handleAppInfo :: View (GLApp)
      handleAppInfo = asSink $ \(GLApp ai vp) -> do
        let updategi = \x -> x {
                graph_appinfo = ai
              , graph_viewport = vp
              }
        mapM_ (\v -> atomically $ modifyTVar v updategi) graphInfoTVars
        setRedraw

  --let handleData :: (Plottable a) => View (SensorReading a)
  let
      handleData = asSink $ \dat -> do
        mapM_ ($ dat) storefns
        setRedraw

  k $ do
    -- Event producer and drawing function
    evts <- MVC.producer (bounded 1) (steptick)
    -- Data and AppInfo handlers
    let hdat = handles _Left handleData
        hapi = handles _Right handleAppInfo

    return (hapi <> hdat, evts)

-- transform AppInfo and camera according to events from OpenGL
campipe :: (Monad m)
        => AppInfo
        -> Camera GLfloat
        -> Viewport
        -> MVC.Proxy () (Event) () (GLApp) m ()
campipe initai initcam initviewport = go initai initcam initviewport
  where
    fwd ai c vp = yield (GLApp ai vp) >> go ai c vp
    go ai c vp = do
      evt <- await
      case evt of
        Keys k | k ^. contains Key'Escape -> return ()
               | k ^. contains Key'Q -> return ()
               | otherwise -> do
                   let newCam = moveCam k c
                   let newAi = SField =: (camMatrix newCam)
                   fwd newAi newCam vp

        WinSize (V2 sx sy) -> fwd ai c (fst vp, Size (fromIntegral sx) (fromIntegral sy))
        _ -> go ai c vp

defaultCamPipe :: Monad m => MVC.Proxy () (Event) () (GLApp) m ()
defaultCamPipe = campipe defaultAppInfo defaultCam defaultViewport

defaultPipe :: forall m a . (Monad m, Plottable a)
    => Pipe (Either (SensorReading a) Event)
            (Either (SensorReading a) GLApp) m ()
defaultPipe = cat +++ defaultCamPipe

keyCallback :: IORef (Set Key) -> KeyCallback
keyCallback keys _w k _ KeyState'Pressed _mods = modifyIORef' keys (S.insert k)
keyCallback keys _w k _ KeyState'Released _mods = modifyIORef' keys (S.delete k)
keyCallback _ _ _ _ _ _ = return ()

mbCallback :: IORef (Set MouseButton) -> MouseButtonCallback
mbCallback mbs _w b MouseButtonState'Pressed _ = modifyIORef' mbs (S.insert b)
mbCallback mbs _w b MouseButtonState'Released _ = modifyIORef' mbs (S.delete b)

mpCallback :: IORef (V2 Double) -> CursorPosCallback
mpCallback mp _w x y = writeIORef mp (V2 x y)

wsCallback :: IORef (V2 Int) -> WindowSizeCallback
wsCallback ws _w w h = writeIORef ws (V2 w h)
