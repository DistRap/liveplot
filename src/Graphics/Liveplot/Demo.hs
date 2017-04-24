module Graphics.Liveplot.Demo where

import MVC
import qualified MVC.Prelude as MVC
import qualified Pipes.Prelude as Pipes
import Control.Concurrent (threadDelay)

import Graphics.Liveplot

runDemo :: IO()
runDemo = runLiveplot demo

demo :: Managed (View (Either (SensorReading GLfloat) GLApp),
                 Controller (Either (SensorReading GLfloat) Event))
demo = do
  let inits = [ lineGraph "adc" (2, 1) (0, 0)
              , lineGraph "dac" (2, 1) (1, 0)]

  (v, c) <- ogl inits

  dat <- MVC.producer (bounded 1) (sinedata 5 10 >-> named "adc")
  dat2 <- MVC.producer (bounded 1) (sinedata 100 10 >-> named "dac")
  return (v, fmap Left (dat <> dat2) <> fmap Right c)

sinedata :: Float -> Float -> Producer GLfloat IO ()
sinedata hz divider =
  hztick hz
  >-> Pipes.map ((/2). (+1) . (sin) . (/divider))

moiredata :: Float -> Producer GLfloat IO ()
moiredata hz =
  hztick hz
  >-> Pipes.map ((/2). (+1) . (sin) . (/2))

hztick :: (Num t, RealFrac t1) => t1 -> MVC.Proxy x' x () t IO b
hztick hz = run 0
  where
    run n = yield n >> (lift $ threadDelay $ floor $ 1000000/hz) >> run (n+1)
