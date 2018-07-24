{-# LANGUAGE DataKinds, TypeOperators #-}

import Pipes
import qualified Pipes.Prelude as P
import Control.Concurrent (threadDelay)

import Graphics.Liveplot

main :: IO ()
main = runLiveplot std

std :: Managed (View (Either (SensorReading GLfloat) GLApp),
                Controller (Either (SensorReading GLfloat) Event))
std = do
  let inits = [ lineGraphN 2000 "adc" (1, 1) (0, 0) ]

  (v, c) <- ogl inits

  dat <- producer (bounded 1) (stdinData >-> normalize (20, 230) >-> named "adc")
  return (v, fmap Left (dat) <> fmap Right c)

stdinData :: Producer GLfloat IO ()
stdinData = for P.stdinLn cvt
  where cvt :: Monad m => String -> Producer Float m ()
        cvt x = do
          yield (read x)
