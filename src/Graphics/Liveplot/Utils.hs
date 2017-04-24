module Graphics.Liveplot.Utils(
    dump
  , dbg
  , moveCam
  , cnf
  , cnfEndo
  , rpad
  ) where

import Prelude
import Control.Concurrent.STM
import Data.Monoid (All(..), Any(..))
import Data.Foldable (Foldable, foldMap,foldl',fold)
import Data.Set (Set)
import qualified Data.Set as S

import Linear
import MVC
import Graphics.GLUtil.Camera2D
import Graphics.GLUtil.Camera3D hiding (roll)
import Graphics.UI.GLFW

dump :: View a
dump = asSink (\_ -> return())

dbg :: (Show a) => View a
dbg = asSink (\e -> (putStrLn $ show e) >> return())

-- | Evaluate a boolean formula in conjunctive normal form (CNF) by
-- applying the predicate to each atom according to the logic of its
-- nesting in the formula.
cnf :: (Foldable s, Foldable t) => s (t Bool) -> Bool
cnf = getAll . foldMap (All . getAny . foldMap Any)

-- | Perform a left fold over a set of guarded update functions,
-- evaluating the guards left-to-right. For each guard that passes,
-- its associated update function is composed into a final composite
-- update function.
cnfEndo :: (k -> s -> Bool) -> (k -> s -> s) -> [([[k]], a -> a)] -> s -> a -> a
cnfEndo p del = go
  where go [] _ = id
        go ((k,f):fs) s | cnf (fmap (fmap (`p` s)) k) = go fs (delAll k s) . f
                        | otherwise = go fs s
        delAll k s = foldl' (flip del) s (fold k)

-- | Translate and rotate a 'Camera' based on 'UI' input.
moveCam :: (Conjugate a, Epsilon a, RealFloat a) => Set Key -> Camera a -> Camera a
moveCam keys = cnfEndo S.member S.delete 
                  [ ([shift, [Key'Left]], roll na)
                  , ([shift, [Key'Right]], roll pa)
                  , ([[Key'Left]], track (V2 np 0))
                  , ([[Key'Right]], track (V2 pp 0))
                  , ([[Key'Up]], track (V2 0 pp))
                  , ([[Key'Down]], track (V2 0 np))
                  --- XXX: tilting instead of zooming, how to zoom in 2d with cam?
                  -- maybe just switch to 3d cam
                  , ([[Key'PageUp]], tilt (pa))
                  , ([[Key'PageDown]], tilt (na))
                  ]
                  keys
  where shift = [Key'LeftShift, Key'RightShift]
        -- XXX: pass timeScale as well? (Normalize speeds to 60Hz update)
        timeScale = 1 -- realToFrac $ timeStep ui * 60
        pp = 0.08 * timeScale -- 1D speed
        np = negate pp
        pa = 2 * timeScale    -- angular step
        na = negate pa


rpad :: Int -> a -> [a] -> [a]
rpad n x xs = xs ++ (take (n-(length xs)) $ repeat x)
