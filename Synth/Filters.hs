{-#  LANGUAGE Arrows  #-}
module Filters (fir3) where

--import Euterpea.IO.Audio.BasicSigFuns (SigFun)
import Prelude hiding (init)
import Euterpea.IO.Audio.Types (SigFun, Signal) --, Clock, Mono, AudRate)
import Euterpea.IO.Audio.Basics (outA)
--import Euterpea.IO.Audio.BasicSigFuns
import Control.CCA.Types (init)

-- Third order FIR (finite impulse response) filter
-- fir3 b0 b1 b2: where b0 is multiplied against on the value of the input
-- on nth time, b1 is on (n-1), and (b2) is on (n-2) (i.e., b1 is b0
-- delayed by one)
fir3 :: Double -> Double -> Double -> Signal p Double Double
fir3 b0 b1 b2 = 
    proc (sig) -> do 
      y1 <- init 0 -< sig
      y2 <- init 0 -< y1
      outA -< sig * b0
              + y1 * b1
              + y2 * b2
      
