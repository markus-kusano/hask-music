{-#  LANGUAGE Arrows  #-}
import Euterpea.IO.Audio.BasicSigFuns (envExponSeg, osc, tableSinesN, Table, delayLineT, delayLine, noiseWhite, filterLowPassBW)
import Euterpea.IO.Audio.Render (Instr,)
import Euterpea.IO.Audio.IO (outFile,)
import Euterpea.IO.Audio.Basics (apToHz, outA)
import Euterpea.IO.Audio.Types (SigFun, Clock, Mono, AudRate)
import Euterpea.Music.Note.Music (absPitch, PitchClass (G))

import Filters (fir3)

-- Sine wave table with 4096 samples. Contains on the fundamental.
sinTab1 :: Table
sinTab1 = tableSinesN 4096 [1]

-- Little wrapper around an envelope generator. 
-- gitEnv01 du: produces an envelope for a `d` duration guitar note
--
-- The envelope has a quick rise to maximum volume and a slower decay
gitEnv01 :: (Clock p) => Double -> SigFun p () Double
gitEnv01 du = envExponSeg vols [riseDur, du - riseDur]
    where vols = [0, 1, 0.001] 
          riseDur = 0.003
          -- fallDur = if du > 2
          --             then 2
          --             else du

-- FM Synthesis gitbox. Uses gitEnv01
-- This one sounds kina like a bass
gitInst01 :: Instr (Mono AudRate)
gitInst01 dur ap vol [] = 
    proc () -> do
      aenv <- gitEnv01 d -< () 
      -- Modulation frequency defined as a ratio of carrier frequency
      let m = f * 0.25
      -- Strength of modulator
      let ma = (0.5 :: Double)
      mWave <- osc sinTab1 0 -< m
      a1 <- osc sinTab1 0 -< f + (mWave * f * ma)
      outA -< a1 * aenv * v
    where f = apToHz ap
          v = fromIntegral vol / 100
          d = fromRational dur

-- FM Synthesis gitbox. Uses gitEnv01
gitInst02 :: Instr (Mono AudRate)
gitInst02 dur ap vol [] = 
    proc () -> do
      aenv <- gitEnv01 d -< () 
      -- Modulation frequency defined as a ratio of carrier frequency
      let m = f * 0.5
      -- Strength of modulator
      let ma = (0.8 :: Double)
      mWave <- osc sinTab1 0 -< m
      a1 <- osc sinTab1 0 -< f + (mWave * f * ma)
      outA -< a1 * aenv * v
    where f = apToHz ap
          v = fromIntegral vol / 100
          d = fromRational dur

gitInst02Dble :: Instr (Mono AudRate)
gitInst02Dble dur ap vol [] = 
    proc () -> do
      aenv <- gitEnv01 d -< () 
      -- Modulation frequency defined as a ratio of carrier frequency
      let m = f * 0.5
      -- Strength of modulator
      let ma = (0.8 :: Double)
      mWave <- osc sinTab1 0 -< m
      a1 <- osc sinTab1 0 -< f + (mWave * f * ma)
      a2 <- osc sinTab1 0 -< (f + 300) + (mWave * f * ma)
      outA -< ((a1 * aenv * v) / 2) + ((0.75 * a2 * aenv * v) / 2)

    where f = apToHz ap
          v = fromIntegral vol / 100
          d = fromRational dur

-- Basic karplus strong without an input. The sound is affected by the
-- initial contents of the delay line.
--
-- The frequency is determined by the length of the buffer (this can be
-- thought of as the time it takes for the wave to travel up the string)
--
-- The energy decay factor used is 0.994
--
-- More details here:
-- http://introcs.cs.princeton.edu/java/assignments/guitar.html
--
-- Even more design detail heres about adding distortion and feedback: 
-- http://www.music.mcgill.ca/~gautam/gau618fp.html
--
-- TODO: Does the table need to be initialized with noise? How do you do
-- this?
karplusStrongBasic :: Instr (Mono AudRate)
karplusStrongBasic dur ap vol [] = 
    proc () -> do
      -- 666 is the whitenoise seed
      -- wn <- (noiseWhite 666) -< ()
      rec a1 <- (delayLineT delayLength sawtoothTable) -< out
          --out <- filterLowPassBW -< (a1 * 0.994, 2000)
          -- TODO: the first parameter of the FIR filter can control the
          -- frequency decay of higher harmonics. It should be controlled
          -- based on the input frequency to prevent higher frequencies
          -- from decaying too quickly
          --
          -- The FIR must be monotonically decreasing (apparently): so:
          --    p1 > 2p0 > 0
          --      where p0 is the first param, p1 the second
          out <-  fir3 0.2 0.5 0.3 -< a1 * 0.994
      outA -< a1
    where f :: Double
          f = apToHz ap
          v = fromIntegral vol / (100 :: Double)
          d :: Double
          d = fromRational dur
          -- The length of the buffer is the integer value of the sample
          -- rate divided by the frequency
          -- TODO: How can we get the current sampling frequency?
          delayLength :: Int
          delayLength = round $ (44100 :: Double) / f
          sawtoothTable = tableSinesN delayLength 
                            [1, 0.5, 0.3, 0.25, 0.2, 0.167, 0.14, 0.125, 0.111]

karplusTest :: IO () 
karplusTest = outFile "out.wav" 6 (karplusStrongBasic 6 (absPitch (G, 4)) 80 [] )

git01Test :: IO ()
git01Test = outFile "out.wav" 6 (gitInst01 6 (absPitch (G, 4)) 80 [] )

git02Test :: IO ()
--git02Test = outFile "out.wav" 6 (gitInst02 6 (absPitch (G, 4)) 80 [] )
git02Test = outFile "out.wav" 6 (gitInst02Dble 6 (absPitch (G, 4)) 80 [] )


main :: IO () 
-- main = git01Test
-- main = git02Test
main = karplusTest
