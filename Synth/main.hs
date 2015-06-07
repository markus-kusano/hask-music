{-#  LANGUAGE Arrows  #-}
import Euterpea.IO.Audio.BasicSigFuns (envExponSeg, osc, tableSinesN, Table)
import Euterpea.IO.Audio.Render (Instr,)
import Euterpea.IO.Audio.IO (outFile,)
import Euterpea.IO.Audio.Basics (apToHz, outA)
import Euterpea.IO.Audio.Types (SigFun, Clock, Mono, AudRate)
import Euterpea.Music.Note.Music (absPitch, PitchClass (G))

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

git01Test :: IO ()
git01Test = outFile "out.wav" 6 (gitInst01 6 (absPitch (G, 4)) 80 [] )

git02Test :: IO ()
--git02Test = outFile "out.wav" 6 (gitInst02 6 (absPitch (G, 4)) 80 [] )
git02Test = outFile "out.wav" 6 (gitInst02Dble 6 (absPitch (G, 4)) 80 [] )

main :: IO () 
-- main = git01Test
main = git02Test
