import Euterpea.Music.Note.Music
import Euterpea.Music.Note.MoreMusic
import Euterpea.IO.MIDI.ToMidi (writeMidi)

-- Default volume (i.e., un accented)
defVol :: Int 
defVol = 64

-- Little helper function. Pass it a Dur and it creates a bass drum hit.
-- Only works in the Percussion instrument 
bassDrum :: Dur -> Music Pitch
bassDrum = perc BassDrum1

-- Same as bassDrum but includes a volume
bassDrumVol :: Dur -> Volume -> Music (Pitch, Volume)
bassDrumVol du v = addVolume v $ bassDrum du

-- Bass drum at default volume
bassDrumDefVol :: Dur -> Music (Pitch, Volume)
bassDrumDefVol = addDefVol . bassDrum

-- Convert a Music Pitch into Music (Pitch, Volume) with the default volume
addDefVol :: Music Pitch -> Music (Pitch, Volume)
addDefVol = addVolume defVol

-- Same as bassDrum but for a snare 
snareDrum :: Dur -> Music Pitch
snareDrum = perc AcousticSnare

-- Same as snareDrum but includes a volume
snareDrumVol :: Dur -> Volume -> Music (Pitch, Volume)
snareDrumVol du v = addVolume v $ snareDrum du

-- Snare drum at default volume
snareDrumDefVol :: Dur -> Music (Pitch, Volume)
snareDrumDefVol = addDefVol . snareDrum 

-- Same as snaredrum/bassdrum functions but with HH
cHH :: Dur -> Music Pitch
cHH = perc ClosedHiHat

-- Same as bassDrum but includes a volume
cHHVol :: Dur -> Volume -> Music (Pitch, Volume)
cHHVol du v = addVolume v $ cHH du

-- Bass drum at default volume
cHHDefVol :: Dur -> Music (Pitch, Volume)
cHHDefVol = addDefVol . cHH

-- Same as snaredrum/bassdrum functions but with HH
oHH :: Dur -> Music Pitch
oHH = perc OpenHiHat

-- Same as bassDrum but includes a volume
oHHVol :: Dur -> Volume -> Music (Pitch, Volume)
oHHVol du v = addVolume v $ oHH du

-- Bass drum at default volume
oHHDefVol :: Dur -> Music (Pitch, Volume)
oHHDefVol = addDefVol . oHH

-- Same as snaredrum/bassdrum functions but for crash
crash :: Dur -> Music Pitch
crash = perc CrashCymbal1

-- Same as bassDrum but includes a volume
crashVol :: Dur -> Volume -> Music (Pitch, Volume)
crashVol du v = addVolume v $ crash du

-- Bass drum at default volume
crashDefVol :: Dur -> Music (Pitch, Volume)
crashDefVol = addDefVol . crash

-- Helper function
-- Scales the volume by the passed factor
scaleVolume :: Rational 
               -> Music (Pitch, Volume)
               -> Music (Pitch, Volume)
scaleVolume s m = mMap scV m
    -- Use the scaling factor on the internal (Pitch, Volume)
    where scV :: (Pitch, Volume) -> (Pitch, Volume)
          scV (p, v) = (p, round (s * fromIntegral v))
          --newVol = round (s * fromIntegral v)


-- This beat is used often. Half a bar, bass drum on beat one and on off
-- beats
-- The off beats are much quieter than the down beat
bdOffBeatHalfBar :: Music (Pitch, Volume)
bdOffBeatHalfBar = bassDrumDefVol sn :+: rest en :+: quietBD sn
                 :+: rest sn :+: quietBD sn
          where quietBD l = scaleVolume (2/5) $ bassDrumDefVol l

-- In general, the cymbals are the most complicated part. On the fourth
-- bar, there is a hi-hat diddly extending into the fifth bar
--
-- This is the intro which starts with a crash (normally, it starts with an
-- open HH)
--
-- It is 4 bars.
cymIntro :: Music (Pitch, Volume)
cymIntro = cymCr1
            :+: cymNoOp
            :+: cymNoOp
            :+: cymOp5        -- Bar 4

-- This is the "middle" sections of the cymbals. It starts with an open HH
-- and ends with an open HH. 
--
-- 4 bars
cymIntroMiddle :: Music (Pitch, Volume)
cymIntroMiddle = cymOp1
            :+: cymNoOp
            :+: cymNoOp
            :+: cymOp5        -- Bar 4

-- There are 16 bars until the vocals come in
cymPreVocal :: Music (Pitch, Volume)
cymPreVocal = cymIntro :+: (timesM 3 cymIntroMiddle)

-- 16 bars of snare and bass before vocals
snbPreVocal :: Music (Pitch, Volume)
snbPreVocal = timesM 4 fourBar
        -- A four bar snare and bass phrase
  where fourBar = (timesM 3 snbIntroMiddle) :+: (snbIntroEnd)

-- 16 more bars for the first verse. 
cymFirstV :: Music (Pitch, Volume)
-- cymIntroMiddle is four bars
cymFirstV = timesM 3 cymIntroMiddle :+: outro
  -- the last bar has an open HH on beat four 
  where outro = instrument Percussion $ 
                cymOp1
                :+: cymNoOp
                :+: cymNoOp
                :+: (timesM 3 (cHHDefVol en))
                :+: (oHHDefVol en)
                :+: rest qn



-- 16 more bars for the first verse
--
--
-- The last eight bars are a little special in that they switch back to
-- having a single snare hit on beat 4 only (snbIntroMiddle)
snbFirstV :: Music (Pitch, Volume)
snbFirstV = 
            -- Eight bars of snare on off beat
            timesM 2 firstFourBar
            -- 2 bars with snare on off beat, two bars without
            -- (four bars total)
            :+: (timesM 2 snbFVMiddle) :+: snbIntroMiddle :+: snbFVEnd
            -- 1 bar double snare, 2 bars single snare, one bar with
            -- a little snare diddly at the end to take us to the next
            -- verse (four bars total)
            :+: snbFVMiddle :+: (timesM 2 snbIntroMiddle) :+: outro
    where firstFourBar = (timesM 3 snbFVMiddle) :+: snbFVEnd
          -- On the 16th bar, beat 5 is a snare with a snare on off-beat. 
          -- There is an open HH on beat 4.
          outro = instrument Percussion $ 
                  bdOffBeatHalfBar 
                  :+: (rest en) 
                  :+: (snareDrumDefVol sn) :+: (snareDrumDefVol sn)
                  :+: (rest en)

-- Cymbal line for the main drum beat with no open hihats or crashes.
-- One bar
cymNoOp :: Music (Pitch, Volume)
cymNoOp = instrument Percussion $ timesM 6 (cHHDefVol en)

-- Cymbal line for the main drum beat. 
-- Open hi-hat on beat one.
cymOp1 :: Music (Pitch, Volume)
cymOp1 = instrument Percussion $ 
                 (oHHDefVol en) 
                 :+: (timesM 5 (cHHDefVol en))

-- Cymbal line for main drum beat.
-- Crash on beat one (this is the intro bar)
cymCr1 :: Music (Pitch, Volume)
cymCr1 = instrument Percussion $ 
                 (crashDefVol en) 
                 :+: (timesM 5 (cHHDefVol en))

-- Cymbal line for main drum beat
-- Open HH on beat 5. This is connected to mainDrumCymOp1
cymOp5 :: Music (Pitch, Volume)
cymOp5 = instrument Percussion $ 
                 timesM 4 (cHHDefVol en) 
                 :+: oHHDefVol en
                 :+: cHHDefVol en

-- Snare and bass for main drum beat intro (first 16 bars)
-- The bass hits on the down beat of 1 and on the off beats for the first
-- three beats
--
-- The snare hits on beat 4 with nothing going on on the off beats
--
-- One bar
snbIntroMiddle :: Music (Pitch, Volume)
snbIntroMiddle = instrument Percussion $ 
                 -- bassDrum sn :+: rest en :+: bassDrum sn
                 -- :+: rest sn :+: bassDrum sn
                 bdOffBeatHalfBar
                 :+: snareDrumDefVol en :+: rest qn

-- On the fourth and fifth bar, the bass follows the open HH (beats five
-- and one)
snbIntroEnd :: Music (Pitch, Volume)
snbIntroEnd = instrument Percussion $ 
                 -- bassDrum sn :+: rest en :+: bassDrum sn
                 -- :+: rest sn :+: bassDrum sn
                 bdOffBeatHalfBar
                 :+: snareDrumDefVol en :+: rest en :+: bassDrumDefVol en


-- Snare and bass for first verse (the 2nd 16 bars (after lyrics come in)).
-- The snare drum almost copies the bass except it hits only on the second
-- off beat (not the third)
-- 
-- This is one bar
snbFVMiddle :: Music (Pitch, Volume)
snbFVMiddle = instrument Percussion $ 
         -- Same first 3 beats as the intro (snb{Middle,End}Intro)
         -- bassDrum sn :+: rest en :+: bassDrum sn
         -- :+: rest sn :+: bassDrum sn
         bdOffBeatHalfBar
         -- Snare on the downbeat, second off beat
         :+: snareDrumDefVol sn :+: rest en :+: snareDrumDefVol sn
         :+: rest en 

-- The fourth bar of the first verse snare and bass.
-- (This is the bar with the open HH)
-- It is the same as in the intro: is one snare hit on the downbeat
snbFVEnd :: Music (Pitch, Volume)
snbFVEnd = snbIntroEnd

-- The last bar of the first verse ends with two snare hits
--snbFVFinal 

-- Here, I stich together all the sequences
song :: Music (Pitch, Volume)
song = 
      -- 16 bar intro before vocals
      cymPreVocal :=: snbPreVocal
      -- 16 bar first verse
      :+: cymFirstV :=: snbFirstV

-- Default tempo is 120. We would like ~75, so, 
-- 120 * x = 75 ==> (5/8). We are also in 6/8 so convert everything to
-- triplets (3/2)
main :: IO ()
main = writeMidi "out.midi" $ tempo (5/8) $ tempo (3/2) song
--main = play mainDrum01
