{-# LANGUAGE OverloadedStrings #-}

import Sound.Tidal.Context

-- SH-101:
--   3: VCF FREQ
--   5: PORTAMENTO TIME
--   9: VCF RESONANCE
--  12: EFX CRUSHER
--  13: EFX DELAY TIME
--  16: MIXER SQR
--  17: MIXER SAW
--  18: MIXER SUB OSC
--  19: MIXER NOISE
--  26: MODULATOR VCO DEPTH
--  28: MODULATOR VCF DEPTH
--  29: MODULATOR RATE
--  35: MODULATOR WAVEFORM
--  41: BEND RANGE
--  47: VCO RANGE
--  50: VCO PULSE WIDTH
--  60: VCO PULSE WIDTH MOD
--  69: VCA TONE
--  81: VCF ENV
--  82: VCF KEY FOLLOW
--  83: VCF ATTACK
--  84: VCF DECAY
--  85: VCF SUSTAIN
--  86: VCF RELEASE
--  89: VCA ATTACK
--  90: VCA DECAY
--  91: EFX REVERB LEVEL
--  94: EFX DELAY LEVEL
--  96: VCA SUSTAIN
--  97: VCA RELEASE
-- 110: PATCH LEVEL
-- 113: SUB OSC TYPE
-- 116: PORTAMENTO MODE
-- 117: ENV TRIG
-- 118: TEMPO SYNC
-- 119: VCA MODE