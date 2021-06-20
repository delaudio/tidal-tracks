{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt, isDigit, ord)

import Sound.Tidal.Context

-- | Step sequencing
fdgStep :: String -> String -> Pattern String
fdgStep s cs = fastcat $ map f cs
  where
    f c
      | c == 'x' = pure s
      | c == 'x' = silence
      | isDigit c = pure $ s ++ ":" ++ [c]
      | otherwise = silence

fdgSteps :: [(String, String)] -> Pattern String
fdgSteps = stack . map (uncurry fdgStep)