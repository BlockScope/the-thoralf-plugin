{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Text.Printf (printf)
import UoM (Unit (..), calcDistance, extract)

main :: IO ()
main = do
  putStrLn "UoM examples"
  putStrLn $ printf "3 m/s for 2 s = %s m" (show $ calcDistance (MkUnit 3) (MkUnit 2))
  putStrLn ""
  $(defaultMainGenerator)

prop_distance :: Double -> Double -> Bool
prop_distance x y = x * y == (extract $ calcDistance (MkUnit x) (MkUnit y))
