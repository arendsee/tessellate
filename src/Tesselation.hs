{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation (myFigs) where

import Diagrams.Prelude
import qualified Diagrams.Backend.SVG.CmdLine as S

type Point' = (Double, Double)

w :: Double
w = 1

-- minimal distance from origin to a face
faceDistance :: Int -> Double
faceDistance i = 1 / (2 * tan (pi / fromIntegral i)) 

-- the angle between the ith face of an n-gon and origin
faceAngle :: Int -> Int -> Double
faceAngle n i = - pi/2 - (fromIntegral i / fromIntegral n) * 2 * pi

cornerDistance :: Int -> Double
cornerDistance i = 1 / (2 * sin (pi / fromIntegral i)) 

cornerAngle :: Int -> Int -> Double
cornerAngle n i = faceAngle n i - pi / fromIntegral n

-- the inside angle of a regular n-gon
innerAngle :: Int -> Double
innerAngle i = pi * (1 - 2 / fromIntegral i)

cornerPoly :: Int -> Diagram S.B
cornerPoly n =
  let a = cornerAngle n 0 + pi
      d = cornerDistance n
  in translate (r2 (d * cos a, d * sin a)) $ regPoly n w

regularOffset :: Int -> Int -> Int -> Diagram S.B -> Diagram S.B
regularOffset i n m x =
  let a = faceAngle n i
      d = faceDistance n + faceDistance m
      rot = -1 * (pi/2 - a) @@ rad
  in
    translate (r2 (d * cos a, d * sin a)) (rotate rot x)

regCycle :: [Int] -> Diagram S.B
regCycle = f 0  where
  f :: Double -> [Int] -> Diagram S.B
  f _ [] = mempty
  f r (i:rs) = rotate (r @@ rad) (cornerPoly i # lw veryThin) <> f (r + innerAngle i) rs

myFigs :: Diagram S.B
myFigs = foldl (beside (r2 (0,-1))) mempty
 [  circle w
 -- , circle 1 # fc green <> pentagon 5 # fc red
 -- , circle 1 # fc green ||| pentagon 5 # fc red
 , regPoly 6 w
 , atPoints (trailVertices $ regPoly 6 w) (repeat (circle 0.2 # fc blue)) <> regPoly 6 w
 -- , foldl (<>) (regPoly 6 1) [regularOffset i 6 4 (regPoly 4 1) | i <- [1..6]]
 , regPoly 6 w <> circle 0.3 # fc grey <>
     foldl (<>) mempty [regularOffset i 6 4 (text (show i) # fontSizeL 0.2 <> regPoly 4 w # fc blue) | i <- [0..5]]

 , foldl (<>) mempty [(regPoly n w # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , foldl (|||) mempty [(regPoly n w # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , regPoly 6 w <> circle 0.3 # fc grey <>
     foldl (<>) mempty [regularOffset i 6 n (text (show i) # fontSizeL 0.5 <> regPoly n w) | (i,n) <- (zip [0..5] [3..8])]
 , regPoly 4 w # rotate (15 @@ deg) <>
     foldl (<>) mempty [regularOffset i 4 n (text (show i) # fontSizeL 0.5 <> regPoly n w) | (i,n) <- (zip [0..3] [3..6])] # rotate (15 @@ deg)
 , foldl (<>) mempty [(cornerPoly n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , regCycle [3,4,6,4]
 ]

surround :: Diagram S.B
surround = undefined

regular :: Double -> Int -> [Int] -> [(Point', Point')]
regular _ _ _ = []
