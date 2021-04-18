{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation (myFigs) where

import Diagrams.Prelude
import qualified Diagrams.Backend.SVG.CmdLine as S

type Point' = (Double, Double)

ngon :: Int ->  Diagram S.B
ngon i = regPoly i 1 # lw veryThin

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
  in translate (r2 (d * cos a, d * sin a)) $ ngon n

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
 [ circle 1
 , circle 1 # fc green <> pentagon 5 # fc red
 , circle 1 # fc green ||| pentagon 5 # fc red
 , ngon 6
 , atPoints (trailVertices $ regPoly 6 1) (repeat (circle 0.2 # fc blue)) <> ngon 6
 -- , foldl (<>) (ngon 6) [regularOffset i 6 4 (ngon 4) | i <- [1..6]]
 , ngon 6 <> circle 0.3 # fc grey <>
     foldl (<>) mempty [regularOffset i 6 4 (text (show i) # fontSizeL 0.2 <> ngon 4 # fc blue) | i <- [0..5]]
 , foldl (<>) mempty [(ngon n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , foldl (|||) mempty [(ngon n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , ngon 6 <> circle 0.3 # fc grey <>
     foldl (<>) mempty [regularOffset i 6 n (text (show i) # fontSizeL 0.5 <> ngon n) | (i,n) <- (zip [0..5] [3..8])]
 , ngon 4 # rotate (15 @@ deg) <>
     foldl (<>) mempty [regularOffset i 4 n (text (show i) # fontSizeL 0.5 <> ngon n) | (i,n) <- (zip [0..3] [3..6])] # rotate (15 @@ deg)
 , foldl (<>) mempty [(cornerPoly n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , regCycle [3,4,6,4]
 , regCycle [3,3,3,4,4]
 , regCycle [12,3,12]
 ]

surround :: Diagram S.B
surround = undefined

regular :: Double -> Int -> [Int] -> [(Point', Point')]
regular _ _ _ = []
