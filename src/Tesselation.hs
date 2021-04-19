{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation (myFigs) where

import Diagrams.Prelude
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Diagrams.TwoD.Vector as TwoD

data RoseTree a = RoseTree a [RoseTree a]

-- counvert a double to a large int that is used to check for node existence in
-- maps. The original doubles cannot be used because imprecision screw equality.
preciseHash :: Double -> Int
preciseHash x = round(x * 2^32)

ngon :: Int ->  Diagram S.B
ngon i = regPoly i 1 # lw veryThin

renderPath :: [Double] -> Diagram S.B
renderPath = lw veryThin . strokeLocTrail . fromOffsets . map (\x -> TwoD.e (x @@ rad))

angleStroke :: Double -> Diagram S.B
angleStroke a = fromOffsets [TwoD.e (a @@ rad)] # lw veryThin

renderAngleTree :: RoseTree Double -> Diagram S.B
renderAngleTree (RoseTree _ xs) = mconcat [beside (r2 (cos a, sin a)) (angleStroke a) (renderAngleTree x) | x@(RoseTree a _) <- xs]

-- This first draft of the tree maker does not ensure paths are unique, so the
-- tree grows exponentially. Every node in the tree Every node in the tree
-- stores an angle, which is the edge from its parent. To build the tree, draw
-- lines of unit length from root rotating by the given angle as you descend
-- through the children.
badTileTree
  :: Int   -- tree depth
  -> [Int] -- tiling pattern (e.g., 3.4.6.4)
  -> RoseTree Double -- this vertex
badTileTree = makeBadTileTree 0 where
  makeBadTileTree
    :: Double -- angle
    -> Int -- depth
    -> [Int] -- the set clock
    -> RoseTree Double
  makeBadTileTree angle d clock
    | d <= 0 = RoseTree angle []
    | otherwise = RoseTree angle
        [
          makeBadTileTree
            (newAngle angle (take (i+1) clock))  -- calculate the child angle relative to parent 
            (d-1)  -- one step closer to the end
            (drop i clock ++ take i clock)  -- shift the clock for the child
        | i <- [0 .. length clock - 1]
        ]

  newAngle :: Double -> [Int] -> Double
  newAngle angle clock = sum (map innerAngle clock)


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
     mconcat [regularOffset i 6 4 (text (show i) # fontSizeL 0.2 <> ngon 4 # fc blue) | i <- [0..5]]
 , mconcat [(ngon n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , foldl (|||) mempty [(ngon n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , ngon 6 <> circle 0.3 # fc grey <>
     mconcat [regularOffset i 6 n (text (show i) # fontSizeL 0.5 <> ngon n) | (i,n) <- (zip [0..5] [3..8])]
 , ngon 4 # rotate (15 @@ deg) <>
     mconcat [regularOffset i 4 n (text (show i) # fontSizeL 0.5 <> ngon n) | (i,n) <- (zip [0..3] [3..6])] # rotate (15 @@ deg)
 , mconcat [(cornerPoly n # fc green) | (i,n) <- (zip [1..10] [3..12])]
 , regCycle [3,4,6,4]
 , regCycle [3,3,3,4,4]
 , regCycle [12,3,12]
 , lw veryThin . strokeLocTrail . fromOffsets $ [TwoD.e (60 @@ deg), TwoD.e (30 @@ deg)]
 , renderAngleTree $ badTileTree 3 [3,4,6,4]
 , renderAngleTree $ badTileTree 3 [3,3,3,4,4]
 ]
