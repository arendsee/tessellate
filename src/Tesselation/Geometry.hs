{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation.Geometry (
    module Diagrams.Prelude
  , RoseTree(..)
  , renderAngleTree
  , preciseHash
  , ngon
  , renderPath
  , angleStroke
  , faceDistance
  , faceAngle
  , cornerDistance
  , cornerAngle
  , innerAngle
  , cornerPoly
  , regularOffset
  , regCycle
) where

import Diagrams.Prelude
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Diagrams.TwoD.Vector as TwoD

data RoseTree a = RoseTree a [RoseTree a] | RoseLeaf a deriving(Ord, Eq, Show)

instance Functor RoseTree where
  fmap f (RoseTree x xs) = RoseTree (f x) (map (fmap f) xs) 
  fmap f (RoseLeaf x) = RoseLeaf (f x)

roseAngle :: RoseTree Double -> Double
roseAngle (RoseTree a _) = a
roseAngle (RoseLeaf a) = a

renderAngleTree :: RoseTree Double -> Diagram S.B
renderAngleTree (RoseLeaf a) = angleStroke a
renderAngleTree (RoseTree _ xs) = foldl atop mempty (map f xs) where
  f x@(RoseTree a _) = translate (r2 (cos a, sin a)) $ atop (angleStroke (a + pi)) (renderAngleTree x)
  f (RoseLeaf a) = (angleStroke a)

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
