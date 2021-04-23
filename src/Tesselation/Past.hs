{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation.Past () where

import Tesselation.Geometry
import qualified Diagrams.TwoD.Vector as T
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Diagrams.TwoD.Vector as TwoD

-- This first draft of the tree maker does not ensure paths are unique, so the
-- tree grows exponentially. Every node in the tree Every node in the tree
-- stores an angle, which is the edge from its parent. To build the tree, draw
-- lines of unit length from root rotating by the given angle as you descend
-- through the children.
badTileTree
  :: Int   -- tree depth
  -> [Int] -- tiling pattern (e.g., 3.4.6.4)
  -> RoseTree Double -- this vertex
badTileTree = makeBadTileTree pi where
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
            (newAngle angle (take i clock))  -- calculate the child angle relative to parent 
            (d-1)  -- one step closer to the end
            (reverse (take i clock) ++ reverse (drop i clock))  -- shift the clock for the child
        | i <- [0 .. length clock - 1]
        ]

  -- the angle from the parent origin to the child origin
  --       c       
  --      / 
  --     /      
  --   p/_r_______      
  --                newAngle is angle r
  newAngle :: Double -> [Int] -> Double
  newAngle angle clock = -1 * sum (map innerAngle clock) + angle + pi

reflectBy :: Double -> Diagram S.B -> Diagram S.B
reflectBy a d =
  let d' = rotateBy (-1 * a) d
  in rotateBy a (d' ||| reflectX d')

myPastFigs :: Diagram S.B
myPastFigs = foldl (beside (r2 (0,-1))) mempty
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
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,4,6,4] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,3,3,4,4] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,12,12] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [4,8,8] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,3,4,3,4] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,6,3,6] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [3,3,3,3,6] | i <- [0..5]]
 , foldr1 (|||) [renderAngleTree $ badTileTree i [4,6,12] | i <- [0..5]]
 ]

pastFigsV2 = vcat
 [ beside (V2 0 (-1)) k t
 , beside (V2 0 1) k t
 , k # snugR <> (rotate (120 @@ deg) t # snugL)
 , rotate (10 @@ deg) (g # showOrigin # fc blue)
 , (translate (r2 (0.3, 0.5)) (ngon 5)) # showOrigin # fc grey  -- translate moves the objectin the given direction, leaving the origin where it was
 , circle 1 # centerX # showOrigin
 , fromVertices [p2 (0, 0), p2 (0,0.5), p2 (1,1)]
 , atPoints (regPoly 5 1) [text (show i) # fontSizeL 0.2 <> rotateBy (3/4 + i * 1/5) (circle 0.2 # fc blue <> fromVertices [p2 (0, 0), p2 (0,0.5), p2 (1,1)]) | i <- [0..]]
 , atPoints (regPoly 16 1) [text (show i) # fontSizeL 0.2 <> circle (0.2 * sqrt i) # fc blue | i <- [1..12]] <> regPoly 16 1
 , position
      [ (p, regPoly i (1/ fromIntegral i) # fc (colourConvert c) # lw veryThin)
      | i <- [3..8]
      , let p = lerp ((fromIntegral i-2) / 6) (p2 (0,0)) (p2 (10,10))
      , let c = blend ((fromIntegral i-2)/6) red green
      ]
 , fromOffsets [unitX, unitY, 2 *^ unit_X, unit_Y] # centerXY # showOrigin
 , fromOffsets [unitY, unitY, unitX, unitY, 10 *^ unit_X, unit_Y] # showOrigin
 , atop
     (reflectY . reflectX $ fromOffsets [T.e (a @@ deg) | a <- [10,20..180]] # showOrigin)
     (fromOffsets [T.e (a @@ deg) | a <- [10,20..270]] # showOrigin)
 , ((reflectBy (1/12) . reflectBy (1/12). reflectBy (1/12)) (ngon 6 # fc green)) # showOrigin
 ]
 where
  k = ngon 3 # fc blue
  t = ngon 4 # fc green
  g = scaleY 2 (ngon 4)
