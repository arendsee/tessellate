{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation.Past () where

import Tesselation.Geometry
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
