{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation.Tesselation (myFigs) where

import Tesselation.Geometry
import qualified Diagrams.TwoD.Vector as T
import Data.Maybe (catMaybes)
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Data.Set as Set
import qualified Control.Monad.State as CMS

type TilingState = CMS.State (Set.Set (Int, Int))


newAngle :: Double -> [Int] -> Double
newAngle angle clock = sum (map innerAngle clock) + angle + pi

regTessBreadthMaze
  :: Int -- depth
  -> [Int] -- clock
  -> RoseTree Double
regTessBreadthMaze d0 c0
  | d0 < 1 = RoseTree 0 []
  | otherwise = (fmap (\(x,_,_) -> x) . fst)
              $ CMS.runState (CMS.foldM (flip (f 0)) (RoseTree (0, (0,0), c0) []) [0..d0]) Set.empty
  where
  f :: Int -- current depth
    -> Int -- depth for this iteration
    -> RoseTree (Double, (Double, Double), [Int])
    -> TilingState (RoseTree (Double, (Double, Double), [Int]))
  f dcur dmax (RoseTree x xs)
    | dcur < dmax = RoseTree x <$> mapM (f (dcur + 1) dmax) xs
    | dcur == dmax = RoseTree x <$> fmap catMaybes (mapM (step x) [0 .. length c0 - 1])
    | otherwise = error "The algorithm is broken"

  step :: (Double, (Double, Double), [Int]) -> Int -> TilingState (Maybe (RoseTree (Double, (Double, Double), [Int]))) 
  step (a, (x,y), c) i = do
      s <- CMS.get
      let a' = newAngle a (take i c)
          c' = reverse (take i c) ++ reverse (drop i c)
          (x',y') = (x + cos a', y + sin a')
          idx = (preciseHash x', preciseHash y')
      if Set.member idx s
        then return Nothing
        else do
          CMS.put (Set.insert idx s)
          return . Just $ RoseTree (a', (x',y'), c') []


regTessBreadthTree
  :: Int -- depth
  -> [Int] -- clock
  -> RoseTree Double
regTessBreadthTree d0 c0
  | d0 < 1 = RoseTree 0 []
  | otherwise = (fmap (\(x,_,_) -> x) . fst)
              $ CMS.runState (CMS.foldM (flip (f 0)) (RoseTree (0, (0,0), c0) []) [0..d0]) Set.empty
  where
  f :: Int -- current depth
    -> Int -- depth for this iteration
    -> RoseTree (Double, (Double, Double), [Int])
    -> TilingState (RoseTree (Double, (Double, Double), [Int]))
  f dcur dmax (RoseTree x@(_, (px, py), _) xs)
    | dcur < dmax = RoseTree x <$> mapM (f (dcur + 1) dmax) xs
    | dcur == dmax = RoseTree x <$> mapM (step x) [0 .. length c0 - 1]
    | otherwise = error "The algorithm is broken"
  f _ _ x@(RoseLeaf _) = return x

  step :: (Double, (Double, Double), [Int]) -> Int -> TilingState (RoseTree (Double, (Double, Double), [Int]))
  step (a, (x,y), c) i = do
      s <- CMS.get
      let a' = newAngle a (take i c)
          c' = reverse (take i c) ++ reverse (drop i c)
          (x',y') = (x + cos a', y + sin a')
          idx = (preciseHash x', preciseHash y')
      if Set.member idx s
        then return (RoseLeaf (a', (x',y'), c'))
        else do
            CMS.put (Set.insert idx s)
            return $ RoseTree (a', (x',y'), c') []


regTessMaze
  :: Int
  -> [Int]
  -> RoseTree Double
regTessMaze depth0 clock = fst $ CMS.runState (sTileTree (0,0) 0 depth0 clock) Set.empty where
  sTileTree
    :: (Double, Double)
    -> Double -- angle
    -> Int -- depth
    -> [Int] -- the set clock
    -> TilingState (RoseTree Double)
  sTileTree p a d c
    | d <= 0 = return $ RoseTree a []
    | otherwise = RoseTree a <$> fmap catMaybes (mapM (step p a d c) [0 .. length c - 1])

  step :: (Double, Double) -> Double -> Int -> [Int] -> Int -> TilingState (Maybe (RoseTree Double)) 
  step (x,y) a d c i = do
      s <- CMS.get
      let a' = newAngle a (take i c)
          c' = reverse (take i c) ++ reverse (drop i c)
          (x',y') = (x + cos a', y + sin a')
          idx = (preciseHash x', preciseHash y')
      if Set.member idx s
        then return Nothing
        else do
          CMS.put (Set.insert idx s)
          t <- sTileTree (x',y') a' (d-1) c'
          return (Just t)

regTessTree
  :: Int
  -> [Int]
  -> RoseTree Double
regTessTree depth0 clock = fst $ CMS.runState (sTileTree (0,0) 0 depth0 clock) Set.empty where
  sTileTree
    :: (Double, Double)
    -> Double -- angle
    -> Int -- depth
    -> [Int] -- the set clock
    -> TilingState (RoseTree Double)
  sTileTree p@(x,y) a d c
    | d <= 0 = return $ RoseLeaf a
    | otherwise = RoseTree a <$> mapM (step p a d c) [0 .. length c - 1]

  step :: (Double, Double) -> Double -> Int -> [Int] -> Int -> TilingState (RoseTree Double)
  step (x,y) a d c i = do
      s <- CMS.get
      let a' = newAngle a (take i c)
          c' = reverse (take i c) ++ reverse (drop i c)
          (x',y') = (x + cos a', y + sin a')
          idx = (preciseHash x', preciseHash y')
      if Set.member idx s
        then return $ RoseLeaf a'
        else CMS.put (Set.insert idx s) >> sTileTree (x',y') a' (d-1) c'

reflectBy :: Double -> Diagram S.B -> Diagram S.B
reflectBy a d =
  let d' = rotateBy (-1 * a) d
  in rotateBy a (d' ||| reflectX d')

myFigs :: Diagram S.B
myFigs = vcat
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

 -- [ renderAngleTree ( regTessTree 50 [6,6,6]     ) ||| renderAngleTree ( regTessMaze 50 [6,6,6]     ) ||| renderAngleTree ( regTessBreadthTree 12 [6,6,6]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [6,6,6]     )
 -- , renderAngleTree ( regTessTree 50 [3,4,6,4]   ) ||| renderAngleTree ( regTessMaze 50 [3,4,6,4]   ) ||| renderAngleTree ( regTessBreadthTree 12 [3,4,6,4]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,4,6,4]   )
 -- , renderAngleTree ( regTessTree 30 [3,12,12]   ) ||| renderAngleTree ( regTessMaze 30 [3,12,12]   ) ||| renderAngleTree ( regTessBreadthTree 10 [3,12,12]   ) ||| renderAngleTree ( regTessBreadthMaze 10 [3,12,12]   )
 -- , renderAngleTree ( regTessTree 50 [4,8,8]     ) ||| renderAngleTree ( regTessMaze 50 [4,8,8]     ) ||| renderAngleTree ( regTessBreadthTree 12 [4,8,8]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [4,8,8]     )
 -- , renderAngleTree ( regTessTree 50 [3,6,3,6]   ) ||| renderAngleTree ( regTessMaze 50 [3,6,3,6]   ) ||| renderAngleTree ( regTessBreadthTree 12 [3,6,3,6]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,6,3,6]   )
 -- , renderAngleTree ( regTessTree 50 [3,3,3,3,6] ) ||| renderAngleTree ( regTessMaze 50 [3,3,3,3,6] ) ||| renderAngleTree ( regTessBreadthTree 12 [3,3,3,3,6] ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,3,3,3,6] )
 -- , renderAngleTree ( regTessTree 40 [4,6,12]    ) ||| renderAngleTree ( regTessMaze 40 [4,6,12]    ) ||| renderAngleTree ( regTessBreadthTree 10 [4,6,12]    ) ||| renderAngleTree ( regTessBreadthMaze 10 [4,6,12]    )
 -- , renderAngleTree ( regTessTree 25 [3,3,4,3,4] ) ||| renderAngleTree ( regTessMaze 25 [3,3,4,3,4] ) ||| renderAngleTree ( regTessBreadthTree 6  [3,3,4,3,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,4,3,4] )
 -- , renderAngleTree ( regTessTree 25 [3,3,3,4,4] ) ||| renderAngleTree ( regTessMaze 25 [3,3,3,4,4] ) ||| renderAngleTree ( regTessBreadthTree 6  [3,3,3,4,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,3,4,4] )
 -- ]
