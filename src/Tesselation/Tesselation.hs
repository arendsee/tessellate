{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation.Tesselation (myFigs) where

import Tesselation.Geometry
import Data.Maybe (catMaybes)
import qualified Diagrams.Backend.SVG.CmdLine as S
import qualified Data.Set as Set
import qualified Control.Monad.State as CMS

type TilingState = CMS.State (Set.Set (Int, Int))


newAngle :: Double -> [Int] -> Double
newAngle angle clock = -1 * sum (map innerAngle clock) + angle + pi

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
    | d <= 0 = return $ RoseTree a []    
    | otherwise = RoseTree a <$> mapM (step p a d c) [0 .. length c - 1]

  step :: (Double, Double) -> Double -> Int -> [Int] -> Int -> TilingState (RoseTree Double)
  step (x,y) a d c i = do
      s <- CMS.get
      let a' = newAngle a (take i c)
          c' = reverse (take i c) ++ reverse (drop i c)
          (x',y') = (x + cos a', y + sin a')
          idx = (preciseHash x', preciseHash y')
      if Set.member idx s
        then return $ RoseTree a' []
        else CMS.put (Set.insert idx s) >> sTileTree (x',y') a' (d-1) c'

myFigs :: Diagram S.B
myFigs = foldl (===) mempty
 [ renderAngleTree ( regTessTree 100 [3,4,6,4] ) ||| renderAngleTree ( regTessMaze 100 [3,4,6,4] )
 , renderAngleTree ( regTessTree 100 [3,12,12] ) ||| renderAngleTree ( regTessMaze 100 [3,12,12] )
 , renderAngleTree ( regTessTree 100 [4,8,8] ) ||| renderAngleTree ( regTessMaze 100 [4,8,8] )
 , renderAngleTree ( regTessTree 20 [3,6,3,6] ) ||| renderAngleTree ( regTessMaze 20 [3,6,3,6] )
 , renderAngleTree ( regTessTree 20 [3,3,3,3,6] ) ||| renderAngleTree ( regTessMaze 20 [3,3,3,3,6] )
 , renderAngleTree ( regTessTree 100 [4,6,12] ) ||| renderAngleTree ( regTessMaze 100 [4,6,12] )
 , renderAngleTree $ regTessTree 5 [3,3,4,3,4]
 , renderAngleTree $ regTessTree 5 [3,3,3,4,4]
 ]
