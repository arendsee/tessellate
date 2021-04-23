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

myFigs = vcat
 [ renderAngleTree ( regTessTree 50 [6,6,6]     ) ||| renderAngleTree ( regTessMaze 50 [6,6,6]     ) ||| renderAngleTree ( regTessBreadthTree 12 [6,6,6]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [6,6,6]     )
 , renderAngleTree ( regTessTree 50 [3,4,6,4]   ) ||| renderAngleTree ( regTessMaze 50 [3,4,6,4]   ) ||| renderAngleTree ( regTessBreadthTree 12 [3,4,6,4]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,4,6,4]   )
 , renderAngleTree ( regTessTree 30 [3,12,12]   ) ||| renderAngleTree ( regTessMaze 30 [3,12,12]   ) ||| renderAngleTree ( regTessBreadthTree 10 [3,12,12]   ) ||| renderAngleTree ( regTessBreadthMaze 10 [3,12,12]   )
 , renderAngleTree ( regTessTree 50 [4,8,8]     ) ||| renderAngleTree ( regTessMaze 50 [4,8,8]     ) ||| renderAngleTree ( regTessBreadthTree 12 [4,8,8]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [4,8,8]     )
 , renderAngleTree ( regTessTree 50 [3,6,3,6]   ) ||| renderAngleTree ( regTessMaze 50 [3,6,3,6]   ) ||| renderAngleTree ( regTessBreadthTree 12 [3,6,3,6]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,6,3,6]   )
 , renderAngleTree ( regTessTree 50 [3,3,3,3,6] ) ||| renderAngleTree ( regTessMaze 50 [3,3,3,3,6] ) ||| renderAngleTree ( regTessBreadthTree 12 [3,3,3,3,6] ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,3,3,3,6] )
 , renderAngleTree ( regTessTree 40 [4,6,12]    ) ||| renderAngleTree ( regTessMaze 40 [4,6,12]    ) ||| renderAngleTree ( regTessBreadthTree 10 [4,6,12]    ) ||| renderAngleTree ( regTessBreadthMaze 10 [4,6,12]    )
 , renderAngleTree ( regTessTree 25 [3,3,4,3,4] ) ||| renderAngleTree ( regTessMaze 25 [3,3,4,3,4] ) ||| renderAngleTree ( regTessBreadthTree 6  [3,3,4,3,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,4,3,4] )
 , renderAngleTree ( regTessTree 25 [3,3,3,4,4] ) ||| renderAngleTree ( regTessMaze 25 [3,3,3,4,4] ) ||| renderAngleTree ( regTessBreadthTree 6  [3,3,3,4,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,3,4,4] )
 ]
