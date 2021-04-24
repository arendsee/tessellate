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

-- get the unit location and hash of a child node a give angle from the parent
childLocation :: Double -> (Double, Double) -> ((Double, Double), (Int, Int))
childLocation a (x,y) = ((x', y'), idx) where
  (x',y') = (x + cos a, y + sin a)
  idx = (preciseHash x', preciseHash y')

toChild
  ::  (Double, (Double, Double), [Int]) -> Int -> Int
  -> ((Double, (Double, Double), [Int]), (Int, Int))
toChild (a, p, c) d i = ((a', p', c'), idx) where
  a' = a + pi + (if mod d 2 == 0 then -1 else 1) * sum (map innerAngle (take i c))
  c' = drop i c ++ take i c
  (p', idx) = childLocation a' p


toChildClockwise
  ::  (Double, (Double, Double), [Int]) -> Int -> Int
  -> ((Double, (Double, Double), [Int]), (Int, Int))
toChildClockwise (a, p, c) _ i = ((a', p', c'), idx) where
  a' = a + pi + sum (map innerAngle (take i c))
  c' = reverse (take i c) ++ reverse (drop i c)
  (p', idx) = childLocation a' p

regTessBreadthMaze
  :: Int -- depth
  -> [Int] -- clock
  -> RoseTree Double
regTessBreadthMaze d0 clock
  | d0 < 1 = RoseTree 0 []
  | otherwise = (fmap (\(x,_,_) -> x) . fst)
              $ CMS.runState (CMS.foldM (flip (f 0)) (RoseTree (0, (0,0), clock) []) [0..d0]) Set.empty
  where
  f :: Int -- current depth
    -> Int -- depth for this iteration
    -> RoseTree (Double, (Double, Double), [Int])
    -> TilingState (RoseTree (Double, (Double, Double), [Int]))
  f dcur dmax (RoseTree x xs)
    | dcur < dmax = RoseTree x <$> mapM (f (dcur + 1) dmax) xs
    | dcur == dmax = RoseTree x <$> fmap catMaybes (mapM (step x dcur) [0 .. length clock - 1])
    | otherwise = error "The algorithm is broken"

  step :: (Double, (Double, Double), [Int]) -> Int -> Int -> TilingState (Maybe (RoseTree (Double, (Double, Double), [Int]))) 
  step x d i = do
      s <- CMS.get
      let (x', idx) = toChild x d i
      if Set.member idx s
        then return Nothing
        else do
          CMS.put (Set.insert idx s)
          return . Just $ RoseTree x' []


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
  f dcur dmax (RoseTree x xs)
    | dcur < dmax = RoseTree x <$> mapM (f (dcur + 1) dmax) xs
    | dcur == dmax = RoseTree x <$> mapM (step x dcur) [0 .. length c0 - 1]
    | otherwise = error "The algorithm is broken"
  f _ _ x@(RoseLeaf _) = return x

  step :: (Double, (Double, Double), [Int]) -> Int -> Int -> TilingState (RoseTree (Double, (Double, Double), [Int]))
  step x d i = do
      s <- CMS.get
      let (x', idx) = toChild x d i 
      if Set.member idx s
        then return (RoseLeaf x')
        else do
            CMS.put (Set.insert idx s)
            return $ RoseTree x' []


regTessMaze
  :: Bool
  -> Int
  -> [Int]
  -> RoseTree Double
regTessMaze alt depth0 clock = fst $ CMS.runState (sTileTree (0,0) 0 depth0 clock) Set.empty where
  sTileTree
    :: (Double, Double)
    -> Double -- angle
    -> Int -- depth
    -> [Int] -- the set clock
    -> TilingState (RoseTree Double)
  sTileTree p a d c
    | d <= 0 = return $ RoseTree a []
    | otherwise = RoseTree a <$> fmap catMaybes (mapM (step p a d c) [0 .. length c - 1])

  toChild' = if alt then toChild else toChildClockwise

  step :: (Double, Double) -> Double -> Int -> [Int] -> Int -> TilingState (Maybe (RoseTree Double)) 
  step p a d c i = do
      s <- CMS.get
      let ((a', p', c'), idx) = toChild' (a, p, c) d i
      if Set.member idx s
        then return Nothing
        else do
          CMS.put (Set.insert idx s)
          Just <$> sTileTree p' a' (d-1) c'

myFigs = vcat
 [ renderAngleTree ( regTessMaze True 40 [4,4,4,4]   ) ||| renderAngleTree ( regTessMaze False 40 [4,4,4,4]   ) |||renderAngleTree ( regTessBreadthTree 12 [4,4,4,4]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [4,4,4,4]   )
 , renderAngleTree ( regTessMaze True 40 [6,6,6]     ) ||| renderAngleTree ( regTessMaze False 40 [6,6,6]     ) |||renderAngleTree ( regTessBreadthTree 12 [6,6,6]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [6,6,6]     )
 , renderAngleTree ( regTessMaze True 40 [3,4,6,4]   ) ||| renderAngleTree ( regTessMaze False 40 [3,4,6,4]   ) |||renderAngleTree ( regTessBreadthTree 12 [3,4,6,4]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,4,6,4]   )
 , renderAngleTree ( regTessMaze True 40 [3,12,12]   ) ||| renderAngleTree ( regTessMaze False 40 [3,12,12]   ) |||renderAngleTree ( regTessBreadthTree 12 [3,12,12]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,12,12]   )
 , renderAngleTree ( regTessMaze True 40 [4,8,8]     ) ||| renderAngleTree ( regTessMaze False 40 [4,8,8]     ) |||renderAngleTree ( regTessBreadthTree 12 [4,8,8]     ) ||| renderAngleTree ( regTessBreadthMaze 12 [4,8,8]     )
 , renderAngleTree ( regTessMaze True 40 [3,6,3,6]   ) ||| renderAngleTree ( regTessMaze False 40 [3,6,3,6]   ) |||renderAngleTree ( regTessBreadthTree 12 [3,6,3,6]   ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,6,3,6]   )
 , renderAngleTree ( regTessMaze True 40 [4,6,12]    ) ||| renderAngleTree ( regTessMaze False 40 [4,6,12]    ) |||renderAngleTree ( regTessBreadthTree 12 [4,6,12]    ) ||| renderAngleTree ( regTessBreadthMaze 12 [4,6,12]    )
 , renderAngleTree ( regTessMaze True 40 [3,3,3,3,6] ) ||| renderAngleTree ( regTessMaze False 40 [3,3,3,3,6] ) |||renderAngleTree ( regTessBreadthTree 12 [3,3,3,3,6] ) ||| renderAngleTree ( regTessBreadthMaze 12 [3,3,3,3,6] )
 , renderAngleTree ( regTessMaze True 25 [3,3,4,3,4] ) ||| renderAngleTree ( regTessMaze False 25 [3,3,4,3,4] ) |||renderAngleTree ( regTessBreadthTree 6  [3,3,4,3,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,4,3,4] )
 , renderAngleTree ( regTessMaze True 25 [3,3,3,4,4] ) ||| renderAngleTree ( regTessMaze False 25 [3,3,3,4,4] ) |||renderAngleTree ( regTessBreadthTree 6  [3,3,3,4,4] ) ||| renderAngleTree ( regTessBreadthMaze 6  [3,3,3,4,4] )
 -- [ renderAngleTree ( regTessBreadthTree 1 [3,3,3,4,4]   ) # showOrigin
 -- , hrule 10 # fc blue
 -- , renderAngleTree ( regTessBreadthMaze 1 [3,3,3,4,4]   ) # showOrigin
 ]
