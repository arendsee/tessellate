module Main where

import Tesselation
import System.Environment (getArgs)
import Data.List (unfoldr)
import Diagrams.Backend.SVG.CmdLine (mainWith)

split :: Eq a => a -> [a] -> [[a]] 
split sep = unfoldr f where
  f [] = Nothing
  f s = case break (== sep) s of
    (x,s') -> Just (x, drop 1 s')

readInt :: String -> Int
readInt s = case reads s :: [(Int,String)] of
  [(x, "")] -> x
  [] -> error "Expected a '.' delimited list of integers, such as '3.4.3.6'"

main = mainWith myFigs
