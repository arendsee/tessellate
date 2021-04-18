{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Tesselation
(
    regular
  , render
) where

import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG.CmdLine as S

type Point' = (Double, Double)

render :: [(Point', Point')] -> D.Diagram S.B
render _ = D.circle 1

regular :: Double -> Int -> [Int] -> [(Point', Point')]
regular _ _ _ = []
