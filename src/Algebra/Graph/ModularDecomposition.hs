module Algebra.Graph.ModularDecomposition where

import qualified Algebra.Graph as D
import Algebra.Graph.Undirected
import Data.Set (Set)
import qualified Data.Set as S

{-
  "Vertex x splits M (or is a splitter of M), if there exist y ∈ M and z ∈ M
  such that xy ∈ E and xz ∉ E" -- Habib2010
-}
-- TODO: Algorithm not specified? Current implementation is quadratic
splits :: Ord a => Graph a -> a -> Bool
splits g x =
  let v = vertexList g
      p = splitEdge g x
   in or [p y z | y <- v, z <- v]

splitEdge :: Ord a => Graph a -> a -> a -> a -> Bool
splitEdge g x y z =
  let e = edgeSet g
   in (S.member (x, y) e || S.member (y, x) e)
        && not (S.member (x, z) e || S.member (z, x) e)

spine :: Graph a -> a -> [a]
spine = undefined

{-
Let v be an arbitrary vertex of a graph G = (V, E). The forcing graph F (G, v)
is a directed graph whose vertex set is V \ {v}. The arc xy exists if y is a
splitter for {x, v}
-}

-- Decomposition Tree
data DCT a = Series [DCT a] | Parallel [DCT a] | Prime [DCT a] | Vertex a

forcingDigraph :: Ord a => Graph a -> a -> D.Graph a
forcingDigraph g v =
  let splitters = splitEdge g v
      verts = filter (/= v) $ vertexList g
   in D.Overlay
        (D.vertices verts)
        (D.edges $ [(y, x) | x <- verts, y <- verts, splitters y x])

inputFigure10 :: Graph Int
inputFigure10 =
  let v = 6
   in edges
        [ (1, 2),
          (1, 5),
          (2, 5),
          (2, v),
          (2, 3),
          (3, 4),
          (3, 5),
          (5, v)
        ]
