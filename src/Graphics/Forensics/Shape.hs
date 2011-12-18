module Graphics.Forensics.Shape where

import Data.Vect
{-| Some geometrical 2-dimensional 'Shape'.

The shape should be assumed to be located at the origin of the
coordinate system, but it does not neccessarily have to be positioned
in such a way that its center is at the origin.
-}
data Shape =
  -- | A rectangle with its lower corner at the origin
  Rectangle
  { rectangleSize :: Vec2
  } |

  -- | A circle centered on the origin
  Circle
  { circleRadius :: Float
  } |

  -- | A freeform path with arbitrary offsets from the origin
  Path
  { pathCommands :: [PathCommand]
  }

-- | A command indicating a continuation of a shape
data PathCommand
    = MoveTo  Vec2              -- ^ "Lift the pen" and put it at the pos
    | LineTo  Vec2              -- ^ Draw a line
    | QuadTo  Vec2 Vec2         -- ^ Draw a quadratic curve
    | CubicTo Vec2 Vec2 Vec2    -- ^ Draw a cubic spline
    | Close                     -- ^ Create a line to the starting position

pathVectors :: PathCommand -> [Vec2]
pathVectors (MoveTo v)         = [v]
pathVectors (LineTo v)         = [v]
pathVectors (QuadTo v1 v2)     = [v1, v2]
pathVectors (CubicTo v1 v2 v3) = [v1, v2, v3]
pathVectors (Close)            = []

allPathVectors :: [PathCommand] -> [Vec2]
allPathVectors = concatMap pathVectors

-- | The area of a shape
-- | This function is approximate
area :: Shape -> Float
area (Circle r)      = 2 * pi * r * r
area (Rectangle v)   = _1 v * _2 v
area (Path cmds)     = abs $ foldr (uncurry pArea) 0 (zip xs (tail xs)) where 
                       xs = allPathVectors cmds
                       pArea x x' acc = 0.5 * _1 x * _2 x' - _1 x' * _2 x + acc

-- | The bounding vector, covering the size of the whole shape
-- | This function is approximate
bounds :: Shape -> Vec2
bounds (Rectangle v) = v
bounds (Circle r)    = Vec2 (2 * r) (2 * r)
bounds (Path cmds)   = Vec2 x y where
                       x    = abs $ maximum xs - minimum xs
                       y    = abs $ maximum ys - minimum ys
                       xs   = map (_1) vecs
                       ys   = map (_2) vecs
                       vecs = allPathVectors cmds

-- | The offset to the center of the shape
center :: Shape -> Vec2
center (Rectangle v) = scalarMul 0.5 v
center (Circle _)    = Vec2 0 0
center (Path cmds)   = Vec2 x y where
                       x     = avg $ map (_1) vecs
                       y     = avg $ map (_2) vecs
                       vecs  = allPathVectors cmds
                       avg l = (sum l) / (fromIntegral $ length l)