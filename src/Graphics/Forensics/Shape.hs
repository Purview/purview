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
    = MoveTo Vec2               -- ^ "Lift the pen" and put it at the pos
    | LineTo Vec2               -- ^ Draw a line
    | QuadTo Vec2 Vec2          -- ^ Draw a quadratic curve
    | CubicTo Vec2 Vec2 Vec2    -- ^ Draw a cubic spline
    | Close                     -- ^ Create a line to the starting position

-- | The area of a shape
area :: Shape -> Float
area = undefined -- TODO

-- | The bounding vector, covering the size of the whole shape
bounds :: Shape -> Vec2
bounds = undefined -- TODO

-- | The offset to the center of the shape
center :: Shape -> Vec2
center = undefined -- TODO
