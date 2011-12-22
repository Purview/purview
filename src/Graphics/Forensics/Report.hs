-- | Structured reports representing completed analyses.
module Graphics.Forensics.Report
       ( -- * Report
         Report
         -- * Data
       , ReportEntry(..)
       , ReportLevel(..)
       , ReportData(..)
         -- ** Helper functions
       , reportNothing
       , reportImage
       , reportOffsetImage
       , reportShape
       , reportOffsetShape
       , reportMovedShape
       ) where

import Data.Set (Set)
import Data.Text (Text)
import Data.Vect
import Data.Word

import Graphics.Forensics.Image
import Graphics.Forensics.Shape

-- | The result of an analysis.
type Report = Set ReportEntry

-- | A part of a report
data ReportEntry =
  ReportEntry
  { reportLevel   :: ReportLevel    -- ^ Relevance level
  , reportMessage :: Text           -- ^ Human-readable message
  , reportData    :: ReportData     -- ^ Machine-readable data
  } deriving (Show, Eq)

instance Ord ReportEntry where
  a `compare` b =
    if compareLevels == EQ
    then reportMessage a `compare` reportMessage b
    else compareLevels
    where
      compareLevels = reportLevel a `compare` reportLevel b

-- | An indicator for how relevant a 'ReportEntry' is
data ReportLevel
    = DebugLevel        -- ^ Only used for debugging
    | InformationLevel  -- ^ Information only
    | WarningLevel      -- ^ Suspicious data
    | ErrorLevel        -- ^ Erroneous data
    | CriticalLevel     -- ^ All-or-nothing fatal indicator
      deriving (Show, Eq, Ord)

-- | Machine-readable data as part of a 'ReportEntry'
data ReportData
    = -- | There's no relevant data
      ReportNothing
      -- | Represents an image
    | ReportImage (Image Word8)
      -- | Represents an image at the given offset
    | ReportOffsetImage Vec2 (Image Word8)
      -- | Represents a shape
    | ReportShape Shape
      -- | Represents a shape at the given offset
    | ReportOffsetShape Vec2 Shape
      -- | Represents a shape that has been moved between offsets
    | ReportMovedShape Vec2 Vec2 Shape
      deriving (Show, Eq)

reportNothing :: ReportData
reportNothing = ReportNothing

reportImage :: Image Word8 -> ReportData
reportImage = ReportImage

reportOffsetImage :: Float -> Float -> Image Word8 -> ReportData
reportOffsetImage x y = ReportOffsetImage (Vec2 x y)

reportShape :: Shape -> ReportData
reportShape = ReportShape

reportOffsetShape :: Float -> Float -> Shape -> ReportData
reportOffsetShape x y = ReportOffsetShape (Vec2 x y)

reportMovedShape :: Float -> Float -> Float -> Float
                    -> Shape -> ReportData
reportMovedShape x0 y0 x1 y1 = ReportMovedShape (Vec2 x0 y0) (Vec2 x1 y1)
