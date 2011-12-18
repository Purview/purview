-- | Structured reports representing completed analyses.
module Graphics.Forensics.Report
       ( -- * Report
         Report(..)
         -- * Data
       , ReportEntry(..)
       , ReportLevel(..)
       , ReportData(..)
       ) where

import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.Vect
import Data.Word

import Graphics.Forensics.Image
import Graphics.Forensics.Shape

-- | The result of an analysis.
newtype Report =
  Report
  { reportEntries :: HashSet ReportEntry    -- ^ Report entries
  }

-- | A part of a report
data ReportEntry =
  ReportEntry
  { reportLevel   :: ReportLevel    -- ^ Relevance level
  , reportMessage :: Text           -- ^ Human-readable message
  , reportData    :: ReportData     -- ^ Machine-readable data
  }

-- | An indicator for how relevant a 'ReportEntry' is
data ReportLevel
    = DebugLevel        -- ^ Only used for debugging
    | InformationLevel  -- ^ Information only
    | WarningLevel      -- ^ Suspicious data
    | ErrorLevel        -- ^ Erroneous data
    | CriticalLevel     -- ^ All-or-nothing fatal indicator

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
