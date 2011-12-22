-- | Provides a system for analysing image data.
module Graphics.Forensics.Analyser
       ( -- * Analyser
         Analyser(..)
         -- * Output
       , Analysis
         -- * Task handling
       , task
       , step
         -- * Evaluation
       , evaluate
         -- * Reporting
       , report
       , reportEntry
         -- ** Convenience functions
       , reportDebug
       , reportInfo
       , reportWarning
       , reportError
       , reportCritical
         -- ** Convenience text functions
       , whisper
       , inform
       , warn
       , yell
       , panic
         -- * Misc
       , readVersion
       ) where

import Control.DeepSeq
import Control.Monad.Progress (ProgressT)
import qualified Control.Monad.Progress as Progress
import Control.Monad.Writer

import qualified Data.Set as Set
import Data.Text (Text)
import Data.Version (Version, parseVersion)
import Data.Word

import Text.ParserCombinators.ReadP

import Graphics.Forensics.Report

-- | A monad modelling a running analysis.
type Analysis a = ProgressT Text (Writer Report) a

{-|
An 'Analyser' that analyses some type of item.

The analyser can yield 'Progress' indications during its execution, and
incrementally write to a 'Report' that is to be considered the result
of the analysis.
-}
data Analyser i =
  Analyser
  { -- | Analyse the given item with this analyser
    analyse :: i -> Analysis ()

    -- | The human-readable name of this analyser
  , name :: Text

    -- | The author of this analyser
  , author :: Text

    -- | The version of this analyser
  , version :: Version
  }

-- | Fully evaluates the argument, ensuring that it is evaluated
-- before the next monad action.
evaluate :: (NFData a) => a -> Analysis a
evaluate a = a `deepseq` return a

-- | Wraps an 'Analysis' in a task. This wrapping makes it possible to
-- monitor the progress of the task.
task :: Text            -- ^ Name of the task
        -> Word         -- ^ Number of steps required to complete the task
        -> Analysis a   -- ^ The 'Analysis' performing the task
        -> Analysis a
task = Progress.task

-- | Marks one step of the current 'Analysis' as completed. This
-- function may only be used if the current 'Analysis' performs a 'task'
step :: Analysis ()
step = Progress.step

-- | Merges a report with the report of this 'Analysis'
report :: Report -> Analysis ()
report = lift . tell

-- | Adds a report entry to the report of this 'Analysis'
reportEntry :: ReportEntry -> Analysis ()
reportEntry = report . Set.singleton

-- | Reports something with a 'DebugLevel' of importance
reportDebug :: Text -> ReportData -> Analysis ()
reportDebug msg = reportEntry . ReportEntry DebugLevel msg

-- | Reports something with an 'InformationLevel' of importance
reportInfo :: Text -> ReportData -> Analysis ()
reportInfo msg = reportEntry . ReportEntry InformationLevel msg

-- | Reports something with a 'WarningLevel' of importance
reportWarning :: Text -> ReportData -> Analysis ()
reportWarning msg = reportEntry . ReportEntry WarningLevel msg

-- | Reports something with an 'ErrorLevel' of importance
reportError :: Text -> ReportData -> Analysis ()
reportError msg = reportEntry . ReportEntry ErrorLevel msg

-- | Reports something with a 'CriticalLevel' of importance
reportCritical :: Text -> ReportData -> Analysis ()
reportCritical msg = reportEntry . ReportEntry CriticalLevel msg

-- | Logs a message with 'DebugLevel' importance
whisper :: Text -> Analysis ()
whisper = flip reportDebug ReportNothing

-- | Logs a message with 'InformationLevel' importance
inform :: Text -> Analysis ()
inform = flip reportInfo ReportNothing

-- | Logs a message with 'WarningLevel' importance
warn :: Text -> Analysis ()
warn = flip reportWarning ReportNothing

-- | Logs a message with 'ErrorLevel' importance
yell :: Text -> Analysis ()
yell = flip reportError ReportNothing

-- | Logs a message with 'CriticalLevel' importance
panic :: Text -> Analysis ()
panic = flip reportCritical ReportNothing

readVersion :: String -> Version
readVersion str = head [x | (x, "") <- readP_to_S parseVersion str]
