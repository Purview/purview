import Prelude hiding (putStrLn)

import Control.Monad
import Control.Monad.Progress
import Control.Monad.Writer

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO
import Data.Time
import Data.Version

import qualified Graphics.Forensics.Analysers as Analysers
import qualified Graphics.Forensics.Analyser as Analyser
import qualified Graphics.Forensics.Image as Image
import qualified Graphics.Forensics.Report as Report

import System.Console.ANSI
import System.Console.CmdArgs.Implicit
import System.Environment as Environment
import System.IO (stderr)
import System.Locale

import qualified Data.Set as Set
import Graphics.Forensics.Report

data RunMode =
  ModeAnalyse
  { modeAnalyser    :: String
  , modeFile        :: String
  } |
  ModeList |
  ModeInfo
  { modeAnalyser    :: String
  }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  pname <- Environment.getProgName
  m <- cmdArgs_ $ makeModes pname :: IO RunMode
  case m of
    ModeAnalyse analyser file ->
      doAnalysis analyser file
    ModeList ->
      listAnalysers
    ModeInfo analyser ->
      showAnalyserInfo analyser

listAnalysers :: IO ()
listAnalysers =
  forM_ Analysers.analysers printAnalyserName
  where
    printAnalyserName = putStrLn . Analyser.name

showAnalyserInfo :: String -> IO ()
showAnalyserInfo analyserName =
  case maybeAnalyser of
    Nothing       -> putErrLn "There is no such analyser installed"
    Just analyser -> do
      putStrLn . Text.append "Name: " . Analyser.name $ analyser
      putStrLn . Text.append "Author: " . Analyser.author $ analyser
      putStrLn . Text.append "Version: " . Text.pack .
        showVersion . Analyser.version $ analyser
  where
    maybeAnalyser = findAnalyser analyserName

findAnalyser :: String -> Maybe (Analyser.Analyser Image.ByteImage)
findAnalyser analyserName =
  find hasRightName Analysers.analysers
  where
    foldName = Text.toCaseFold . Text.pack $ analyserName
    hasRightName = (== foldName) . Text.toCaseFold . Analyser.name

doAnalysis :: String -> String -> IO ()
doAnalysis analyserName fileName =
  case maybeAnalyser of
    Nothing       -> putErrLn "There is no such analyser installed"
    Just analyser -> do
      image <- Image.readImage fileName
      runAnalysis mempty $ Analyser.analyse analyser image
  where
    maybeAnalyser = findAnalyser analyserName

runAnalysis :: Report.Report -> Analyser.Analysis () -> IO ()
runAnalysis currReport analysis =
  case result of
    Left (cont, stack) -> do
      whenLoud $ renderTaskStack stack
      runAnalysis report cont
    Right () -> whenLoud $ do
      hClearFromCursorToScreenEnd stderr
      saveReport report
      putErrLn "Done."
  where
    (result, newReport) = runWriter . runProgressT $ analysis
    report = currReport `mappend` newReport

saveReport :: Report -> IO ()
saveReport = mapM_ processReportEntry . Set.toList

processReportEntry :: ReportEntry -> IO ()
processReportEntry (ReportEntry _ message dat) = do
  putStrLn message
  saveReportData dat

saveReportData :: ReportData -> IO ()
saveReportData (ReportNothing) =
  return ()
saveReportData (ReportImage i) = do
  timestamp <- getTime
  Image.writeImage ("output/output-" ++ timestamp ++ ".png") i
saveReportData _ =
  return ()

getTime :: IO (String)
getTime =
  liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime >>=
  return . formatTime defaultTimeLocale "%y%m%d-%H%M%S"

renderTaskStack :: TaskStack Text -> IO ()
renderTaskStack stackRev = do
  hHideCursor stderr
  when currentIsNew $ do
    putErrLn . renderNewTask numberOfTasks $ curr
    hClearFromCursorToScreenEnd stderr
  forM_ stack $ putErrLn . renderTask
  hClearFromCursorToScreenEnd stderr
  replicateM_ numberOfTasks moveUpTwice
  hShowCursor stderr
  where
    curr = head stackRev
    currentIsNew = taskStep curr == 0
    stack = reverse stackRev
    numberOfTasks = length stack
    moveUpTwice = hCursorUpLine stderr 2

renderNewTask :: Int -> Task Text -> Text
renderNewTask level t =
  Text.replicate level "-" <> "> " <> taskLabel t

renderTask :: Task Text -> Text
renderTask t =
  taskLabel t <> "\n" <> makeProgress t

makeProgress :: Task a -> Text
makeProgress t =
  "[" <> bar <> "]"
  where
    ratio =
      (fromIntegral . taskStep $ t) /
      (fromIntegral . taskTotalSteps $ t) :: Double
    barElemCount = round . (* 78) $ ratio
    restElemCount = 78 - barElemCount
    barElems = Text.replicate barElemCount "="
    restElems = Text.replicate restElemCount " "
    bar = barElems <> restElems

putErrLn :: Text -> IO ()
putErrLn = hPutStrLn stderr

makeModes :: String -> Annotate Ann
makeModes pname =
  modes_
  [ record (ModeAnalyse "" "")
    [ modeAnalyser :=
      def
      += typ "ANALYSER"
      += argPos 0
    , modeFile :=
      def
      += typ "IMAGE"
      += argPos 1
    ]
    += help "Analyses an image with the specified analyser"
    += name "analyse"
  , record ModeList []
    += help "Lists all available analysers"
    += name "list"
  , record (ModeInfo "")
    [ modeAnalyser :=
      def
      += typ "ANALYSER"
      += argPos 0
    ]
    += help "Shows information about the specified analyser"
    += name "info"
  ]
  += program pname
  += summary (pname ++ " v0.1")
  += verbosity