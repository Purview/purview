
import Prelude hiding (putStrLn)

import Control.Monad
import Control.Monad.Progress
import Control.Monad.Writer

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.IO
import Data.Version

import qualified Graphics.Forensics.Analysers as Analysers
import qualified Graphics.Forensics.Analyser as Analyser
import qualified Graphics.Forensics.Image as Image
import qualified Graphics.Forensics.Report as Report

import System.Console.ANSI
import System.Console.CmdArgs.Implicit
import System.Environment as Environment
import System.IO (stderr)

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
      hHideCursor stderr
      runAnalysis mempty $ Analyser.analyse analyser image
      hShowCursor stderr
  where
    maybeAnalyser = findAnalyser analyserName

runAnalysis :: Report.Report -> Analyser.Analysis () -> IO ()
runAnalysis currReport analysis =
  case result of
    Left (cont, stack) -> do
      renderTaskStack stack
      runAnalysis report cont
    Right () -> do
      hClearFromCursorToScreenEnd stderr
      putStrLn "Done."
  where
    (result, newReport) = runWriter . runProgressT $ analysis
    report = currReport `mappend` newReport

renderTaskStack :: TaskStack Text -> IO ()
renderTaskStack stackRev = do
  forM_ stack renderTask
  hClearFromCursorToScreenEnd stderr
  replicateM_ numberOfTasks moveUpTwice
  where
    stack = reverse stackRev
    numberOfTasks = length stack
    moveUpTwice = hCursorUpLine stderr 2

renderTask :: Task Text -> IO ()
renderTask t = do
  putErrLn . taskLabel $ t
  putErrLn . makeProgress $ t

makeProgress :: Task a -> Text
makeProgress t =
  "[" `Text.append` bar `Text.append` "]"
  where
    ratio =
      (fromIntegral . taskStep $ t) /
      (fromIntegral . taskTotalSteps $ t) :: Double
    barElemCount = round . (* 78) $ ratio
    restElemCount = 78 - barElemCount
    barElems = Text.replicate barElemCount "="
    restElems = Text.replicate restElemCount " "
    bar = barElems `Text.append` restElems

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
