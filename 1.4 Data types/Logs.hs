module Logs where

import Data.Time.Clock
import Data.Time.Format
import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry
	{ timestamp	:: UTCTime
	, logLevel 	:: LogLevel
	, message 	:: String }

instance Show LogEntry where
	show (LogEntry t l m) =
		timeToString t ++ ": " ++
		show l ++ ": " ++
		m

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString = show

main = do
      tm <- getCurrentTime
      print $ logEntryToString (LogEntry tm Info "Everything is awesome!")