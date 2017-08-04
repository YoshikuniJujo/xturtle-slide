module Graphics.X11.Slide (
	Version,
	runSlide
	) where

import Data.List
import Data.Function
import System.Environment
import System.Exit
import Graphics.X11.Slide.Options

runSlide :: Version -> IO ()
runSlide v = do
	pn <- getProgName
	os <- getOptions
	_ <- sequence . map snd . sortBy (compare `on` fst)
		. map (getAction pn v) $ fst os
	return ()

data Setting

type ProgramName = String
type Version = [Int]

type Action = (Int, IO Setting)

getAction :: ProgramName -> Version -> Option -> Action
getAction pn v Version = (1, printVersion pn v >> exitSuccess)

printVersion :: ProgramName -> Version -> IO ()
printVersion pn v = putStrLn . ((pn ++ "-") ++) . intercalate "." $ map show v
