{-# LANGUAGE LambdaCase #-}

module Graphics.X11.Slide (
	Version, Slide, Page, Line,
	runSlide, text
	) where

import Control.Applicative
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import Control.Monad.State (StateT(..), evalStateT, gets)
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import System.Environment
import System.Exit
import Graphics.X11.Turtle
import Graphics.X11.Slide.Options

import qualified Data.List.NonEmpty as NE

runSlide :: Version -> Slide -> IO ()
runSlide v sld = do
	pn <- getProgName
	os <- getOptions
	st <- (mconcat <$>) $ sequence . map snd . sortBy (compare `on` fst)
		. map (getAction pn v) $ fst os
	print st
	sttc <- makeStatic st sld
	let	stte = makeState st sld
	runSlideS sld `runReaderT` sttc `evalStateT` stte

data Setting = Setting {
	stRatio :: Maybe Double,
	stBeginWith :: Maybe Int,
	stSvgPrefix :: Maybe FilePath
	} deriving Show

initialSetting :: Setting
initialSetting = Setting {
	stRatio = Nothing,
	stBeginWith = Nothing,
	stSvgPrefix = Nothing }

appendSettings :: Setting -> Setting -> Setting
appendSettings s1 s2 = Setting {
	stRatio = stRatio s1 <|> stRatio s2,
	stBeginWith = stBeginWith s1 <|> stBeginWith s2,
	stSvgPrefix = stSvgPrefix s1 <|> stSvgPrefix s2 }

instance Monoid Setting where
	mempty = initialSetting
	mappend = appendSettings

type ProgramName = String
type Version = [Int]

type Action = (Int, IO Setting)

getAction :: ProgramName -> Version -> Option -> Action
getAction pn v Version =
	(1, printVersion pn v >> exitSuccess)
getAction _ _ CountPages =
	(2, print (NE.length $ 123 :| []) >> exitSuccess)
getAction _ _ (OptRatio r) =
	(3, return $ initialSetting { stRatio = Just r })
getAction _ _ (OptPage p) =
	(3, return $ initialSetting { stBeginWith = Just p })
getAction _ _ (OptGetSvg fp) =
	(3, return $ initialSetting { stSvgPrefix = Just fp })

printVersion :: ProgramName -> Version -> IO ()
printVersion pn v = putStrLn . ((pn ++ "-") ++) . intercalate "." $ map show v

type SlideM = ReaderT Static (StateT State IO)

data Static = Static {
	ratio :: Double,
	slideField :: Field,
	pageTurtle :: Turtle,
	bodyTurtle :: Turtle,
	allPages :: Int,
	clock :: Chan (),
	end :: Chan (),
	svgPrefix :: Maybe FilePath }

data State = State {
	pageNumber :: Int,
	pageZipper :: Zipper Page,
	pageEnd :: Bool,
	allEnd :: Bool,
	needEnd :: Int,
	runTurtle :: Bool }

data Zipper a = Zipper [a] (NonEmpty a) deriving Show

type Slide = NonEmpty Page
type Page = NonEmpty Line
type Line = SlideM ()

makeStatic :: Setting -> Slide -> IO Static
makeStatic st sld = do
	fld <- openField
	topleft fld
	p <- newTurtle fld
	hideturtle p
	penup p
	t <- newTurtle fld
	hideturtle t
	penup t
	cl <- newChan
	e <- newChan
	return $ Static {
		ratio = fromMaybe 1 $ stRatio st,
		slideField = fld,
		pageTurtle = p,
		bodyTurtle = t,
		allPages = NE.length sld,
		clock = cl,
		end = e,
		svgPrefix = stSvgPrefix st }

makeState :: Setting -> Slide -> State
makeState st sld =  State {
	pageNumber = fromMaybe 1 $ stBeginWith st,
	pageZipper = Zipper [] sld,
	pageEnd = False,
	allEnd = False,
	needEnd = 0,
	runTurtle = True }

tstFstLine :: Slide -> Line
tstFstLine ((l :| _) :| _) = l

runSlideS :: Slide -> SlideM ()
runSlideS sld = do
	tstFstLine sld
	fld <- asks slideField
	liftIO $ do
		onkeypress fld $ \case
			'q' -> return False
			_ -> return True
		waitField fld


cvt :: Double -> SlideM Double
cvt x = asks $ (x *) . ratio

width, height :: SlideM Double
width = cvt 512
height = cvt 375

fontName :: String
fontName = "KochiGothic"

text :: String -> Line
text tx = do
	t <- asks bodyTurtle
	w <- width
	rt <- lift $ gets runTurtle
	fs <- cvt 13
	liftIO $ do
		sety t 100
		setx t $ w / 8
		setheading t 0
		write t fontName fs tx

-- sitext :: Double -> Double -> String -> Line
-- sitext s i tx st = do
