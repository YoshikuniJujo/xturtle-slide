{-# LANGUAGE LambdaCase #-}

module Graphics.X11.Slide (
	Version, Slide, Page, Line,
	runSlide, writeTitle, pageTitle, text, itext, nextLine,
	writeImage
	) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ReaderT(..), runReaderT, asks)
import Control.Monad.State (StateT(..), evalStateT, get, gets, put)
import Control.Concurrent
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import Data.Function
import Data.Char
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
	(2, print (NE.length $ (123 :: Int) :| []) >> exitSuccess)
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

nonEmptyToZipper :: NonEmpty a -> Zipper a
nonEmptyToZipper = Zipper []

peekZipper :: Zipper a -> a
peekZipper (Zipper _ (x :| _)) = x

nextZipper :: Zipper a -> (Bool, Maybe (Zipper a))
nextZipper (Zipper b n) = case NE.uncons n of
	(x, Just n'@(_ :| l)) -> (not $ null l, Just $ Zipper (x : b) n')
	(_, Nothing) -> (False, Nothing)

nextZipper' :: Zipper a -> Zipper a
nextZipper' z@(Zipper b n) = case NE.uncons n of
	(x, Just n') -> Zipper (x : b) n'
	(_, Nothing) -> z

nextPage :: SlideM Bool
nextPage = do
	s <- get
	case nextZipper $ pageZipper s of
		(c, Just z') -> do
			put s { pageZipper = z', pageNumber = pageNumber s + 1 }
			return c
		(c, Nothing) -> return c

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
	pageZipper = iterate nextZipper' (nonEmptyToZipper sld) !!
		(fromMaybe 1 (stBeginWith st) - 1),
	pageEnd = False,
	allEnd = False,
	needEnd = 0,
	runTurtle = True }

{-
tstFstLine :: Slide -> Line
tstFstLine ((l :| _) :| _) = l
-}

runSlideS :: Slide -> SlideM ()
runSlideS _sld = do
	fld <- asks slideField
	c <- asks clock
	liftIO $ do
		onkeypress fld $ \case
			'q' -> return False
			' ' -> writeChan c () >> return True
			_ -> return True
	runPage =<< gets (peekZipper . pageZipper)
	loop $ do
		liftIO $ readChan c
		cnt <- nextPage
		runPage =<< gets (peekZipper . pageZipper)
		return cnt
	liftIO $ waitField fld

loop :: Monad m => m Bool -> m ()
loop m = do
	c <- m
	when c $ loop m

runPage :: Page -> SlideM ()
runPage p = do
	bt <- asks bodyTurtle
	pt <- asks pageTurtle
	w <- width
	h <- height
	c <- asks clock
	fs <- cvt 12
	apgs <- asks allPages
	pn <- gets pageNumber
	liftIO $ do
		clear pt
		goto pt (w * 44 / 50) (h * 48 / 50)
		write pt fontName fs $ show pn
		forward pt (2 * fs)
		write pt fontName fs $ "/" ++ show apgs
		clear bt
		goto bt (w / 8) (h / 8)
	sequence_ . NE.toList
		$ NE.intersperse (liftIO (readChan c) >> nextLine) p

nextLine :: SlideM ()
nextLine = do
	t <- asks bodyTurtle
	d <- cvt 24
	liftIO $ do
		setheading t (- 90)
		forward t d

{-
append :: NonEmpty a -> [a] -> NonEmpty a
append (x :| xs) ys = x :| xs ++ ys
-}

cvt :: Double -> SlideM Double
cvt x = asks $ (x *) . ratio

width, height :: SlideM Double
width = cvt 512
height = cvt 375

fontName :: String
fontName = "KochiGothic"

writeTitle :: String -> String -> Line
writeTitle ttl sttl = do
	t <- asks bodyTurtle
	w <- width
	h <- height
	dw <- cvt 20
	dw' <- cvt 12
	liftIO $ do
		hideturtle t
		speed t "fastest"
		goto t ((w - dw * myLength ttl) / 2) (h / 2)
		write t fontName dw ttl
		goto t ((w - dw' * myLength sttl + 4 * dw') / 2) (h / 2 + dw)
		write t fontName dw' sttl
		speed t "slow"

myLength :: String -> Double
myLength "" = 0
myLength (c : cs)
	| isAscii c = 0.7 + myLength cs
	| otherwise = 1.4 + myLength cs

pageTitle :: String -> Line
pageTitle ttl = do
	t <- asks bodyTurtle
	w <- width
	fs <- cvt 15
	liftIO $ do
		hideturtle t
		setx t $ w / 8
		write t fontName fs ttl
		setheading t (- 90)
		forward t fs

text :: String -> Line
text = itext 0

itext :: Double -> String -> Line
itext i tx = do
	t <- asks bodyTurtle
	w <- width
	_rt <- lift $ gets runTurtle
	fs <- cvt 13
	liftIO $ do
		setx t $ w / 8 + i * fs
		setheading t 0
		write t fontName fs tx
		showturtle t
		speed t "slow"
		forward t $ fs * myLength tx
		hideturtle t

-- sitext :: Double -> Double -> String -> Line
-- sitext s i tx st = do

writeImage :: Double -> Double -> Double -> Double -> FilePath -> Line
writeImage x y w h fp = do
	t <- asks bodyTurtle
	pw <- width
	ph <- height
	w' <- cvt w
	h' <- cvt h
	l <- cvt 3
	liftIO $ do
		(x0, _) <- position t
		goto t (x * pw) (y * ph)
		image t fp w' h'
		goto t x0 (y * ph + h' + l)
