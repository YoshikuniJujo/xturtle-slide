module Graphics.X11.Slide.Options (
	Option(..), getOptions
	) where

import Control.Monad
import System.Environment
import System.Console.GetOpt

data Option
	= Version
	deriving (Show, Eq)

getOptions :: IO ([Option], [String])
getOptions = do
	(opts, noOpts, emsg) <- getOpt Permute optDescrs <$> getArgs
	when (not $ null emsg) . error $ unlines emsg
	return (opts, noOpts)

optDescrs :: [OptDescr Option]
optDescrs = [
	Option "" ["version"] (NoArg Version) "show version"
	]
