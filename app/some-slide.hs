module Main where

import Data.List.NonEmpty
import Graphics.X11.Slide

version :: Version
version = [0, 0, 0, 1]

main :: IO ()
main = runSlide version someSlide

someSlide :: Slide
someSlide = title :| [
	prelude
	]

title :: Page
title = writeTitle "サンプルのスライド" "スライドのシステムをテストする" :| []

prelude :: Page
prelude = pageTitle "テストのためのスライド" :| [
	text "これはテストのためのスライドです",
	text "2行目です",
	text "3行目ですよ"
	]
