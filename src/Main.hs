module Main where

import Control.Monad
import System.Environment

functionList :: [(Int,IO String)]
functionList =
	[
		(0,return "Meow.")
	]

main :: IO ()
main = do
	args <- getArgs 
	case lookup (read $ args!!0) functionList of
		Nothing -> putStrLn "Number not in list"
		Just foo -> join $ (fmap (putStrLn)) foo

