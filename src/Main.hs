module Main where

import Control.Monad
import Data.List
import System.Environment

fibonacci :: Integral a => [a]
fibonacci = unfoldr (\(x,y) -> Just (x,(y,x+y))) (1,1)

primeFactor :: Integral a => a -> [a]
primeFactor 1 = []
primeFactor x = f:(primeFactor (x `div` f))
	where f = head $ filter ((==0) . mod x) $ (2:[3,5..])

applyAll :: [a -> b] -> ([b] -> c) -> a -> c
applyAll mapper folder = folder . zipWith ($) mapper . replicate (length mapper)

divisibleBy :: Integral a => ((a -> Bool) -> [a] -> Bool) -> [a] -> a -> Bool
divisibleBy foo numbers = applyAll (map (flip mod) numbers) (foo (==0))

functionList :: [(Int,IO String)]
functionList =
	[
		(0,return "Meow.")
		, (1,return $ show $ sum $ filter (divisibleBy any [3,5]) [1..999])
		, (2,return $ show $ sum $ filter odd $ takeWhile (<= 4000000) fibonacci)
		, (3,return $ show $ last $ primeFactor 600851475143)
	]

main :: IO ()
main = do
	args <- getArgs 
	case lookup (read $ args!!0) functionList of
		Nothing -> putStrLn "Number not in list"
		Just foo -> join $ (fmap (putStrLn)) foo

