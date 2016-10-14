module Main where

import Control.Monad
import Data.List
import System.Environment

fibonacci :: Integral a => [a]
fibonacci = unfoldr (\(x,y) -> Just (x,(y,x+y))) (1,1)

primes :: Integral a => [a]
primes = filter isPrime (2:[3,5..])

isPrime :: Integral a => a -> Bool
isPrime = (==1) . length . primeFactor

primeFactor :: Integral a => a -> [a]
primeFactor 1 = []
primeFactor x = f:(primeFactor (x `div` f))
	where f = head $ filter ((==0) . mod x) $ (2:[3,5..])

isPalindrom :: Integral a => a -> a -> Bool
isPalindrom base number = (take len dig) == (take len (reverse dig))
	where
		dig = digits base number
		len = div (length dig) 2

isPalindromBin = isPalindrom 2
isPalindromOct = isPalindrom 8
isPalindromDec = isPalindrom 10
isPalindromHex = isPalindrom 16

digits :: Integral a => a -> a -> [a]
digits base 0 = []
digits base number = (number `mod` base):(digits base $ number `div` base)

digitsBin = digits 2
digitsOct = digits 8
digitsDec = digits 10
digitsHex = digits 16

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
		, (4,return $ show $ maximum $ filter isPalindromDec $ concat $ [[i*j|i <- [999,998..100]]|j <- [999,998..100]])
		, (5,return $ show $ head $ filter (divisibleBy all [2..20]) $ [(lcm 19 20),((lcm 19 20)*2)..])
		, (6,return $ show $ (let s=(sum [1..100]) in s*s)-(sum [i*i|i <- [1..100]]))
		, (7,return $ show $ primes!!10001)
		-- TODO
		, (30,return $ show $ sum $ filter (\x -> x == (sum $ map (^5) $ digitsDec x)) [2..(354294*2)])
	]

main :: IO ()
main = do
	args <- getArgs 
	case lookup (read $ args!!0) functionList of
		Nothing -> putStrLn "Number not in list"
		Just foo -> join $ (fmap (putStrLn)) foo

