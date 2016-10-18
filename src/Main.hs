module Main where

import Control.Monad
import Data.List
import Data.List.Split
import Math.NumberTheory.Primes.Sieve
import System.Environment

data Function = Plain (Integer)
	| File String (String -> Integer)
	| Text String

triangleNumbers :: Integral a => [a]
triangleNumbers = unfoldr (\(x,y) -> Just (x,(x+y,y+1))) (1,2)

fibonacci :: Integral a => [a]
fibonacci = unfoldr (\(x,y) -> Just (x,(y,x+y))) (1,1)

isPrime :: Integral a => a -> Bool
isPrime = (==1) . length . primeFactor

primeFactor :: Integral a => a -> [a]
primeFactor 1 = []
primeFactor x = f:(primeFactor (x `div` f))
	where f = head $ filter ((==0) . mod x) $ (2:[3,5..])

factor :: Integral a => a -> [a]
factor n = nub $ concatMap (\x -> [div n x,x]) $ filter ((==0) . mod n) $ takeWhile ((<=n) . (^2)) [1..]

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

functionList :: [(Int,Function)]
functionList =
	[
		(0,Text
		  "Meow."
		)
		, (1, Plain $
			sum . filter (divisibleBy any [3,5]) $ [1..999]
		)
		, (2,Plain $
			sum . filter odd . takeWhile (<= 4000000) $ fibonacci
		)
		, (3,Plain $
			last $ primeFactor 600851475143
		)
		, (4,Plain $
			maximum . filter isPalindromDec . concat $ [[i*j|i <- [999,998..100]]|j <- [999,998..100]]
		)
		, (5,Plain $
			head . filter (divisibleBy all [2..20]) $ [(lcm 19 20),((lcm 19 20)*2)..]
		)
		, (6,Plain $
			((sum [1..100])^2)-(sum [i*i|i <- [1..100]])
		)
		, (7,Plain $
			primes!!10001
		)
		, (8,File "res/8.txt" $
			maximum . map (foldl (*) 1) . divvy 13 1 . map (read . ((flip (:)) [])) . filter (\x -> (x >= '0') && (x <= '9'))
		)
		, (9,Plain $
			head . head . filter (not . null) . concat . concat . concat $ [[[[[a*b*c|a^2+b^2 == c^2]|a+b+c == 1000]|a <- [1..1000]]|b <- [1..1000]]|c <- [1..1000]]
		)
		, (10,Plain $
			sum $ takeWhile (<2000000) primes
		)
		, (11,File "res/11.txt" $
			\input ->
				let
					matrix = chunksOf 20 $ map read $ words input
					mlen = length matrix
					horizontal = maximum . map (maximum . map (foldl (*) 1) . divvy 4 1)
					baseindices = concat [[(0,i),(i,0)]|i <- [0..(mlen-1)]]
					diagonalindices = map (\(x,y) -> filter (\(x,y) -> x < 20 && y < 20) $ [(x+i,y+i)|i <- [0..(mlen-1)]]) baseindices :: [[(Int,Int)]]
					diagonalvalues = \m -> map (map (\(x,y) -> (m!!y)!!x)) diagonalindices
					diagonal = maximum . map (foldl (*) 1) . concat . map (divvy 4 1) . diagonalvalues
				in
					maximum
						[
							horizontal matrix,
							horizontal $ transpose matrix,
							diagonal matrix,
							diagonal $ reverse matrix
						]
		)
		, (12,Plain $
			head . filter ((>=500) . length . factor) $ triangleNumbers
		)
		, (13,File "res/13.txt" $
			read . map (head . show) . take 10 . reverse . digitsDec . sum . map (read :: String -> Integer) . lines
		)
		-- TODO
		, (30,Plain $
			sum . filter (\x -> x == (sum . map (^5) . digitsDec $ x)) $ [2..(354294*2)]
		)
	]

run :: Function -> IO ()
run (Plain foo) = putStrLn $ show foo
run (File file foo) = do
	text <- readFile file
	putStrLn $ show $ foo text
run (Text text) = putStrLn text

main :: IO ()
main = do
	args <- getArgs 
	case lookup (read $ args!!0) functionList of
		Nothing -> putStrLn "Number not in list"
		Just foo -> run foo

