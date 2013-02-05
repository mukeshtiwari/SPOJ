import Data.List

isprime :: Integer -> Bool
isprime n | n<=1 = False 
	  | otherwise = all ( (/=0).mod n ) . takeWhile ( \x -> x*x<= n) $ [2..]

count :: Integer -> Integer -> Integer
count 0 _ = 0 
count  n  p = div n p + count ( div n p ) p  

plist ::[Integer]
plist = filter isprime [2..]


solve :: Integer -> String
solve 1 = show 1
solve n = show a    where 
	p = takeWhile ( <=n) plist
	a = foldl (\acc x -> acc * ( 2 * ( count n x ) + 1 ) ) 1 p 
        

main = interact $ unlines . map ( solve . read ) . init . lines

