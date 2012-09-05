import Data.List
import Data.Array
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Printf


--monotone

data Point a = P a a deriving ( Show , Ord , Eq ) 
data Turn = RS | L deriving ( Show , Ord  , Eq , Enum ) 

calAngle :: ( Num a , Ord a , Eq a ) => Point a -> Point a -> Point a -> Ordering
calAngle ( P x0 y0 ) ( P x1 y1 ) ( P x2 y2 ) = compare ( ( y1 - y0 ) * ( x2 - x0 ) ) ( ( y2 - y0 ) * ( x1 - x0 ) )


sortByangle :: ( Num a , Ord a , Eq a ) => [ Point a ] -> [ Point a ]
sortByangle ( x : xs ) = x : sortBy (\ p1 p2 -> calAngle x p1 p2 ) xs 

findTurn :: ( Num a , Ord a , Eq a ) => Point a -> Point a -> Point a -> Turn
findTurn ( P x0 y0 ) ( P x1 y1 ) ( P x2 y2 ) = 
   case compare ( ( y1 - y0 ) * ( x2 - x0 ) ) ( ( y2 - y0 ) * ( x1 - x0 ) ) of 
	    LT -> L 
	    _  -> RS 


computeHull :: ( Num a , Ord a , Eq a ) => [ Point a ] -> [ Point a ] -> [ Point a ] 
computeHull [ x ] ( z : ys ) = computeHull [ z , x ] ys 
computeHull ys [] =  ys 
computeHull u@( y : x : xs ) t@( z : ys )  
  | findTurn x y z == RS = computeHull ( x : xs ) t 
  | otherwise  = computeHull ( z : u ) ys 


convexHull :: ( Num a , Ord a , Eq a ) => [ Point a ] -> [ Point a ]
convexHull xs = final   where 
	t1@( x1 : y1 : xs1)  = sort xs 
	lhull = computeHull [ y1 , x1 ] xs1 
	t2@( x2 : y2 : xs2 ) = reverse t1 
	uhull = computeHull [ y2 , x2 ] xs2 
	final =  nub $  lhull ++  uhull
	


--end of monotone


caltmp :: ( Num a , Ord a , Floating a ) => Int -> Int -> Int -> Array Int ( Point a ) -> a 
caltmp a b c arr = area where 
	P x1 y1 = arr ! a 
	P x2 y2 = arr ! b 
	P x3 y3 = arr ! c 
	area = 0.5 * ( abs $ ( x1 * y2 + x2 * y3 + x3 * y1 )  -  ( y1 * x2 + y2 * x3 + y3 * x1 ) )
	 

calArea :: ( Num a , Ord a , Floating a ) => Int -> Int -> Int  -> Int  -> a -> Array Int ( Point a ) -> ( Int , Int , Int , a ) 
calArea a b c  n area arr
 | area' >= area = calArea a b ( mod ( c + 1 ) n )   n area' arr      --area a b  ( c + 1 )  is greater than area a b c
 | area'' >=  area  = calArea a ( mod ( b + 1 ) n )  c  n area'' arr   --check if area a ( b + 1 ) c is greater area a b c 
 | otherwise =  ( a , b , c , area ) 
	where 
	 area' = caltmp a b ( mod ( c + 1 ) n ) arr
	 area'' = caltmp a ( mod ( b + 1 ) n ) c arr 
		    
looPing :: ( Num a , Ord a , Eq a , Floating a ) => Int -> Int -> Int -> Int -> a -> a -> Array Int ( Point a ) -> a 
looPing a b c n area best arr  
 | a == n = max area best
 | otherwise = looPing a'' b'' c'' n area'' ( max area' best )  arr where 
	( a' , b' , c' , area' ) = calArea a b c n area arr 
	a'' = a' + 1 
	b'' = if a'' == b' then mod ( b' + 1 ) n else b'
	c'' = if b'' == c' then mod ( c' + 1 ) n else c'
 	area'' = caltmp ( mod a'' n ) b'' c'' arr

solve :: ( Num a , Ord a , Floating a ) => [ Point a ] ->  a 
solve [] = 0
solve [ p ] = 0
solve [ p1 , p2 ] = 0
solve arr =  
	looPing 0 1 2  n area area arr' where 
	n = length arr 
	arr' = listArray ( 0 , pred n ) arr
        area = caltmp 0 1 2 arr'	

 
final :: ( Num a , Ord a , RealFloat a ) => [ Point a ] -> a
final [] = 0
final [ p ] =  0 
final [ p1 , p2 ] =  0 
final arr = solve . convexHull $ arr 


format :: ( Num a , Ord a , Floating a ) => [ Int ] -> [ [ Point a ]]
format [] = []
format (x:xs ) =  t : format b  where
        ( a , b ) = splitAt ( 2 * x ) xs
        t = helpFormat a where
            helpFormat [] = []
            helpFormat ( x' : y' : xs' ) =  P ( fromIntegral x' ) ( fromIntegral  y' )  : helpFormat xs'
 
readD :: BS.ByteString -> Int
readD = fst . fromJust . BS.readInt
 
main = BS.interact $ BS.unlines . map ( BS.pack . ( printf "%.2f" :: Double -> String ) . final ) . format . concat . map  ( map  readD . BS.words ) . init  . BS.lines  


--main = interact $ unlines . map ( show . convexHull ) . format . concat . map ( map read . words ) . init . lines 

