import qualified Data.ByteString.Char8 as BS
import Data.List
import Data.Maybe ( fromJust )

continuedFraction :: Integer -> [ Integer ] 
continuedFraction n = map ( \ ( a , _ , _ ) -> a ) . iterate fun $ ( d , 0 , 1 ) 
                  where 
                  d = truncate . sqrt . fromIntegral $ n 
                  fun ( a0 , p0 , q0 ) = ( a1 , p1 , q1 ) where
                      p1 = a0 * q0 - p0
                      q1 = div ( n - p1 ^ 2 ) q0
                      a1 = div ( d + p1 ) q1 

pellSolver :: Integer -> BS.ByteString
pellSolver n 
  | perfectSqr n = BS.pack. show $ ( -1 ) 
  | otherwise =  ( BS.pack . show $ p ) `BS.append` ( BS.pack " " ) 
                 `BS.append` ( BS.pack.show $ q ) where 
           d = truncate . sqrt . fromIntegral $ n
           lst = takeWhile ( /= 2 * d ) . continuedFraction $ n 
           len = length lst
           r@( x : y : xs ) = take ( if even len then len else 2 * len ) . 
                              continuedFraction $ n
           ( p0 , p1 , q0 , q1 ) = ( x , x * y + 1 , 1 , y )
           ( p , _ ) = foldl' compute ( p1 , p0 ) $ xs
           ( q , _ ) = foldl' compute ( q1 , q0 ) $ xs
           compute :: ( Integer , Integer ) -> Integer -> ( Integer , Integer )
           compute ( p1 , p0 ) a = ( a * p1 + p0 , p1 )


perfectSqr :: Integer -> Bool
perfectSqr n = d * d == n where 
           d = truncate . sqrt . fromIntegral $ n 

readI :: BS.ByteString -> Integer
readI = fst . fromJust . BS.readInteger 

main = BS.interact $  BS.unlines . map ( pellSolver . readI ) . tail . BS.lines