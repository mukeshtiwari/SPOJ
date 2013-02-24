import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe ( fromJust )
import Data.Bits

powM :: Integer -> Integer -> Integer ->  Integer
powM a d n = powM' a d n where 
  powM' a d n 
        | d == 0 = 1
        | d == 1 = mod a n
        | otherwise = mod q n  where
	       p = powM'  ( mod ( a^2 ) n ) ( shiftR d 1 ) n
	       q = if (.&.) d 1 == 1 then mod ( a * p ) n else p


calSum :: [ Integer ] -> Integer 
calSum [ n , k ] = ret  where 
    a = 2 * powM ( n - 1 ) k 10000007
    b = 2 * powM ( n - 1 ) ( n - 1 ) 10000007
    c = powM n k 10000007
    d = powM n n 10000007 
    ret = mod ( a + b + c + d ) 10000007 


readD :: BS.ByteString -> Integer 
readD = fst . fromJust . BS.readInteger 

main = BS.interact $  BS.unlines . map ( BS.pack . show . calSum . map readD ) . init . map BS.words . BS.lines