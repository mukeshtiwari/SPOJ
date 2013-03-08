import Data.List
import qualified Text.Parsec.ByteString as PB
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Data.ByteString.Char8 as BS
import Control.Applicative hiding ( ( <|> ) , many ) 

validChars :: PB.Parser Char
validChars  = alphaNum <|> oneOf "._" 

dontCare :: PB.Parser Char 
dontCare = oneOf "~!@#$%^&*()<>?,."

{--
emailAddress :: PB.Parser  String
emailAddress = do 
             _ <- many dontCare
             fi <- alphaNum
             se <- validChars
             th <- validChars
             fo <- validChars
             ft <- validChars
             restAddr <- many validChars
             let addr = fi : se : th : fo : ft : restAddr 
             char '@'
             dom <- many1 alphaNum 
             rest <- try ( string ".com" <|> string ".org"  
                  <|>  string ".edu" ) <|> try ( string ".co.in" )
             _ <- many dontCare
             return $  addr ++ (  '@': dom ++ rest ) 
   
--} 
          
emailAddress :: PB.Parser String
emailAddress = conCatfun <$> ( many dontCare *> alphaNum ) <*> validChars <*> 
               validChars <*> validChars <*> validChars <*> many alphaNum  <*> 
               ( char '@' *> many1 alphaNum ) <*> ( try ( string ".com" <|> 
               string ".org" <|>  string ".edu" ) <|> try ( string ".co.in" ) 
               <* many dontCare ) where 
                conCatfun a b c d e f dom rest = 
                       ( a : b : c : d : e : f ) ++ ( '@' : dom ) ++ rest 


collectEmail :: BS.ByteString -> String
collectEmail email = case  parse emailAddress "" email of
                        Right addr -> addr 
                        Left err ->  "" 

process :: ( Int , [ String ] ) -> BS.ByteString
process ( k , xs ) = ( BS.pack "Case " ) `BS.append` ( BS.pack . show $ k ) 
          `BS.append` ( BS.pack ": " ) `BS.append` ( BS.pack . show $ k ) 
          `BS.append` ( BS.pack "\n" ) `BS.append` ( BS.pack  
          (  unlines . filter ( not . null ) $  xs ) )

main = BS.interact $ BS.concat .  map process . zip [ 1.. ] . map ( map collectEmail . 
       BS.words ) . tail . BS.lines

