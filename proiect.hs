import Control.Monad.Trans.State
import qualified Data.Map.Lazy as Map
import Data.Array
import System.Environment
import qualified Data.ByteString 
import Data.Text.Internal.Search as S

type Bmtable = Map.Map Char Int


pattern_sample1="aaa"
text_sample1="aaabbaaa"



badCharacter :: String -> Bmtable
badCharacter p = execState (st p indices) Map.empty where
    indices = [0, 1 .. (plen-1)]
    plen = length p --ma folosesc de functia de lungime a unui text
    st [x] _         = do l <- get
                          put (Map.insert x (length p) l) --pentru ultimul caracter
    st (x:xs) (y:ys) = do l <- get
                          put (Map.insert x (plen-y-1) l)   --tot adaug la tabel noi valori imrpeuna cu cheile lor (fiecare char cu shift) 
                          st xs ys
--comp ((length pattern)-1) txtpat pattern bmt



matching :: Int -> String-> String ->Bmtable -> Int 
matching indice txtpat pattern bmt = case comp indice txtpat pattern bmt of
                                     Just val -> val
                                     Nothing -> (-1)
                                        
                                       
                                       
                             

comp  :: Int -> String -> String -> Bmtable -> Maybe Int
comp i txtpat pattern  bmt= case caseEqual txtpat pattern i ((length pattern)-1) bmt of
                                  Nothing -> Just (i - length pattern+1)
                                  Just val -> if val >= length txtpat
                                               then Nothing
                                               else comp val txtpat pattern  bmt

   
                                               

caseEqual :: String ->String -> Int ->Int ->Bmtable-> Maybe Int
caseEqual text pattern indicet (-1) bmt     = Nothing   
caseEqual text pattern indicet indicep bmt= if ((text !! indicet)==(pattern !! indicep ))
                                             then caseEqual text pattern (indicet-1) (indicep-1) bmt                                   
                                             else  case Map.lookup(text !! indicet ) bmt of
                                               Just v->  Just (indicet+v)
                                               Nothing -> Just (indicet +(length pattern))
     
                                               

algoritm indiceprimar text_sample pattern_sample= matching indiceprimar text_sample pattern_sample (badCharacter pattern_sample)



algoritm1::Int -> String -> String -> [Int] -> [Int]
algoritm1 indiceprimar text_sample pattern_sample list= if (indiceprimar)>= length text_sample --daca cumva am ajuns peste lungimea textului de cautare
                                                        then do
                                                        reverse list
                                                        else
                                                        case algoritm indiceprimar text_sample pattern_sample of 
                                                        (-1) -> reverse (list)   
                                                        val-> algoritm1 (val + length (pattern_sample)) text_sample pattern_sample (val : list)
                         



booyer :: String -> String -> [Int]
booyer text pattern = if length pattern >  length text
                      then []
                      else if pattern=="" || text==""
                      then []
                      else do
                      let x=algoritm1  ((length pattern)-1) text pattern []
                      x

main :: IO()
main  = do
       if length pattern_sample1 > length text_sample1
       then do
       print ([] :: [Int])
       else do
       let x=algoritm1 0 text_sample1 pattern_sample1 []
       print(x)
       

--indices from data.Text
--prop  text pattern = booyer text pattern == S.indices (unpack 
--prop  text pattern =  booyer text pattern == S.indices (pack pattern) (pack text)
--quickCheck prop
--import Data.Text.Internal.Search as S
--import Data.Text
--import Test.QuickCheck
