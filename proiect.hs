import Control.Monad.Trans.State
import qualified Data.Map.Lazy as Map
import Data.Array
type Bmtable = Map.Map Char Int

--sample
text_sample = "co"
pattern_sample = "-odsadsa "
--

badCharacter :: String -> Bmtable
badCharacter p = execState (st p indices) Map.empty where
    indices = [0, 1 .. (plen-1)]
    plen = length p --ma folosesc de functia de lungime a unui text
    st [x] _         = do l <- get
                          put (Map.insert x 1 l) --pentru ultimul caracter
    st (x:xs) (y:ys) = do l <- get
                          put (Map.insert x (plen-y-1) l)   --tot adaug la tabel noi valori imrpeuna cu cheile lor (fiecare char cu shift) 
                          st xs ys
--comp ((length pattern)-1) txtpat pattern bmt
match :: Int -> String-> String ->Bmtable -> Int 
match indice txtpat pattern bmt = case comp indice txtpat pattern bmt of
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
                                                                                                                                                              
algoritm indiceprimar= match indiceprimar text_sample pattern_sample (badCharacter pattern_sample)

algoritm1::Int -> [Int] -> [Int]
algoritm1 indiceprimar list= if (indiceprimar+length pattern_sample)>= length text_sample
                                 then do
                                 reverse list
                                 else
                                 case algoritm indiceprimar of 
                                 (-1) -> reverse (list)   
                                 val-> algoritm1 (val + length (pattern_sample)) (val : list)
                         


main Int :: IO()
main s = do
       if length pattern_sample > length text_sample
       then do
       print ([] :: [Int])
       else do
       let x=algoritm1 ((length pattern_sample)-1) []
       print(x)
      
