import Control.Monad.Trans.State
import qualified Data.Map.Lazy as Map
import Data.Array
import Test.LeanCheck
import Test.QuickCheck
type Text = String
type Pattern = String
type ArrText = Array Int Char
type ArrPat = Array Int Char
type Bmtable = Map.Map Char Int

--sample
text_sample = "mere"
pattern_sample = "mere"
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

match :: String-> String ->Bmtable -> Maybe Int
match txtpat pattern bmt = comp ((length pattern)-1) txtpat pattern bmt

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
                                                                                                                                                              
algoritm = match text_sample pattern_sample (badCharacter pattern_sample)

main :: IO()
main = do
       if length pattern_sample > length text_sample
       then print("Nu se poate")
       else print(algoritm)