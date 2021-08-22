import Hugs.Prelude
import DataFile

wordToken:: String -> [String]
wordToken x = words x

wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs 

uniqueBigramsHelper :: [String] -> [(String,String)]
uniqueBigramsHelper [x] = []
uniqueBigramsHelper (x1:x2:xs) = (x1,x2) : uniqueBigramsHelper (x2:xs)

uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams (x1:x2:xs) = unique (uniqueBigramsHelper (x1:x2:xs))

uniqueTrigramsHelper  :: [String] -> [(String,String,String)]
uniqueTrigramsHelper [x1,x2] = []
uniqueTrigramsHelper (x1:x2:x3:xs) = (x1,x2,x3) : uniqueTrigramsHelper (x2:x3:xs)

uniqueTrigrams  :: [String] -> [(String,String,String)]
uniqueTrigrams (x1:x2:x3:xs) = unique  (uniqueTrigramsHelper (x1:x2:x3:xs))
							
bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq []=[]
bigramsFreq (x:xs) = freqList (uniqueBigramsHelper(x:xs))


trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq[]=[]
trigramsFreq (x:xs) = freqList (uniqueTrigramsHelper(x:xs))

getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq x ((x1,x2):xs) = if x==x1
							then x2
							else getFreq x xs

generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((a,b,c),x) y = x/(getFreq (a,b) y)

genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] x =[]
genProbPairs (((a,b,c),x):xs) y = ((a,b,c),(generateOneProb ((a,b,c),x) y)) : genProbPairs xs y

generateNextWordHelper [a,b] [] = []
generateNextWordHelper [a,b] (((x1,x2,x3),p):xs) =  if a==x1 && b==x2 && p>0.3 && notElem x3 ["!", "#", "$", "%", "&", ",", ".", ":", ";", "?", "@", "`", "|", "~"]
														then x3 : generateNextWordHelper [a,b] xs
														else  generateNextWordHelper [a,b] xs

generateNextWord :: (Ord a, Fractional a) => [String] ->[((String,String,String),a)] -> String
generateNextWord [a,b] [] = "Sorry, it is not possible to infer from current database"
generateNextWord [a,b] (((x1,x2,x3),p):xs) =  if length (generateNextWordHelper [a,b] (((x1,x2,x3),p):xs))==0
												then "Sorry, it is not possible to infer from current database"
												else  (generateNextWordHelper [a,b] (((x1,x2,x3),p):xs))!! randomZeroToX (length (generateNextWordHelper [a,b] (((x1,x2,x3),p):xs))-1)

--generateText :: String -> Int -> String
generateText (x:xs) n  =  (x:xs)++" " ++ generateTextHelper2 (x:xs) n
--trigramsFreq(wordTokenList docs)
--bigramsFreq(wordTokenList docs)
-- (genProbPairs ((trigramsFreq(wordTokenList docs)) (bigramsFreq(wordTokenList docs))))
--generateNextWord (wordToken (x:xs)) (genProbPairs ((trigramsFreq(wordTokenList docs)) (bigramsFreq(wordTokenList docs))))

generateTextHelper = genProbPairs (trigramsFreq(wordTokenList docs)) (bigramsFreq(wordTokenList docs))
generateTextHelper2 (x:xs) n = if n>1
						then generateNextWord (wordToken (x:xs)) generateTextHelper ++ " "++ generateTextHelper2 ((getSecondWord(x:xs))++" "++ (generateNextWord (wordToken (x:xs)) generateTextHelper)++ " ")  (n-1)
						else generateNextWord (wordToken (x:xs)) generateTextHelper

getSecondWord (x:xs) = if x==' ' then xs else getSecondWord xs 

remove y [] = []
remove y (x:xs) = if not(y==x)
					then x : (remove y xs)
					else remove y xs

freq y [] = 0
freq y (x:xs) = if y==x
				then 1 + (freq y xs)
				else freq y xs
				
unique [] = []
unique (x:xs) = if elem x xs
				then unique xs
				else x : unique xs
				
freqList []=[]
freqList (x:xs) = 	(x,(freq x (x:xs))) : freqList (remove x xs)
----------------
--haskelltask
charToString :: Char -> String
charToString c = [c]

sentToken:: String -> [String]
sentToken []=[] 
sentToken (x:xs) = sentTokenHelper (words (x:xs))
sentTokenHelper :: [String] -> [String]
sentTokenHelper (x1:x2:xs) = if notElem x2  [".","!","?"]
							then sentTokenHelper ((x1++x2):xs)
							else (x1++x2): sentTokenHelper xs