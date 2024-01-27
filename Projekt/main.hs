import Regex(generateSetFromRegex)
import SetOperations(nD)

import Data.Set (Set, fromList, map, union, unions, filter, elems, insert, intersection, delete)
import qualified Data.Set as Set

import qualified Data.Sequence as Seq

s :: Set String -> [Set String] -> Bool
s setOfWords [] = s setOfWords [delete "" (nD setOfWords setOfWords)]
s setOfWords alreadyGenerated = do
    let nextS = nD setOfWords (last alreadyGenerated) `union` nD (last alreadyGenerated) setOfWords
    if "" `elem` nextS || (nextS `intersection` setOfWords) /= Set.empty
        then False
    else if nextS `elem` alreadyGenerated
        then True
    else s setOfWords (alreadyGenerated ++ [nextS])

main :: IO ()
main = do
    putStrLn "Podaj regex: "
    line <- getLine
    let generatedSet = generateSetFromRegex [""] line
    print generatedSet
    let wordsSet = fromList generatedSet
    print (show (s wordsSet []))
