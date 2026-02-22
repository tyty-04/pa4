module Main where

import Data.List.Split (splitOn)
import Data.Char (isAscii, isAlpha, toLower)
import qualified Data.Map.Strict as M

main :: IO ()
main = prepare

prepare :: IO ()
prepare = do
    contents <- readFile "enwiki-2023-04-13.txt"
    let asciiPairs =
            [ (w, n)
            | line <- lines contents
            , [w, n] <- [words line]
            , all isAscii w
            ]
        processed = concatMap processPair asciiPairs
        counts = M.fromListWith (+) processed
        filtered = M.filter (>= 50) counts
        totalCount = sum (M.elems filtered)
        entropy = M.map (\c -> let p = fromIntegral c / fromIntegral totalCount in - logBase 2 p) filtered
    writeFile "entropy.txt" (unlines [w ++ " " ++ show e | (w, e) <- M.toAscList entropy])
    
    

processPair :: (String, String) -> [(String, Int)]
processPair (w, n) =
    let count   = read n
        lowered = map toLower w
        split   = splitOn "-" lowered
        alpha = map (filter isAlpha) split
        valid   = filter (not . null) alpha
    in [ (sub, count) | sub <- valid ]
