module Main where

import qualified Data.Map.Strict as M
import Data.List (isPrefixOf)
import qualified Data.Array as A
import Data.Char (isAsciiLower)

main :: IO ()
main = do
    em <- loadEntropy
    contents <- readFile "ishmael.dec"
    let m = filter isAsciiLower contents
        segmented= segment em m
    writeFile "ishmael.txt" (wrap60 (unwords segmented))

wrap60 :: String -> String
wrap60 = unlines . go
    where
        go "" = []
        go s = let (a, b) = splitAt 60 s in a : go b

loadEntropy :: IO (M.Map String Double)
loadEntropy = do
    contents <- readFile "entropy.txt"
    let entropyMap =
            M.fromAscList
                [ (w, read e :: Double)
                | line <- lines contents
                , [w, e] <- [words line] 
                ]
    pure entropyMap

prefixes :: M.Map String Double -> String -> [(String, Double)]
prefixes em s = go 1 []
    where
        n = length s
        go i prefs
            | i > n = prefs
            | otherwise =
                let p = take i s
                    (_, cost, gt) = M.splitLookup p em
                    prefs' = case cost of
                        Just c -> (p, c) : prefs
                        Nothing -> prefs
                in case M.lookupMin gt of
                    Just (k, _) -> if p `isPrefixOf` k then go (i + 1) prefs' else prefs'
                    Nothing -> prefs'

segment :: M.Map String Double -> String -> [String]
segment em m =
    let n = length m
        assocs = [ (i, bestAt i) | i <- [0..n-1]]
        entropyArr = A.array (0, n) $ (n, 0) : (n, 0) : [ (i, fst cw) | (i, cw) <- assocs ]
        wordArr = A.array (0, n-1) [ (i, snd cw) | (i, cw) <- assocs ]

        bestAt i =
            let suff = drop i m
                options = [ (c + entropyArr A.! (i + length w), w)
                        | (w, c) <- prefixes em suff
                        ]
            in if null options then (1/0, "") else minimum options

        output =
            let go i segs
                    | i >= n = reverse segs
                    | otherwise = go (i + length (wordArr A.! i)) (wordArr A.! i : segs)
            in go 0 []

    in output