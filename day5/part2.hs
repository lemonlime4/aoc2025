import Data.Bifunctor (bimap)
import Data.List (sort)
type Number = Integer
type Interval = (Number, Number)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

converge :: Eq a => (a -> a) -> a -> a
converge f = fst . head . dropWhile (uncurry (/=)) . pairs . iterate f

combine :: Interval -> Interval -> Maybe Interval
combine (i0, i1) (j0, j1)
    | i0 <= j0 && j0 <= i1 = Just (i0, max i1 j1)
    | j0 <= i0 && i0 <= j1 = Just (j0, max i1 j1)
    | otherwise = Nothing

main :: IO ()
main = readFile "day5/input.txt" >>= print . main'
  where main' = total . converge mergeStep . parse

parse :: String -> [Interval]
parse = map parseRow  . words . takeFirstPart
  where
    takeFirstPart :: String -> String
    takeFirstPart s = map fst . takeWhile (/= ('\n', '\n')) $ zip s (drop 1 s)
    
    parseRow :: String -> Interval
    parseRow = bimap read (read . drop 1) . break (== '-')

mergeStep :: [Interval] -> [Interval]
mergeStep = sort . foldr incorporate []
  where
    incorporate :: Interval -> [Interval] -> [Interval]
    incorporate j (i:is) = case combine i j of
        Just k -> k:is
        Nothing -> i : incorporate j is
    incorporate j [] = [j]

total :: [Interval] -> Number
total = sum . map (succ . uncurry (flip (-)))