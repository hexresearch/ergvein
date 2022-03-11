module Ergvein.Test where

foo :: [Int]
foo = [1 .. 9]

chunked :: Int -> [a] -> [[a]]
chunked n = reverse . fmap reverse . go 0 []
    where
        go :: Int -> [[a]] -> [a] -> [[a]]
        go i acc [] = acc
        go i [] (x : xs) = go (i+1) [[x]] xs
        go i (curr : acc) (x : xs)
            | i >= n = go 0 ([] : curr : acc) (x : xs)
            | otherwise = go (i+1) ((x : curr) : acc) xs

