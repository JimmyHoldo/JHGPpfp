{-# LANGUAGE FlexibleContexts #-}
import Criterion.Main
import Data.List
import Control.Parallel
import System.Random
import Control.Parallel.Strategies


main = do
    putStrLn $ show $ "Choose a number for fib calculations:"
    str <- getLine
    let i = read str :: Integer
    putStrLn "Choose length of array:"
    str2 <- getLine
    let lengthList = read str2 :: Int
    g <- newStdGen
    let rndL = rndList lengthList g
    defaultMain [
        -- bench "qsort" $ nf (sum . qsort) rndL,
        -- bench "qsortp" $ nf (sum . (qsortp 10000)) rndL,
        bench "qsortpf" $ nf (sum . (qsortpf 10000)) rndL
      ]

-- | create a random list of length n
rndList :: (RandomGen t1, Random Int) => Int -> t1 -> [Int]
rndList 0 _ = []
rndList n g = nr : (rndList (n-1) g')
    where
        (nr,g') = randomR (1,50000) g
-----------------------------------------------------------------------------
-- Sequential fib                                                          --
-----------------------------------------------------------------------------
fib :: Integer -> Integer
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

-----------------------------------------------------------------------------
-- Parallel fib try 1                                                      --
-----------------------------------------------------------------------------

fib1 :: Integer -> Integer
fib1 1 = 1
fib1 2 = 1
fib1 n = fib1 (n-1) + (par (fib1 (n-1)) (fib1 (n-2)))

-----------------------------------------------------------------------------
-- Parallel fib try 2                                                      --
-----------------------------------------------------------------------------

fib2 :: Integer -> Integer
fib2 1 = 1
fib2 2 = 1
fib2 n = (par (fib2 (n-1)) (fib2 (n-2))) + fib2 (n-1)

-----------------------------------------------------------------------------
-- Parallel fib try 3                                                      --
-----------------------------------------------------------------------------

fib3 :: Integer -> Integer
fib3 1 = 1
fib3 2 = 1
fib3 n = (par fibn1 (fib3 (n-2))) + fibn1
    where fibn1 = fib3 (n-1)

-----------------------------------------------------------------------------
-- Sequential fib                                                          --
-----------------------------------------------------------------------------
qsort [] = []
qsort (x:xs) =
    qsort [y | y <- xs, y<x]
    ++ [x]
    ++ qsort [y | y <- xs, y>=x]


-----------------------------------------------------------------------------
-- Sequential fib                                                          --
-----------------------------------------------------------------------------
qsortp :: (Num a, Ord a) => Int -> [a] -> [a]
qsortp _ [] = []
qsortp n (x:xs) | n > 0 = a `par`(b `pseq` (a ++ [x] ++ b))
                | otherwise = a ++ [x] ++ b
    where
        a = qsortp c [y | y <- xs, y<x]
        b = qsortp c [y | y <- xs, y>=x]
        c = if (n == 0) then 0 else (n-1)


-----------------------------------------------------------------------------
-- Sequential fib                                                          --
-----------------------------------------------------------------------------
forcelist :: (Num a) => [a] -> ()
forcelist []     = ()
forcelist (x:xs) = x `pseq` forcelist xs


qsortpf :: (Num a, Ord a) => Int -> [a] -> [a]
qsortpf _ [] = []
qsortpf n (x:xs) | n > 0 = ( a) `par`
                            ((forcelist b) `pseq` ((forcelist a) `pseq`(a) ++ [x] ++ b))
                 | otherwise = a ++ [x] ++ b
    where
        a = qsortpf c [y | y <- xs, y<x]
        b = qsortpf c [y | y <- xs, y>=x]
        c = if (n == 0) then 0 else (n-1)
