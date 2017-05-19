{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


import Data.Array.Repa as Repa
import Data.List
import Data.Functor.Identity
import System.Random
import Control.Monad
import Criterion.Main

-----------------------------------------------------------------------------
-- Main                                                             --
-----------------------------------------------------------------------------
main :: IO()
main = do
    putStrLn "Choose length of array:"
    str <- getLine
    let lengthList = read str :: Int
    g <- newStdGen
    let rndList = ins lengthList g
    let list = toList rndList
    -- putStrLn $ show $ buySellS rndList
    defaultMain
          [
           bench "buySellP" (whnf ((buySellP  )) rndList ),
           bench "buySell" (whnf ((buySell  )) rndList ),
           bench "buySellList" (whnf ((buySellList  )) list ),
           bench "buySellS" (whnf ((buySellS ) ) rndList)
           ]

rndList 0 _ = []
rndList n g = nr : (rndList (n-1) g')
    where
        (nr,g') = randomR (1,100) g

-- Array for testing [0,0,2,9,8,10,1,10]
ins n g = fromListUnboxed (Z :. (n::Int)) (rndList n g) :: Array U DIM1 Int


-----------------------------------------------------------------------------
-- Sequential                                                              --
-----------------------------------------------------------------------------

-- Version using lists
buySellList :: [Int] -> (Int, Int, Int)
buySellList xs = buySellList' xs 0 (-1,9999999) (-1,-1, 0)


buySellList' :: [Int] -> Int -> (Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
buySellList' [] _ _ ret = ret
buySellList' (i:is) n (index, val) (b, s, p)
    | i <= val    = buySellList' is (n+1) (n,i) (b, s, p)
    | (i-val) > p = buySellList' is (n+1) (index,val) (index, n, i-val)
    | otherwise   = buySellList' is (n+1) (index, val) (b, s, p)



-- Version using Arrays
buySell :: Array U DIM1 Int -> (Int, Int, Int)
buySell xs = buySell' xs 0 (-1,9999999) (-1,-1, 0)

buySell' :: Array U DIM1 Int -> Int -> (Int, Int)
                -> (Int, Int, Int) -> (Int, Int, Int)
buySell' is n (index, val) (b, s, p)
         | n >= (size (extent is)) = (b, s, p)
         | (is ! (Z :. n)) <= val
            = buySell' is (n+1) (n,(is ! (Z :. n))) (b, s, p)
         | ((is ! (Z :. n))-val) > p
            = buySell' is (n+1) (index,val) (index, n, (is ! (Z :. n))-val)
         | otherwise = buySell' is (n+1) (index, val) (b, s, p)


buySellS :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySellS arr = foldS fun (0, 0, 0) $  allBuySellProfitsS arr


allBuySellProfitsS :: Array U DIM1 Int -> (Array U DIM1 (Int, Int, Int))
allBuySellProfitsS arr =
    computeS $ (Repa.traverse arr
               (\(Z :. x) -> ix1 x)
               (\idx (Z :. b) ->
                 do let s = (getBestSellId b b arr)
                    (b,s,idx (ix1 s) - idx (ix1 b))))


-----------------------------------------------------------------------------
-- Parallel                                                              --
-----------------------------------------------------------------------------


-- allBuySellProfits arr = fromListUnboxed
--                         (Z:.(length result)) result :: Array U DIM1 (Int,Int,Int)
--     where
--         list = toList arr
--         result = run list 0
--
-- run [x] _    = []
-- run (x:xs) n = [(n,i,v-x) | (v,i) <- Data.List.zip xs [n+1..],v>x]
--                 Data.List.++ run xs (n+1)



buySellP :: Array U DIM1 Int -> Array U DIM0 (Int, Int, Int)
buySellP arr = runIdentity $ do
    arr' <- allBuySellProfits arr
    sol <- foldP fun (0, 0, 0) $ arr'
    return sol

allBuySellProfits :: (Monad m) =>
    Array U DIM1 Int -> m (Array U DIM1 (Int, Int, Int))
allBuySellProfits arr =
  computeP $ (Repa.traverse arr
              (\(Z :. x) -> ix1 x)
              (\idx (Z :. b) ->
                do let s = (getBestSellId b b arr)
                   (b,s,idx (ix1 s) - idx (ix1 b))))

fun :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
fun (a,b,c) (d,e,f) | c<f = (d,e,f)
                    | c==f && a<d = (d,e,f)
                    | otherwise = (a,b,c)


getBestSellId :: Int -> Int -> Array U DIM1 Int -> Int
getBestSellId id1 id2 arr
    | id1 == size(extent arr) = id2
    | (arr ! (Z :. id1)) > (arr ! (Z :. id2)) = getBestSellId (id1+1) id1 arr
    | otherwise = getBestSellId (id1+1) id2 arr






-----------------------------------------------------------------------------
