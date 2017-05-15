import Data.Array.Repa as Repa
import Data.List as List


-- Version using lists
buySellList :: [Int] -> (Int, Int, Int)
buySellList xs = buySellList' xs 0 (-1,9999999) (-1,-1, 0)


buySellList' :: [Int] -> Int -> (Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
buySellList' [] _ _ ret = ret
buySellList' (i:is) n (index, val) (b, s, p) | i <= val = buySellList' is (n+1) (n,i) (b, s, p)
                                             | (i-val) > p = buySellList' is (n+1) (index,val) (index, n, i-val)
                                             | otherwise = buySellList' is (n+1) (index, val) (b, s, p)

-- Array for testing
ins = fromListUnboxed (Z :. (8::Int)) [0,0,2,9,8,10,1,10] :: Array U DIM1 Int


-- Version using Arrays
buySell :: Array U DIM1 Int -> (Int, Int, Int)
buySell xs = buySell' xs 0 (-1,9999999) (-1,-1, 0)

buySell' :: Array U DIM1 Int -> Int -> (Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
buySell' is n (index, val) (b, s, p) 
         | n >= (size (extent is)) = (b, s, p)
         | (is ! (Z :. n)) <= val = buySell' is (n+1) (n,(is ! (Z :. n))) (b, s, p)
         | ((is ! (Z :. n))-val) > p = buySell' is (n+1) (index,val) (index, n, (is ! (Z :. n))-val)
         | otherwise = buySell' is (n+1) (index, val) (b, s, p)
