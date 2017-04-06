import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Control.Monad.Par

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion
-- ghc -O2 -threaded -rtsopts -evenlog given
-- ./given +RTS -N4 -s


data T a = T !a !Int


mean :: (RealFrac a) => [a] -> a
mean = fini . foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
      where m' = m + (x - m) / fromIntegral n'
            n' = n + 1


resamples :: Int -> [a] -> [[a]]
resamples k xs =
    take (length xs - k) $
    zipWith (++) (inits xs) (map (drop k) (tails xs))

-- | Jackknife using a map function utilizing par and pseq
pjackknife :: ([a] -> b) -> [a] -> [b]
pjackknife f = pmap f . resamples 500

-- | Jackknife using a map function utilizing the Eval monad.
rjackknife :: ([a] -> b) -> [a] -> [b]
rjackknife f = rmap f . resamples 500

-- | Jackknife using a map function utilizing Strategies.
sjackknife :: NFData b =>  ([a] -> b) -> [a] -> [b]
sjackknife f = smap f . resamples 500

-- | Jackknife using a map function utilizing the Par monad.
pmjackknife :: NFData b =>  ([a] -> b) -> [a] -> [b]
pmjackknife f = pmmap f . resamples 500

-- | Jackknife using a map function utilizing sequential map.
jackknife :: ([a] ->b) -> [a] -> [b]
jackknife f = map f . resamples 500

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]


main = do
  let (xs,ys) = splitAt 1500  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )

  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys

  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)

  defaultMain
        [
        --  bench "pjackknife" (nf (pjackknife  mean) rs),
         bench "pmjackknife" (nf (pmjackknife mean) rs)
        --  bench "rjackknife" (nf (rjackknife  mean) rs),
        --  bench "sjackknife" (nf (sjackknife  mean) rs),
        --  bench "jackknife"  (nf (jackknife  mean) rs)
         ]
  runSort
-------------------------------------------------------------------------------
-- map functions
-------------------------------------------------------------------------------
force :: a -> a
force x = x `pseq` x

forcelist :: [a] -> ()
forcelist []     = ()
forcelist (x:xs) = x `pseq` forcelist xs

-- | Map over a list using par and pseq
pmap :: (a -> b) -> [a] -> [b]
pmap _ []     = []
pmap f (x:xs) = (fx) `par` ((forcelist fxs) `pseq` (fx:fxs))
            where fx = f x
                  fxs = pmap f xs

-- | Map over a list using the Eval monad
rmap :: (a -> b) -> [a] -> [b]
rmap f xs = rmap' 5000 f xs

rmap' :: Int -> (a -> b) -> [a] -> [b]
rmap' _ _ []     = []
rmap' 0 f xs     = map f xs
rmap' n f (x:xs) = runEval $ do
                fx <- rpar (force(f x))
                fxs <- rseq (rmap' (n-1) f xs)
                return (fx:fxs)

-- | Map over a list with Strategies.
smap :: NFData t => (a -> t) -> [a] -> [t]
smap _ [] = []
smap f xs = (map f xs `using` parListChunk 200 rdeepseq)

pmmap :: NFData t => (a -> t) -> [a] -> [t]
pmmap f xs = runPar $ do
    a <- pmmap' 5000 f xs
    return a

-- | Map over a list using the Par monad.
pmmap' :: NFData t => Int -> (a -> t) -> [a] -> Par [t]
pmmap' _ _ []     = return []
pmmap' 0 f xs     = return $ map f xs
pmmap' n f (x:xs)  = do
               i <- new
               fork $ put i (f x)
               let xs' = (pmmap' (n-1) f xs)
               x <- get i
               xs'' <- xs'
               return (x:xs'')


-------------------------------------------------------------------
-- mergesort
------------------------------------------------------------------
merge :: Ord a => [a] -> [a] -> [a]
merge xs []     = xs
merge [] ys     = ys
merge (x:xs) (y:ys)
    | x <= y    = (force x):(merge xs (y:ys))
    | otherwise = (force y):(merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergsort []   = []
mergesort [x] = [x]
mergesort xs  = merge (mergesort ys) (mergesort zs)
        where
            (ys,zs) = splitAt (length xs `div` 2) xs

-----------------------------------------------------------------
-- mergesort Eval monad
-----------------------------------------------------------------
dcmergesort :: Ord a => [a] -> [a]
dcmergesort xs = dcmergesort' 4 xs

dcmergesort' :: (Ord a) => Int ->[a] -> [a]
dcmergesort' _ []  = []
dcmergesort' _ [x] = [x]
dcmergesort' 0 xs  = mergesort xs
dcmergesort' n xs  = merge ys zs
    where (ys,zs) = runEval $ do
                    zs <- rpar $ dcmergesort' (n-1) b
                    ys <- rpar $ dcmergesort' (n-1) a
                    rseq $ forcelist zs
                    return (ys,zs)
          (a,b)   = splitAt (length xs `div` 2) xs


------------------------------------------------------------------------------
-- mergesort Par Monad
------------------------------------------------------------------------------
pmergesort :: (Ord a, NFData a) => [a] -> [a]
pmergesort xs =   pmergesort' 4 xs


pmergesort' :: (Ord a, NFData a) => Int -> [a] -> [a]
pmergesort' _ [] =  []
pmergesort' _ [x] =  [x]
pmergesort' 0 xs =  (mergesort xs)
pmergesort' n xs = runPar $ do
    i <- new
    j <- new
    fork $ put i $ pmergesort' (n-1) (a)
    fork $ put j $ pmergesort' (n-1) (b)
    ys <- get i
    zs <- get j
    return (merge ( ys) ( zs))
  where (a,b) = splitAt (length xs `div` 2) xs

------------------------------------------------------------------------------
-- Run function for sorting
------------------------------------------------------------------------------
runSort = do
    let (xs,ys)   = splitAt 1500
                    (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )
    let rs  = crud xs ++ ys

    -- check if the sorted lists are equal.
    print("Check if the sorted lists are equal.")
    print (mergesort rs == dcmergesort rs)
    print (mergesort rs == pmergesort rs)
    putStrLn $  ""

    defaultMain
          [
        --    bench "mergesort" (nf (mergesort  ) rs),
        --    bench "dcmergesort" (nf (dcmergesort  ) rs),
        --    bench "pmergesort" (nf (pmergesort  ) rs)
           ]
