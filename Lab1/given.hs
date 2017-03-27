import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies
import Data.Time.Clock (diffUTCTime, getCurrentTime)

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


pjackknife :: ([a] -> b) -> [a] -> [b]
pjackknife f = pmap f . resamples 500

rjackknife :: ([a] -> b) -> [a] -> [b]
rjackknife f = rmap f . resamples 500

sjackknife :: NFData b =>  ([a] -> b) -> [a] -> [b]
sjackknife f = smap f . resamples 500

jackknife :: ([a] ->b) -> [a] -> [b]
jackknife f = map f . resamples 500

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]


main = do
  let (xs,ys) = splitAt 1500  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )
  let (xs2,ys2) = splitAt 1500  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )
  let (xs3,ys3) = splitAt 1500  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )
  let (xs4,ys4) = splitAt 1500  (take 6000 (randoms (mkStdGen 211570155)) :: [Float] )

  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  let rs2 = crud xs2 ++ ys2
  let rs3 = crud xs3 ++ ys3
  let rs4 = crud xs4 ++ ys4

  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  -- start <- getCurrentTime
  -- pseq (sum(jackknife mean rs :: [Float])) (return ())
  -- end <- getCurrentTime
  -- putStrLn ("time: " ++ show (end `diffUTCTime` start) ++ " seconds")
  -- start2 <- getCurrentTime
  -- pseq (sum(pjackknife mean rs2 :: [Float])) (return ())
  -- end2 <- getCurrentTime
  -- putStrLn ("time: " ++ show (end2 `diffUTCTime` start2) ++ " seconds")
  -- start3 <- getCurrentTime
  -- pseq (sum(rjackknife mean rs3 :: [Float])) (return ())
  -- end3 <- getCurrentTime
  -- putStrLn ("time: " ++ show (end3 `diffUTCTime` start3) ++ " seconds")
  -- start4 <- getCurrentTime
  -- pseq (sum(sjackknife mean rs4 :: [Float])) (return ())
  -- end4 <- getCurrentTime
  -- putStrLn ("time: " ++ show (end4 `diffUTCTime` start4) ++ " seconds")
  defaultMain
        [
         bench "pjackknife" (nf (pjackknife  mean) rs),
         bench "rjackknife" (nf (rjackknife  mean) rs2),
         bench "sjackknife" (nf (sjackknife  mean) rs3),
         bench "jackknife"  (nf (jackknife  mean) rs4)
         ]

force :: a -> ()
force x = x `pseq` ()

forcelist :: [a] -> ()
forcelist [] = ()
forcelist (x:xs) = x `pseq` forcelist xs

pmap :: (a -> b) -> [a] -> [b]
pmap _ [] = []
pmap f (x:xs) = ( fx) `par` ((forcelist fxs) `pseq` (fx:fxs))
            where fx = f x
                  fxs = pmap f xs


rmap :: (a -> b) -> [a] -> [b]
rmap _ [] = []
rmap f (x:xs) = runEval $ do
                fx <- rpar ((f x))
                fxs <- rseq (rmap f xs)
                return (fx:fxs)

smap :: NFData t => (a -> t) -> [a] -> [t]
smap _ [] = []
smap f xs = (map f xs `using` parListChunk 100 rdeepseq)
