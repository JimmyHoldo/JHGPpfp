import Data.List
import System.Random
import Criterion.Main
import Control.Parallel
import Control.Parallel.Strategies

-- code borrowed from the Stanford Course 240h (Functional Systems in Haskell)
-- I suspect it comes from Bryan O'Sullivan, author of Criterion

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

jackknife :: ([a] -> b) -> [a] -> [b]
jackknife f = map f . resamples 500

crud = zipWith (\x a -> sin (x / 300)**2 + a) [0..]

main = do
  let (xs,ys) = splitAt 1500  (take 6000
                               (randoms (mkStdGen 211570155)) :: [Float] )
  -- handy (later) to give same input different parallel functions

  let rs = crud xs ++ ys
  putStrLn $ "sample mean:    " ++ show (mean rs)

  let j = jackknife mean rs :: [Float]
  putStrLn $ "jack mean min:  " ++ show (minimum j)
  putStrLn $ "jack mean max:  " ++ show (maximum j)
  defaultMain
        [
         bench "pjackknife" (nf (pjackknife  mean) rs),
         bench "rjackknife" (nf (rjackknife  mean) rs),
         bench "jackknife"  (nf (jackknife  mean) rs)
         ]

pmap :: (a -> b) -> [a] -> [b]
pmap _ [] = []
pmap f (x:xs) = par fx $ pseq fxs (fx:fxs)
            where fx = f x
                  fxs = pmap f xs


rmap :: (a -> b) -> [a] -> [b]
rmap _ [] = []
rmap f (x:xs) = runEval $ do
                fx <- rpar (f x) 
                fxs <- rseq (rmap f xs)
                return (fx:fxs)
