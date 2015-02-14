module Stats.Stats(
    Stats(..)
  , StatType
  , stats
  , avg
  , cov
  , stdev) where

import Data.Int
import Data.List
import GHC.Float(float2Double)

-- * IDEAS:
--   - http://en.wikipedia.org/wiki/Correlation_and_dependence
--     Population correlation coefficient.
--          (cov(X,Y)/(stdev(X)*stdev(Y)))
--
--
-- | The statistics on a given dataset.  Each record entry corresponds
-- to some computation that the statistical analysis uses.
data Stats =
  Stats
  { stLen :: Int64
  , stSum :: Double
  , stMin :: Double
  , stMax :: Double
  , stAvg :: Double
  , stMed :: Double
  , stVar :: Double
  , stSdv :: Double
  , stCfv :: Double
  , stSer :: Double
  } deriving Show

-- | Calculates various stats given a sample of numeric types.
-- Each value is not strictly computed.  Hence, the caller
-- doesn't pay the overhead on stats that aren't used.  However,
-- some may be reused.
--
-- Note, the spread statistics are all corrected to be unbiased
-- If you want the population variance you can correct for this by
-- multiplying the variance by ((n - 1)/n) where n is the number of
-- samples.
--
-- We indicate a variance of @0.0@ on sample sets with fewer than
-- two elements.
--
-- The average of an empty list is @NaN@.
--
-- Example:
-- > *Util.Stats> stats [1,2,3,5]
-- > Stats {stLen = 4,
-- >        stSum = 11.0,
-- >        stMin = 1.0,
-- >        stMax = 5.0,
-- >        stAvg = 2.75,
-- >        stMed = 2.5,
-- >        stVar = 2.9166666666666665,
-- >        stSdv = 1.707825127659933,
-- >        stCfv = 0.6210273191490665,
-- >        stSer = 0.8539125638299665}
stats :: StatType a => [a] -> Stats
stats as = Stats n sum mn mx avg med var stdev cfv serr
  where (n,sum) = foldl' acc ((0 :: Int64),0) ads
        acc (n,sum) a = n `seq` sum `seq` (n + 1,sum + a)
        nd = fromIntegral n :: Double
        ads = map stToDouble as

        mn = minimum ads

        mx = maximum ads

        med
          | n == 0    = error "Tim.Stats.stats: median not defined on empty list"
          | odd  n    = sort ads !! mid
          | otherwise = let (x1:x2:_) = drop (mid - 1) (sort ads) in (x1 + x2) / 2
          where mid = fromIntegral (n `div` 2) :: Int

        avg = sum / nd

        var
          | n <= 1    = 0
          | otherwise = devsum / (fromIntegral (n - 1))
          where devsum = foldl' (\s a -> s + (a - avg)*(a - avg)) (0 :: Double) ads

        stdev = sqrt var

        cfv = stdev / avg

        serr = stdev / sqrt nd

-- | The type class for statistical types.
-- Instances must be able to convert to a 'Double'.
class StatType a where
  stToDouble :: a -> Double
instance StatType Double where
  stToDouble = id
instance StatType Float where
  stToDouble = float2Double
-- Doesn't work since resolution ignores the constraint
-- (someone could add @instance Integral Double ...@ tomorrow).
-- instance Integral a => StatType a where
--  stToDouble = fromIntegral
instance StatType Int where
  stToDouble = fromIntegral
instance StatType Int64 where
  stToDouble = fromIntegral

avg :: StatType a => [a] -> Double
avg = stAvg . stats

stdev :: StatType a => [a] -> Double
stdev = stSdv . stats


-- | The (unbiased) covariance of list of samples.  One can get the unbiased version
-- by multiplying @((n-1)/n)@ times the result.  The lists must all be the same
-- length.
--
-- Meaning:
--    * a positive value indicates the samples are generally going in the
--      same direction
--    * a negative sign indicates the opposite
--    * the magnitude indicates the strength of the correlation
cov :: StatType a => [[a]] -> Double
cov [] = 0
cov xssa
  | any ((/=n) . length) xssa = error "Stats.hs: cov: lists are of different length"
  | otherwise = the_sum / n_minus_one
  where n = length (head xssa)
        xss = map (map stToDouble) xssa
        devs = map (\xs -> let mean = avg xs in map (\x -> x - mean) xs) xss
        the_sum = sum (map product (transpose devs))
        n_minus_one = fromIntegral (length (head xss)) - 1




-- cov2 :: StatType a => [a] -> [a] -> Double
-- cov2 xsa ysa
--  | length xsa /= length ysa = error "Stats.hs: cov2: lists are different length"
--  | otherwise = foldl1' (+) devs / (fromIntegral (length xsa) - 1)
--  where (xs,ys) = (map stToDouble xsa, map stToDouble ysa)
--        (xbar,ybar) = (avg xs, avg ys)
--        devs = zipWith (\x y -> (x - xbar) * (y - ybar)) xs ys

-- TODO: text histograms
--   ppHistogram :: Num a => [a]
--
--

-- | From a Bryan O'Sullivan post, this illustrates how a jackknife mean looks.
-- This gives you an idea of how stable an estimator is.
--
-- http://www.serpentine.com/blog/2014/06/10/win-bigger-statistical-fights-with-a-better-jackknife/
{-
jackknifeMean :: Fractional a => [a] -> [a]
jackknifeMean xs =
    map (/ n) $
    zipWith (+)
    (init (scanl (+) 0 xs))
    (tail (scanr (+) 0 xs))
  where n = fromIntegral (length xs - 1)

-- | Similarly for the variance.
jackknifeVar :: Fractional a => [a] -> [a]
jackknifeVar xs =
    zipWith4 var squaresLeft squaresRight sumsLeft sumsRight
  where
    var l2 r2 l r = ((l2 + r2) - (l + r) ^ 2 / n) / n
    squares       = map (^2) xs
    squaresLeft   = init (scanl (+) 0 squares)
    squaresRight  = tail (scanr (+) 0 squares)
    sumsLeft      = init (scanl (+) 0 xs)
    sumsRight     = tail (scanr (+) 0 xs)
    n             = fromIntegral (length xs - 1)


data WelfordMean a = M !a !Int
              deriving (Show)

-- A more accuruate mean.
welfordMean = end . foldl' step zero
  where end  (M m _)   = m
        step (M m n) x = M m' n'
          where m'     = m + (x - m) / fromIntegral n'
                n'     = n + 1
        zero           = M 0 0
-}

