{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = let a = min (att - 1) 2
                                   d = min def 2
                                   battles = min a d
                               in do
                                 aDies <- randList a
                                 dDies <- randList d
                                 let aWins = attWins aDies dDies
                                 return $ Battlefield (att - battles + aWins) (def - aWins)
  where
    attWins xs ys = length . filter id $ zipWith (>)
                    (reverse . Data.List.sort $ xs)
                    (reverse . Data.List.sort $ ys)
    randList n = liftM (map unDV) . replicateM n $ die

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield att def) = if def <= 0 || att < 2
                               then do
                                   return b
                               else battle b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = do
  bResult <- replicateM n (attWin b)
  return $ (/(fromIntegral n)) . fromIntegral . length . filter id $ bResult
  where attWin b = do
          Battlefield att def <- invade b
          return (att > def)
        n = 1000

print m n = do
  x <- evalRandIO (successProb (Battlefield m n))
  putStrLn (show x)
