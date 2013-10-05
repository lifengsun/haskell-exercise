{-# OPTIONS_GHC -Wall #-}

-- Ex.1
toDigits    :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = n `mod` 10 : toDigitsRev (n `div` 10)

-- Ex.2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $ foldr f ([], False) xs
  where f x (acc, True)  = (2 * x : acc, False)
        f x (acc, False) = (    x : acc, True)

-- Ex.3
sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

-- Ex.4
validate :: Integer -> Bool
validate = (==0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits

-- Ex.5
type Peg  = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = concat [(hanoi (n - 1) a c b), [(a, b)], (hanoi (n - 1) c a b)]

-- Ex.6
