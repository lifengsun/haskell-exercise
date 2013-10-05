module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let m = read (args !! 0) :: Int
      n = read (args !! 1) :: Int
  putStrLn ("Sum = " ++ show (m + n))
