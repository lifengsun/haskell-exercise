module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "please a name: "
  line <- getLine
  putStrLn ("the name is: " ++ line)
