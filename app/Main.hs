module Main (main) where

import EuterpeaSandbox (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)
