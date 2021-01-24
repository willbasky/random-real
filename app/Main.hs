module Main (main) where

import RandomReal (makeRandom)
import Control.Exception (bracket_)




main :: IO ()
main = do
  bracket_ (putStrLn "Run randomizer") (putStrLn "\nRandomizer finished") makeRandom
