module Main where

import Translator

main :: IO ()
main = print $ translateAbstract exampleTmpModule
