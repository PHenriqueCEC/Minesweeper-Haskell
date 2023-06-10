module Main where

import Minefield

main :: IO ()
main = do
    minefield <- readMinefield
    putStrLn $ "Número de bombas válido! \n" ++ show (numBombs minefield)
