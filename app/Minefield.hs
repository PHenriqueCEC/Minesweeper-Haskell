import Data.Char (chr)


module Minefield
    ( Minefield(..)
    , readMinefield
    ) where

data Minefield = Minefield
    { numLines :: Int
    , numColumns :: Int
    , numBombs :: Int
    }

validateNumBombs :: Int -> Int -> Int -> Bool
validateNumBombs numLines numColumns numBombs =
  numBombs <= (numLines * numColumns) `div` 2

readMinefield :: IO Minefield
readMinefield = do
    putStrLn "--------------------------------------------------"
    putStrLn "-----------------CAMPO MINADO---------------------"
    putStrLn "--------------------------------------------------"

    putStrLn "Digite o número de linhas: \n"
    numLinesStr <- getLine
    let numLines = read numLinesStr :: Int

    putStrLn "Digite o número de colunas: \n"
    numColumnsStr <- getLine
    let numColumns = read numColumnsStr :: Int

    let getNumBombs = do
          putStrLn "Digite o número de bombas: \n"
          numBombsStr <- getLine
          let numBombs = read numBombsStr :: Int
          let isValidNumBombs = validateNumBombs numLines numColumns numBombs
          if not isValidNumBombs
              then do
                  putStrLn "O número de bombas deve ser metade do tamanho do tabuleiro! \n"
                  getNumBombs
              else return numBombs

    numBombs <- getNumBombs

    return Minefield
        { numLines = numLines
        , numColumns = numColumns
        , numBombs = numBombs
        }

main :: IO ()
main = do
    minefield <- readMinefield
    putStrLn $ "Número de bombas válido! \n" ++ show (numBombs minefield)

getNumLines :: Minefield -> Int
getNumLines = numLines

getNumColumns :: Minefield -> Int
getNumColumns = numColumns

getNumBombs :: Minefield -> Int
getNumBombs = numBombs
