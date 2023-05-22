validateNumBombs :: Int -> Int -> Int -> Bool
validateNumBombs numLines numColumns numBombs =
  numBombs <= (numLines * numColumns) `div` 2

main:: IO()
main = do
    putStrLn "--------------------------------------------------"
    putStrLn "-----------------CAMPO MINADO---------------------"
    putStrLn "--------------------------------------------------"

    putStrLn "Digite o número de linhas: "
    numLinesStr <- getLine
    let numLines = read numLinesStr :: Int

    putStrLn "Digite o número de colunas: "
    numColumnsStr <- getLine
    let numColumns = read numColumnsStr :: Int

    let getNumBombs = do
          putStrLn "Digite o número de bombas: "
          numBombsStr <- getLine
          let numBombs = read numBombsStr :: Int
          let isValidNumBombs = validateNumBombs numLines numColumns numBombs
          if not isValidNumBombs
              then do
                  putStrLn "O número de bombas deve ser metade do tamanho do tabuleiro"
                  getNumBombs
              else return numBombs

    numBombs <- getNumBombs

    putStrLn $ "Número de bombas válido: " ++ show numBombs

