import System.Random
import Data.List (nub)

validateNumBombs :: Int -> Int -> Int -> Bool
validateNumBombs numLines numColumns numBombs =
  numBombs <= (numLines * numColumns) `div` 2

createMatrix :: Int -> Int -> IO [[String]]
createMatrix numLines numColumns = do
  gen <- newStdGen
  let bombPositions = generateBombPositions gen numLines numColumns
  return $ generateMatrix numLines numColumns bombPositions

generateBombPositions :: RandomGen g => g -> Int -> Int -> [(Int, Int)]
generateBombPositions gen numLines numColumns =
  take numBombs $ nub $ randomPositions gen
  where
    numBombs = (numLines * numColumns) `div` 2
    randomPositions = randomRs ((0, 0), (numLines - 1, numColumns - 1))

generateMatrix :: Int -> Int -> [(Int, Int)] -> [[String]]
generateMatrix numLines numColumns bombPositions =
  [[if (i, j) `elem` bombPositions then "*" else " " | j <- [0 .. numColumns - 1]] | i <- [0 .. numLines - 1]]

printMatrix :: [[String]] -> IO ()
printMatrix matrix = do
  putStrLn "Matriz:"
  mapM_ printRow matrix

printRow :: [String] -> IO ()
printRow row = do
  putStrLn $ unwords row

printBombPositions :: [(Int, Int)] -> IO ()
printBombPositions bombPositions = do
  putStrLn "Posições das Bombas:"
  mapM_ printPosition bombPositions

printPosition :: (Int, Int) -> IO ()
printPosition (i, j) = putStrLn $ "(" ++ show i ++ ", " ++ show j ++ ")"

main :: IO ()
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

  matrix <- createMatrix numLines numColumns

  printMatrix matrix

  let bombPositions = generateBombPositions (mkStdGen 0) numLines numColumns
  printBombPositions bombPositions
