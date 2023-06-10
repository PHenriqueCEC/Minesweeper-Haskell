import System.Random

data Cell = Cell { isMine :: Bool
                 , isOpen :: Bool
                 , isFlagged :: Bool
                 , nearbyMines :: Int
                 } deriving (Eq, Show)

data MinesweeperBoard = MinesweeperBoard { boardSize :: (Int, Int)
                                         , mineCount :: Int
                                         , cells :: [[Cell]]
                                         } deriving (Eq, Show)

readInt :: IO Int
readInt = readLn

createEmptyBoard :: (Int, Int) -> [[Cell]]
createEmptyBoard (rows, cols) = replicate rows (replicate cols (Cell False False False 0))

initBoard :: (Int, Int) -> Int -> IO MinesweeperBoard
initBoard (rows, cols) mineCount = do
  gen <- getStdGen
  let positions = take mineCount $ randomRs (0, rows * cols - 1) gen
      board = createEmptyBoard (rows, cols)
      boardWithMines = foldr (\pos b -> placeMine pos b) board positions
  return $ MinesweeperBoard (rows, cols) mineCount boardWithMines

placeMine :: Int -> [[Cell]] -> [[Cell]]
placeMine pos board = updateCell row col (\cell -> cell { isMine = True }) board
  where
    (row, col) = indexToPosition pos (length board)

updateCell :: Int -> Int -> (Cell -> Cell) -> [[Cell]] -> [[Cell]]
updateCell row col f board = take row board ++
                             [take col (board !! row) ++ [f (board !! row !! col)] ++ drop (col + 1) (board !! row)] ++
                             drop (row + 1) board

indexToPosition :: Int -> Int -> (Int, Int)
indexToPosition index cols = (index `div` cols, index `mod` cols)

validateNumBombs :: Int -> Int -> Int -> Bool
validateNumBombs numLines numColumns numBombs =
  numBombs <= (numLines * numColumns) `quot` 2

printBoard :: MinesweeperBoard -> IO ()
printBoard (MinesweeperBoard _ _ cells) =
  mapM_ (putStrLn . concatMap cellToChar) cells
  where
    cellToChar (Cell True _ _ _) = "X "
    cellToChar (Cell False isOpen _ _) = if isOpen then "_ " else "* "

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

  board <- initBoard (numLines, numColumns) numBombs
  printBoard board
