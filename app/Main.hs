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
    cellToChar (Cell False isOpen False nearbyMines) = if isOpen then show nearbyMines ++ " " else "* "
    cellToChar (Cell False _ True _) = "B "

openCell :: (Int, Int) -> MinesweeperBoard -> MinesweeperBoard
openCell (row, col) (MinesweeperBoard size count cells) =
  if isOpen cell || isFlagged cell
    then MinesweeperBoard size count cells
    else MinesweeperBoard size count (updateCell row col (\cell -> cell { isOpen = True }) cells')
  where
    cell = cells !! row !! col
    cells' = updateAdjacentCells (row, col) cells

flagCell :: (Int, Int) -> MinesweeperBoard -> MinesweeperBoard
flagCell (row, col) (MinesweeperBoard size count cells) =
  if isOpen cell
    then MinesweeperBoard size count cells
    else MinesweeperBoard size (count + if isFlagged cell then -1 else 1) (updateCell row col toggleFlag cells)
  where
    cell = cells !! row !! col
    toggleFlag c = c { isFlagged = not (isFlagged c) }

updateAdjacentCells :: (Int, Int) -> [[Cell]] -> [[Cell]]
updateAdjacentCells (row, col) cells = updateCell row col updateAdjacent cells
  where
    updateAdjacent cell =
      if isMine cell
        then cell
        else cell { nearbyMines = countAdjacentMines (row, col) cells }

countAdjacentMines :: (Int, Int) -> [[Cell]] -> Int
countAdjacentMines (row, col) cells = length $ filter isMine adjacentCells
  where
    adjacentCells = [ cells !! i !! j | i <- [row-1..row+1], j <- [col-1..col+1], isValidCell (i, j) ]
    isValidCell (i, j) = i >= 0 && i < numRows && j >= 0 && j < numCols
    numRows = length cells
    numCols = length (head cells)

validatePosition :: (Int, Int) -> MinesweeperBoard -> Bool
validatePosition (row, col) (MinesweeperBoard (numLines, numColumns) _ _) =
  row >= 0 && row < numLines && col >= 0 && col < numColumns

playGame :: MinesweeperBoard -> IO ()
playGame board = do
  putStrLn "Digite o comando (o para abrir, m para marcar, d para desmarcar):"
  command <- getLine
  case command of
    "o" -> do
      putStrLn "Digite a posição para abrir (linha coluna):"
      positionStr <- getLine
      let [row, col] = map read (words positionStr) :: [Int]
      if validatePosition (row, col) board
        then do
          let newBoard = openCell (row, col) board
          printBoard newBoard
          playGame newBoard
        else do
          putStrLn "Posição inválida!"
          playGame board
    "m" -> do
      putStrLn "Digite a posição para marcar (linha coluna):"
      positionStr <- getLine
      let [row, col] = map read (words positionStr) :: [Int]
      if validatePosition (row, col) board
        then do
          let newBoard = flagCell (row, col) board
          printBoard newBoard
          playGame newBoard
        else do
          putStrLn "Posição inválida!"
          playGame board
    "d" -> do
      putStrLn "Digite a posição para desmarcar (linha coluna):"
      positionStr <- getLine
      let [row, col] = map read (words positionStr) :: [Int]
      if validatePosition (row, col) board
        then do
          let newBoard = flagCell (row, col) board
          printBoard newBoard
          playGame newBoard
        else do
          putStrLn "Posição inválida!"
          playGame board
    _ -> do
      putStrLn "Comando inválido!"
      playGame board

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
  playGame board
