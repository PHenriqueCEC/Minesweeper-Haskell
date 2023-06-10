module Main where

import System.Random (randomRIO)

data Tile = Mine | Safe Int deriving Eq

type Board = [[Tile]]

-- Função para imprimir o tabuleiro
printBoard :: Board -> IO ()
printBoard board = mapM_ putStrLn (map (concatMap showTile) board)
  where
    showTile :: Tile -> String
    showTile Mine = "* "
    showTile (Safe 0) = ". "
    showTile (Safe n) = show n ++ " "

-- Função para gerar um tabuleiro vazio
emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = replicate rows (replicate cols (Safe 0))

-- Função para adicionar minas aleatoriamente ao tabuleiro
addMines :: Int -> Int -> Int -> Board -> IO Board
addMines numMines rows cols board
  | numMines <= 0 = return board
  | otherwise = do
      row <- randomRIO (0, rows - 1)
      col <- randomRIO (0, cols - 1)
      let tile = board !! row !! col
      if tile == Mine
        then addMines numMines rows cols board
        else addMines (numMines - 1) rows cols (replaceTile row col Mine board)

-- Função auxiliar para substituir um elemento em uma lista
replace :: Int -> a -> [a] -> [a]
replace index newVal list = take index list ++ [newVal] ++ drop (index + 1) list

-- Função para substituir uma tile em um tabuleiro
replaceTile :: Int -> Int -> Tile -> Board -> Board
replaceTile row col tile board =
  replace row newRow board
  where
    rowTiles = board !! row
    newRow = replace col tile rowTiles

-- Função para contar o número de minas adjacentes a uma tile
countAdjacentMines :: Int -> Int -> Board -> Int
countAdjacentMines row col board =
  length $ filter (== Mine) adjacentTiles
  where
    adjacentTiles =
      [ board !! r !! c
      | r <- [row - 1 .. row + 1]
      , c <- [col - 1 .. col + 1]
      , r >= 0 && r < numRows
      , c >= 0 && c < numCols
      , (r, c) /= (row, col)
      ]
    numRows = length board
    numCols = length (head board)

-- Função para atualizar as dicas de minas vizinhas no tabuleiro
updateHints :: Board -> Board
updateHints board =
  [ [updateTile r c tile | (c, tile) <- zip [0 ..] row]
  | (r, row) <- zip [0 ..] board
  ]
  where
    updateTile :: Int -> Int -> Tile -> Tile
    updateTile r c Mine = Mine
    updateTile r c (Safe _) =
      let numAdjacentMines = countAdjacentMines r c board
       in Safe numAdjacentMines

-- Função principal para criar e jogar o Campo Minado
playMinesweeper :: Int -> Int -> Int -> IO ()
playMinesweeper rows cols numMines = do
  board <- addMines numMines rows cols (emptyBoard rows cols)
  let finalBoard = updateHints board
  printBoard finalBoard

-- Exemplo de uso
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  putStrLn "Digite o número de linhas: "
  rows <- readLn
  putStrLn "Digite o número de colunas: "
  cols <- readLn
  putStrLn "Digite o número de minas: "
  numMines <- readLn
  playMinesweeper rows cols numMines
