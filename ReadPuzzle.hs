-- pola: A,B,C,D,E,F,G
-- kropka (.) oznacza puste pole
-- opis lamiglowki: Plaster ["pola 1. wiersza", "pola 2. wiersza", ..., "pola n-tego wiersza"]
-- np. Plaster ["BD..", ".GA.D", ".FEG", "ABDCF", "E..."]
-- dlugosc kolejnych wierszy: (n-1), n, (n-1), ..., (n-1)
--
-- obrane zalozenie: minimalna obslugiwana plansza -> 3-wierszowa

module ReadPuzzle
( Plaster
, readPuzzle
, checkPuzzle
) where

import System.IO

data Plaster = Plaster [String] deriving (Read, Show)
type Filename = String

readPuzzle :: Filename -> IO Plaster
readPuzzle [] = error "Empty file name!"
readPuzzle name = do
	content <- readFile name
	let puzzle = (read content) :: Plaster
	return puzzle

checkPuzzle' :: Int -> Int -> [String] -> Bool
checkPuzzle' _ _ [] = True
checkPuzzle' n s (x:xs)
	| n-s /= length x = False
	| otherwise = if s == 0 then checkPuzzle' n 1 xs else checkPuzzle' n 0 xs

checkPuzzle :: Plaster -> Bool
checkPuzzle (Plaster []) = False
checkPuzzle (Plaster lst)
	| n < 3 = False
	| otherwise = checkPuzzle' n 1 lst
	where n = length lst

