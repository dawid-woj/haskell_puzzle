-- pola: A,B,C,D,E,F,G
-- kropka (.) oznacza puste pole
-- opis lamiglowki: Plaster ["pola 1. wiersza", "pola 2. wiersza", ..., "pola n-tego wiersza"]
-- np. Plaster ["BD..", ".GA.D", ".FEG", "ABDCF", "E..."]
-- dlugosc kolejnych wierszy: (n-1), n, (n-1), ..., (n-1)
--
-- obrane zalozenie: minimalna obslugiwana plansza -> 3-wierszowa

module ReadPuzzle
( Plaster(Plaster)
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
checkPuzzle' n s (r:rs)
	| n-s /= length r = False
	| not (all (\a -> any (==a) ".ABCDEFG") r) = False
	| otherwise = if s == 0 then checkPuzzle' n 1 rs else checkPuzzle' n 0 rs

checkPuzzle :: Plaster -> Bool
checkPuzzle (Plaster []) = False
checkPuzzle (Plaster rows)
	| n < 3 = False
	| n `mod` 2 == 0 = False
	| otherwise = checkPuzzle' n 1 rows
	where n = length rows

