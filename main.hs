-- pola: A,B,C,D,E,F,G
-- kropka (.) oznacza puste pole
-- opis lamiglowki: Plaster ["pola 1. wiersza", "pola 2. wiersza", ..., "pola n-tego wiersza"]
-- np. Plaster ["BD..", ".GA.D", ".FEG", "ABDCF", "E..."]
-- dlugosc kolejnych wierszy: (n-1), n, (n-1), ..., (n-1)
--
-- wymog glowny: zeby nie robic tego w sposob brutalny, tzn. generowac wszystkich mozliwych rozwiazan
--
-- obrane zalozenie: minimalna obslugiwana plansza -> 3-wierszowa

import ReadPuzzle

main = do
	putStrLn "Welcome to Honeycomb Puzzle!"
	putStrLn "Give the name of puzzle level to solve: "
	filename <- getLine
	puzzle <- readPuzzle filename
	putStrLn ("Puzzle: " ++ show puzzle)
	putStrLn ("Valid: " ++ show (checkPuzzle puzzle))

