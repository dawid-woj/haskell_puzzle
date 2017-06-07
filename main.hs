-- pola: A,B,C,D,E,F,G
-- kropka (.) oznacza puste pole
-- opis lamiglowki: Plaster ["pola 1. wiersza", "pola 2. wiersza", ..., "pola n-tego wiersza"]
-- np. Plaster ["BD..", ".GA.D", ".FEG", "ABDCF", "E..."]
-- dlugosc kolejnych wierszy: (n-1), n, (n-1), ..., (n-1) => nieparzysta liczba wierszy planszy
--
-- obrane zalozenie: minimalna obslugiwana plansza -> 3-wierszowa

import ReadPuzzle
import Utils
import Solver


main = do 
	putStrLn "Welcome to Honeycomb Puzzle!"
	putStrLn "Give the name of puzzle lvel to solve: "
	filename <- getLine
-- TODO odkomentowac stale dodawanie planszy	
--	puzzle <- readPuzzle "plansze/srednie9.txt"
	puzzle <- readPuzzle filename
	putStrLn ("Puzzle: " ++ show puzzle)
	if (checkPuzzle puzzle) then do
		putStrLn "Puzzle form and content are valid."
		putStrLn "Solving..."
		let solution = solvePuzzle puzzle
		putStrLn ("Valid solution: " ++ show (checkSolution solution))
		putStrLn "Puzzle:"
		putStr (puzzlePrettyString solution)
	else
		putStrLn "Invalid form/content of puzzle!"

