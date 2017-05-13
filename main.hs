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
import Data.List --sort
--import Data.Maybe --fromJust

type Pos = (Int,Int)

noCumb = 'X'
emptyCumb = '.'

data PuzzleState = PuzzleState {
lastEmptyCumb :: Pos,	-- pozycja = (wiersz,kolumna) ostatniego rozpatrywanego pustego plastra
before :: Plaster,		-- stan planszy przed wstawieniem litery do ostatnio rozpatrywanego pustego plastra
after :: Plaster,		-- stan planszy po -||-
emptyCumbs :: Int		-- liczba pustych plastrow pozostalych do rozpatrzenia
}

isPuzzleFilled :: PuzzleState -> Bool
isPuzzleFilled p = (emptyCumbs p) == 0

--createPuzzleState :: Plaster -> PuzzleState
--TODO

--findNextEmptyCumb :: Plaster -> Pos
--TODO

-- Plaster pos -> is neighourhood of cumb at 'pos' valid?
checkNeighbours :: Plaster -> Pos -> Bool
checkNeighbours p pos = not (isThereDuplicates (sort (getCumb p pos : getNearCumbs p pos)))
isThereDuplicates :: String -> Bool
isThereDuplicates [] = False
isThereDuplicates [x] = False
isThereDuplicates (x:y:ys)
	| x == y && x /= noCumb && x /= emptyCumb = True
	| otherwise = isThereDuplicates (y:ys)

-- Plaster pos -> list of cumbs' values in neighourhood of cumb at 'pos'
getNearCumbs :: Plaster -> Pos -> String
getNearCumbs p (r,c)
	| r `mod` 2 /= 0 = [getCumb p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]
	| otherwise = [getCumb p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]

-- Plaster pos -> cumb value
getCumb :: Plaster -> Pos -> Char
getCumb (Plaster rows) (r,c)
	| r < 0 || r >= length rows = noCumb
	| c < 0 || c >= length (rows !! r) = noCumb
	| otherwise = (rows !! r) !! c
--getCumb :: Plaster -> Int -> Int -> Maybe Char
--getCumb (Plaster rows) r c
--	| r < 0 || r >= length rows = Nothing
--	| c < 0 || c >= length (rows !! r) = Nothing
--	| otherwise = Just ((rows !! r) !! c)

--solve :: PuzzleState -> (...) -> PuzzleState/Plaster
--TODO

checkSolution :: Plaster -> Bool
checkSolution (Plaster rows) = checkAt (Plaster rows) (0,0)
checkAt :: Plaster -> Pos -> Bool
checkAt (Plaster rows) (r,c)
	| (getCumb (Plaster rows) (r,c)) == '.' || not (checkNeighbours (Plaster rows) (r,c)) = False
	| (r == len-1) && (c == length (rows !! r) - 1) = True
	| otherwise = checkAt (Plaster rows) (r',c')
	where
		r' = if c == length (rows !! r) - 1 then r+1 else r
		c' = if c == length (rows !! r) - 1 then 0 else c+1
		len = length rows

--puzzleAsPrettyString :: Plaster -> String
--TODO


main = do
	putStrLn "Welcome to Honeycomb Puzzle!"
	putStrLn "Give the name of puzzle level to solve: "
	filename <- getLine
	puzzle <- readPuzzle filename
	putStrLn ("Puzzle: " ++ show puzzle)
	if (checkPuzzle puzzle) then
		putStrLn ("Solution: " ++ show (checkSolution puzzle))
		--TODO
	else
		putStrLn "Invalid form/content of puzzle!"

