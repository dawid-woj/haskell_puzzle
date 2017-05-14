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
import Data.List --sort, find, elemindex, elemIndices, \\ operator, concat, nub
import Data.Maybe --fromJust

type Pos = (Int,Int)

noCumb = 'X'
emptyCumb = '.'

data PuzzleState = PuzzleState {
puzzle :: Plaster,				-- plansza
emptyCumbs :: Int,				-- liczba pustych plastrow pozostalych do rozpatrzenia
status :: Bool					-- status zwracany przez kazdy krok algorytmu -> false: nieudalo sie / nawrot, true: udalo sie, idziemy dalej
} 

--PuzzleState -> czy plansza jest wypelniona w calosci?
isPuzzleFilled :: PuzzleState -> Bool
isPuzzleFilled p = (emptyCumbs p) == 0

--Plaster -> liczba pustych plastrow w planszy
countEmptyCumbs :: Plaster -> Int
countEmptyCumbs (Plaster []) = 0
countEmptyCumbs (Plaster (r:rs)) = length (elemIndices emptyCumb r) + countEmptyCumbs (Plaster rs)

-- Plaster pos -> czy sasiedzi plastra na pozycji 'pos' sa odpowiedni?
checkNeighbours :: Plaster -> Pos -> Bool
checkNeighbours p pos = not (isThereDuplicates (sort (getCumb p pos : getNearCumbs p pos)))
isThereDuplicates :: String -> Bool
isThereDuplicates [] = False
isThereDuplicates [x] = False
isThereDuplicates (x:y:ys)
	| x == y && x /= noCumb && x /= emptyCumb = True
	| otherwise = isThereDuplicates (y:ys)

-- Plaster pos -> lista wartosci plastrow sasiednich dla plastra na pozycji 'pos'
getNearCumbs :: Plaster -> Pos -> String
getNearCumbs (Plaster []) _ = []
getNearCumbs p (r,c)
	| r `mod` 2 /= 0 = [getCumb p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]
	| otherwise = [getCumb p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]

--checkNeighbours2 :: Plaster -> Pos -> Bool
--checkNeighbours2 p pos = not (isThereDuplicates (sort (getCumb p pos : getNearCumbs p pos)))

getNearCumbs2 :: Plaster -> Pos -> String
getNearCumbs2 (Plaster []) _ = []
getNearCumbs2 p (r,c)
	| r `mod` 2 /= 0 = (concat [getNearCumbs p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]) \\ (replicate 6 (getCumb p (r,c)))
	| otherwise = (concat [getNearCumbs p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]) \\ (replicate 6 (getCumb p (r,c)))

-- Plaster pos -> wartosc plastra na pozycji 'pos'
getCumb :: Plaster -> Pos -> Char
getCumb (Plaster rows) (r,c)
	| r < 0 || r >= length rows = noCumb
	| c < 0 || c >= length (rows !! r) = noCumb
	| otherwise = (rows !! r) !! c

--Plaster pos -> pozycja kolejnego pustego plastra zaczynajac poszukiwania od pozycji 'pos'
findNextEmptyCumb :: Plaster -> Pos -> Maybe Pos
findNextEmptyCumb (Plaster []) _ = Nothing
findNextEmptyCumb (Plaster rows) (r,c)
	| r >= len = Nothing
	| id == Nothing = findNextEmptyCumb (Plaster rows) (r',c')
	| otherwise = Just (r, fromJust id)
	where
		len = length rows
		r' = if c == length row - 1 then r+1 else r
		c' = if c == length row - 1 then 0 else c+1
		id = find (\x -> x > c) (elemIndices emptyCumb row)
		row = rows !! r

--replElem lista id v -> lista po zmianie, czyli zamiany elementu list na pozycji 'id' na wartosc 'v'
replElem :: [a] -> Int -> a -> [a]
replElem [] _ _ = []
replElem (x:xs) id v
	| id >= length (x:xs) = x:xs
	| id == 0 = v:xs
	| otherwise = x : replElem xs (id-1) v

--Plaster pos c -> plansza po zmianie, czyli wstawieniu 'c' w plaster na pozycji 'pos'
--(nie sprawdza poprawnosci 'pos' i tego co bylo w plastrze przed wstawieniem!)
fillCumb :: Plaster -> Pos -> Char -> Plaster
fillCumb (Plaster []) _ _ = (Plaster [])
fillCumb (Plaster rows) (r,c) v = (Plaster (replElem rows r row))
	where row = replElem (rows !! r) c v

--Plaster -> plansza po rozwiazaniu
solvePuzzle :: Plaster -> Plaster
solvePuzzle (Plaster []) = (Plaster [])
solvePuzzle p
	| ec == 0 = p
	| otherwise = puzzle (solve (PuzzleState p ec False) pos)
	where
		pos = findNextEmptyCumb p (0,-1)
		ec = countEmptyCumbs p

--PuzzleState pos -> stan po uzupelnieniu znalezionego pustego plastra na pozycji 'pos'
solve :: PuzzleState -> Maybe Pos -> PuzzleState
solve p Nothing
	| emptyCumbs p > 0 || not (checkSolution (puzzle p)) = (PuzzleState (puzzle p) (emptyCumbs p) False)
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) True)
solve p@(PuzzleState _ 0 _) pos = (PuzzleState (puzzle p) (emptyCumbs p) False)
solve p pos = tryOptions p pos options
	where
		nears = getNearCumbs (puzzle p) (fromJust pos)
--		nears = getNearCumbs2 (puzzle p) (fromJust pos)
		options = "ABCDEFG" \\ nears

tryOptions :: PuzzleState -> Maybe Pos -> String -> PuzzleState
tryOptions p _ [] = (PuzzleState (puzzle p) (emptyCumbs p) False)
tryOptions p Nothing _
	| emptyCumbs p > 0 || not (checkSolution (puzzle p)) = (PuzzleState (puzzle p) (emptyCumbs p) False)
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) True)
tryOptions p pos (o:os)
--	| elem o (getNearCumbs (puzzle p) (fromJust pos)) = try
	| not (status opt) = tryOptions p pos os
	| otherwise = opt
	where
		newPuzzle = fillCumb (puzzle p) (fromJust pos) o
		emptyCount = (emptyCumbs p) - 1
		newPos = findNextEmptyCumb (puzzle p) (fromJust pos)
		opt = solve (PuzzleState newPuzzle emptyCount False) newPos

--Plaster -> czy dana plansza jest poprawnym rozwiazaniem?
checkSolution :: Plaster -> Bool
checkSolution (Plaster rows) = checkAt (Plaster rows) (0,0)
checkAt :: Plaster -> Pos -> Bool
checkAt (Plaster rows) (r,c)
	| (getCumb (Plaster rows) (r,c)) == emptyCumb || not (checkNeighbours (Plaster rows) (r,c)) = False
	| (r == len-1) && (c == length (rows !! r) - 1) = True
	| otherwise = checkAt (Plaster rows) (r',c')
	where
		r' = if c == length (rows !! r) - 1 then r+1 else r
		c' = if c == length (rows !! r) - 1 then 0 else c+1
		len = length rows

--Plaster -> string do wizualnego wyswietlenia planszy
puzzlePrettyString :: Plaster -> String
puzzlePrettyString (Plaster rows) = puzzlePrettyString' (Plaster rows) (length rows)
puzzlePrettyString' :: Plaster -> Int -> String
puzzlePrettyString' (Plaster []) _ = []
puzzlePrettyString' (Plaster (r:rs)) n
	| length r /= n = " " ++ puzzlePrettyRow r ++ "\n" ++ puzzlePrettyString' (Plaster rs) n
	| otherwise = puzzlePrettyRow r ++ "\n" ++ puzzlePrettyString' (Plaster rs) n
puzzlePrettyRow :: String -> String
puzzlePrettyRow [] = []
puzzlePrettyRow (x:xs) = x : " " ++ puzzlePrettyRow xs


main = do
	putStrLn "Welcome to Honeycomb Puzzle!"
	putStrLn "Give the name of puzzle level to solve: "
	filename <- getLine
	puzzle <- readPuzzle filename
	--putStrLn ("Puzzle: " ++ show puzzle)
	if (checkPuzzle puzzle) then do
		putStrLn "Puzzle form and content are valid."
		putStrLn "Solving..."
		let solution = solvePuzzle puzzle
		putStrLn ("Valid solution: " ++ show (checkSolution solution))
		putStrLn "Puzzle:"
		putStr (puzzlePrettyString solution)
	else
		putStrLn "Invalid form/content of puzzle!"
