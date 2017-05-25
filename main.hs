-- pola: A,B,C,D,E,F,G
-- kropka (.) oznacza puste pole
-- opis lamiglowki: Plaster ["pola 1. wiersza", "pola 2. wiersza", ..., "pola n-tego wiersza"]
-- np. Plaster ["BD..", ".GA.D", ".FEG", "ABDCF", "E..."]
-- dlugosc kolejnych wierszy: (n-1), n, (n-1), ..., (n-1) => nieparzysta liczba wierszy planszy
--
-- obrane zalozenie: minimalna obslugiwana plansza -> 3-wierszowa

import ReadPuzzle

import Data.List --sort, find, elemindex, elemIndices, \\ operator, concat
import Data.Maybe --fromJust
import Debug.Trace --trace

--pozycja w planszy (wiersz, kolumna); indeksowanie od zera:
type Pos = (Int,Int)

_NO_CUMB = 'X'		-- "brak" plastra w planszy (oznaczenie granicy planszy)
_EMPTY_CUMB = '.'	-- pusty plaster planszy

--typ aktualnego stanu planszy (rozwiazania):
data PuzzleState = PuzzleState {
puzzle :: Plaster,				-- plansza
emptyCumbs :: Int,				-- liczba pustych plastrow pozostalych do rozpatrzenia
status :: Bool					-- status zwracany przez kazdy krok algorytmu -> false: nieudalo sie / nawrot, true: udalo sie, idziemy dalej
} deriving Show

--isPuzzleFilled (stan_planszy) -> true jesli plansza jest wypelniona
isPuzzleFilled :: PuzzleState -> Bool
isPuzzleFilled p = (emptyCumbs p) == 0

--countEmptyCumbs (plansza) -> liczba pustych plastrow w planszy
countEmptyCumbs :: Plaster -> Int
countEmptyCumbs (Plaster []) = 0
countEmptyCumbs (Plaster (r:rs)) = length (elemIndices _EMPTY_CUMB r) + countEmptyCumbs (Plaster rs)

--checkNeighbours (plansza) (pozycja) -> czy sasiedzi plastra na danej pozycji sa odpowiedni (maja rozne wartosci)?
checkNeighbours :: Plaster -> Pos -> Bool
checkNeighbours p pos = not (isThereDuplicates (sort (getCumb p pos : getNearCumbs p pos)))
isThereDuplicates :: String -> Bool
isThereDuplicates [] = False
isThereDuplicates [x] = False
isThereDuplicates (x:y:ys)
	| x == y && x /= _NO_CUMB && x /= _EMPTY_CUMB = True
	| otherwise = isThereDuplicates (y:ys)

--getNearCumbs (plansza) (pozycja) -> lista wartosci plastrow sasiednich dla plastra na danej pozycji
getNearCumbs :: Plaster -> Pos -> String
getNearCumbs (Plaster []) _ = []
getNearCumbs p (r,c)
	| r `mod` 2 /= 0 = [getCumb p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]
	| otherwise = [getCumb p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]

--getNearCumbs2 :: Plaster -> Pos -> String
--getNearCumbs2 (Plaster []) _ = []
--getNearCumbs2 p (r,c)
--	| r `mod` 2 /= 0 = (concat [getNearCumbs p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]) \\ (replicate 6 (getCumb p (r,c)))
--	| otherwise = (concat [getNearCumbs p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]) \\ (replicate 6 (getCumb p (r,c)))

--getCumb (plansza) (pozycja) -> wartosc plastra na danej pozycji
getCumb :: Plaster -> Pos -> Char
getCumb (Plaster rows) (r,c)
	| r < 0 || r >= length rows = _NO_CUMB
	| c < 0 || c >= length (rows !! r) = _NO_CUMB
	| otherwise = (rows !! r) !! c

--findNextEmptyCumb (plansza) (pozycja) -> pozycja kolejnego pustego plastra zaczynajac poszukiwania od danje pozycji lub Nothing, gdy nie ma
findNextEmptyCumb :: Plaster -> Pos -> Maybe Pos
findNextEmptyCumb (Plaster []) _ = Nothing
findNextEmptyCumb (Plaster rows) (r,c)
	| r >= len = Nothing
	| id == Nothing = findNextEmptyCumb (Plaster rows) (r+1,-1)
	| otherwise = Just (r, fromJust id)
	where
		len = length rows
		id = find (\x -> x > c) (elemIndices _EMPTY_CUMB row)
		row = rows !! r

--replElem (lista) (index_elementu_do_zamiany) (nowa_wartosc) -> lista po zmianie, czyli zamiany elementu listy pod danym indeksem na nowa wartosc
replElem :: [a] -> Int -> a -> [a]
replElem [] _ _ = []
replElem (x:xs) id v
	| id >= length (x:xs) = x:xs
	| id == 0 = v:xs
	| otherwise = x : replElem xs (id-1) v

--fillCumb (plansza) (pozycja) (wartosc) -> plansza po zmianie, czyli wstawieniu danej wartosci w plaster na danej pozycji
--(nie sprawdza poprawnosci pozycji i tego co bylo w plastrze przed wstawieniem!)
fillCumb :: Plaster -> Pos -> Char -> Plaster
fillCumb (Plaster []) _ _ = (Plaster [])
fillCumb (Plaster rows) (r,c) v = (Plaster (replElem rows r row))
	where row = replElem (rows !! r) c v

--solvePuzzle (plansza) -> plansza po rozwiazaniu
solvePuzzle :: Plaster -> Plaster
solvePuzzle (Plaster []) = (Plaster [])
solvePuzzle p
	| ec == 0 = p
	| otherwise = puzzle (solve (PuzzleState p ec False) (Just (0,-1)))
	where
		ec = countEmptyCumbs p		

--solve (stan_planszy) (ostatnio_analizowana_pozycja) -> stan planszy po dokonanej zmianie
solve :: PuzzleState -> Maybe Pos -> PuzzleState
solve p Nothing = p
solve p pos
--	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos) False = undefined
	| newPos /= Nothing = tryOptions p newPos options
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
	where
		newPos = findNextEmptyCumb (puzzle p) (fromJust pos)
		options = "ABCDEFG" \\ getNearCumbs (puzzle p) (fromJust newPos)

--tryOptions (stan_planszy) (pozycja_pustego_plastra) (lista_mozliwych_wartosci_do_wstawienia) -> stan planszy po dokonanej zmianie
tryOptions :: PuzzleState -> Maybe Pos -> String -> PuzzleState
tryOptions (PuzzleState p ec _) _ [] = (PuzzleState p ec False)
tryOptions p Nothing _ = p
tryOptions p pos (o:os)
--	| trace ("tryOptions: state = " ++ show p ++ " pos = " ++ show pos ++ " options = " ++ show (o:os)) False = undefined
	| not (status next) = tryOptions p pos os
	| not (checkSolution (puzzle next)) = tryOptions p pos os
	| otherwise = (PuzzleState (puzzle next) (emptyCumbs next) True)
	where
		newPuzzle = fillCumb (puzzle p) (fromJust pos) o
		emptyCount = (emptyCumbs p) - 1
		next = solve (PuzzleState newPuzzle emptyCount False) pos

--checkSolution (plansza) -> czy dana plansza jest poprawnym rozwiazaniem?
checkSolution :: Plaster -> Bool
checkSolution (Plaster rows) = checkAt (Plaster rows) (0,0)
checkAt :: Plaster -> Pos -> Bool
checkAt (Plaster rows) (r,c)
--	| trace ("checkAt: (r,c) = " ++ show (r,c)) False = undefined
	| (getCumb (Plaster rows) (r,c)) == _EMPTY_CUMB || not (checkNeighbours (Plaster rows) (r,c)) = False
	| (r == len-1) && (c == length (rows !! r) - 1) = True
	| otherwise = checkAt (Plaster rows) (r',c')
	where
		r' = if c == length (rows !! r) - 1 then r+1 else r
		c' = if c == length (rows !! r) - 1 then 0 else c+1
		len = length rows

--puzzlePrettyString (plansza) -> string do landego wyswietlenia planszy w terminalu
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
