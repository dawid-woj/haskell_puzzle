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
import Data.List (sortBy)
import Data.Ord (comparing)

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

--checkCloseNeighbours (plansza) (pozycja) -> czy sasiedzi plastra na danej pozycji sa odpowiedni (maja rozne wartosci)?
checkCloseNeighbours :: Plaster -> Pos -> Bool
checkCloseNeighbours p pos = not (isThereDuplicates (sort (getCumb p pos : getCloseNearCumbs p pos)))
isThereDuplicates :: String -> Bool
isThereDuplicates [] = False
isThereDuplicates [x] = False
isThereDuplicates (x:y:ys)
	| x == y && x /= _NO_CUMB && x /= _EMPTY_CUMB = True
	| otherwise = isThereDuplicates (y:ys)

--getNearCumbs (plansza) (pozycja) -> lista wartosci plastrow sąsiadów i sąsiadów sąsiadów dla plastra na danej pozycji
getNearCumbs :: Plaster -> Pos -> String
getNearCumbs (Plaster []) _ = []
getNearCumbs p (r,c)
	| r `mod` 2 == 0 =  [getCumb p (x,y) | (x,y) <- [(r-2,c-1), (r-2,c), (r-2,c+1),
																	   (r-1,c-1), (r-1,c),(r-1,c+1), (r-1,c+2), 
																	(r,c-2), (r,c-1), (r,c), (r,c+1), (r,c+2), 
																	   (r+1,c-1), (r+1,c),(r+1,c+1), (r+1,c+2),
																	        (r+2,c-1), (r+2,c),(r+2,c+1)
																	 ]]
	| otherwise = [getCumb p (x,y) | (x,y) <- [       (r-2,c-1), (r-2,c), (r-2,c+1),
																	   (r-1,c-2), (r-1,c-1),(r-1,c), (r-1,c+1), 
																	(r,c-2), (r,c-1), (r,c), (r,c+1), (r,c+2), 
																	   (r+1,c-2), (r+1,c-1),(r+1,c), (r+1,c+1),
																	        (r+2,c-1), (r+2,c),(r+2,c+1)
																	 ]]

--getCloseNearCumbs (plansza) (pozycja) -> lista wartosci plastrow sasiednich dla plastra na danej pozycji
getCloseNearCumbs :: Plaster -> Pos -> String
getCloseNearCumbs (Plaster []) _ = []
getCloseNearCumbs p (r,c)
	| r `mod` 2 /= 0 = [getCumb p (x,y) | (x,y) <- [(r-1,c-1), (r-1,c), (r,c-1), (r,c+1), (r+1,c-1), (r+1,c)]]
	| otherwise = [getCumb p (x,y) | (x,y) <- [(r-1,c), (r-1,c+1), (r,c-1), (r,c+1), (r+1,c), (r+1,c+1)]]

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
	| trace("solvePuzzle, emptyPositions: "++show emptyPositions) False = undefined
	| ec == 0 = p
	| otherwise = puzzle (solve (PuzzleState p ec False) (Just (0,-1)) emptyPositions)
	where
		ec = countEmptyCumbs p
		emptyPositions = reverse (sort (getEmptyPositions p (0, -1)))

--solve (stan_planszy) (ostatnio_analizowana_pozycja) -> stan planszy po dokonanej zmianie
solve :: PuzzleState -> Maybe Pos -> [EmptyPos] -> PuzzleState
solve p Nothing _ = p
solve p pos [] 
	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++", options: []") False = undefined
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
solve p pos [(EmptyPos pos1 n)]
	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++", options: " ++ show options) False = undefined
	| newPos /= Nothing = tryOptions p newPos options []
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
	where
		newPos = Just pos1
		options = "ABCDEFG" \\ getNearCumbs (puzzle p) (fromJust newPos)
	
solve p pos ((EmptyPos pos1 n):es)
	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++ ", options: " ++ show options) False = undefined
	| newPos /= Nothing = tryOptions p newPos options es
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
	where
		newPos = Just pos1
		options = "ABCDEFG" \\ getNearCumbs (puzzle p) (fromJust newPos)

--tryOptions (stan_planszy) (pozycja_pustego_plastra) (lista_mozliwych_wartosci_do_wstawienia) -> stan planszy po dokonanej zmianie
tryOptions :: PuzzleState -> Maybe Pos -> String -> [EmptyPos] -> PuzzleState
tryOptions (PuzzleState p ec _) _ [] _ = (PuzzleState p ec False)
tryOptions p Nothing _ _ = p
tryOptions p pos (o:os) ep
--	| trace ("tryOptions: state = " ++ show p ++ " pos = " ++ show pos ++ " options = " ++ show (o:os)) False = undefined
	| not (status next) = tryOptions p pos os ep
	| not (checkSolution (puzzle next)) = tryOptions p pos os ep
	| otherwise = (PuzzleState (puzzle next) (emptyCumbs next) True)
	where
		newPuzzle = fillCumb (puzzle p) (fromJust pos) o
		emptyCount = (emptyCumbs p) - 1
		next = solve (PuzzleState newPuzzle emptyCount False) pos ep

--checkSolution (plansza) -> czy dana plansza jest poprawnym rozwiazaniem?
checkSolution :: Plaster -> Bool
checkSolution (Plaster rows) = checkAt (Plaster rows) (0,0)
checkAt :: Plaster -> Pos -> Bool
checkAt (Plaster rows) (r,c)
--	| trace ("checkAt: (r,c) = " ++ show (r,c)) False = undefined
	| (getCumb (Plaster rows) (r,c)) == _EMPTY_CUMB || not (checkCloseNeighbours (Plaster rows) (r,c)) = False
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
	putStrLn "Give the name of puzzle lvel to solve: "
--	filename <- getLine
-- TODO odkomentowac stale dodawanie planszy	
	puzzle <- readPuzzle "plansze/srednie9.txt"
	--puzzle <- readPuzzle filename
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



data EmptyPos = EmptyPos {
pos :: Pos,
neighboursCount :: Int
} 

instance Eq EmptyPos where
    (EmptyPos _ c1) == (EmptyPos _ c2) = c1 == c2
  
instance Ord EmptyPos where 
    (EmptyPos _ c1) `compare` (EmptyPos _ c2) = c1 `compare` c2
	
instance Show EmptyPos where
	show (EmptyPos (r,c) n) = "{("++show r++", "++show c++"), "++show n++"}"

getEmptyPositions :: Plaster -> Pos -> [EmptyPos]
getEmptyPositions (Plaster rows) (r, c) 
    | (r > len-1)  = []
	| trace ("getEmptyPositions, row:"++ show r ++ ", column: " ++ show c ++ ", cumb: "++ show cumb ++ ", c: '"++show c'++ ", r': "++show r') False = undefined
    | cumb == _EMPTY_CUMB = (EmptyPos (r,c) neighboursCount) : getEmptyPositions (Plaster rows) (r',c')
	| otherwise =  getEmptyPositions (Plaster rows) (r',c')
    where
		r' = if c == length (rows !! r) - 1 then r+1 else r
		c' = if c == length (rows !! r) - 1 then 0 else c+1
		cumb = (getCumb (Plaster rows) (r, c))
		len = length rows
		neighboursCount = length ([n | n <- getCloseNearCumbs (Plaster rows) (r,c), n /= _EMPTY_CUMB && n /= _NO_CUMB])














































