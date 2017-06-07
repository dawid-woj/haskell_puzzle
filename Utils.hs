
module Utils (
Pos,
_EMPTY_CUMB,
_NO_CUMB,
PuzzleState(..),
EmptyPos(EmptyPos),
puzzlePrettyString,
puzzlePrettyRow,
replElem,
checkSolution,
isPuzzleFilled,
countEmptyCumbs,
checkCloseNeighbours,
isThereDuplicates,
getNearCumbs,
getCloseNearCumbs,
getCumb,
findNextEmptyCumb,
fillCumb,
getEmptyPositions
) where


import ReadPuzzle
import Debug.Trace --trace
import Data.Ord (comparing)
import Data.List
import Data.Maybe

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

-- Reprezentuje puste pole (pozycja, liczba wypełnionych sąsiadów)
data EmptyPos = EmptyPos {
pos :: Pos,
neighboursCount :: Int
} 
-- Do zastosowania Data.List.sort
instance Eq EmptyPos where
    (EmptyPos _ c1) == (EmptyPos _ c2) = c1 == c2
  
instance Ord EmptyPos where 
    (EmptyPos _ c1) `compare` (EmptyPos _ c2) = c1 `compare` c2
	
instance Show EmptyPos where
	show (EmptyPos (r,c) n) = "{("++show r++", "++show c++"), "++show n++"}"

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

--replElem (lista) (index_elementu_do_zamiany) (nowa_wartosc) -> lista po zmianie, czyli zamiany elementu listy pod danym indeksem na nowa wartosc
replElem :: [a] -> Int -> a -> [a]
replElem [] _ _ = []
replElem (x:xs) id v
	| id >= length (x:xs) = x:xs
	| id == 0 = v:xs
	| otherwise = x : replElem xs (id-1) v
	
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


--fillCumb (plansza) (pozycja) (wartosc) -> plansza po zmianie, czyli wstawieniu danej wartosci w plaster na danej pozycji
--(nie sprawdza poprawnosci pozycji i tego co bylo w plastrze przed wstawieniem!)
fillCumb :: Plaster -> Pos -> Char -> Plaster
fillCumb (Plaster []) _ _ = (Plaster [])
fillCumb (Plaster rows) (r,c) v = (Plaster (replElem rows r row))
	where row = replElem (rows !! r) c v
	
-- getEmptyPositions (plansza) (pozycja) -> lista elementow EmptyPos, oznaczajacych pozycje pustego elementu i liczbe jego niepustych sasiadow
getEmptyPositions :: Plaster -> Pos -> [EmptyPos]
getEmptyPositions (Plaster rows) (r, c) 
    | (r > len-1)  = []
--	| trace ("getEmptyPositions, row:"++ show r ++ ", column: " ++ show c ++ ", cumb: "++ show cumb ++ ", c: '"++show c'++ ", r': "++show r') False = undefined
    | cumb == _EMPTY_CUMB = (EmptyPos (r,c) neighboursCount) : getEmptyPositions (Plaster rows) (r',c')
	| otherwise =  getEmptyPositions (Plaster rows) (r',c')
    where
		r' = if c == length (rows !! r) - 1 then r+1 else r
		c' = if c == length (rows !! r) - 1 then 0 else c+1
		cumb = (getCumb (Plaster rows) (r, c))
		len = length rows
		neighboursCount = length ([n | n <- getCloseNearCumbs (Plaster rows) (r,c), n /= _EMPTY_CUMB && n /= _NO_CUMB])


