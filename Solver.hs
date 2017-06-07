
module Solver (
solvePuzzle,
solve,
tryOptions
)  where

import ReadPuzzle
import Debug.Trace --trace
import Data.List --sort, find, elemindex, elemIndices, \\ operator, concat
import Data.Maybe --fromJust
import Debug.Trace --trace
import Data.Ord (comparing)
import Utils

--solvePuzzle (plansza) -> plansza po rozwiazaniu
solvePuzzle :: Plaster -> Plaster
solvePuzzle (Plaster []) = (Plaster [])
solvePuzzle p
--	| trace("solvePuzzle, emptyPositions: "++show emptyPositions) False = undefined
	| ec == 0 = p
	| otherwise = puzzle (solve (PuzzleState p ec False) (Just (0,-1)) emptyPositions)
	where
		ec = countEmptyCumbs p
		emptyPositions = reverse (sort (getEmptyPositions p (0, -1)))

--solve (stan_planszy) (ostatnio_analizowana_pozycja) -> stan planszy po dokonanej zmianie
solve :: PuzzleState -> Maybe Pos -> [EmptyPos] -> PuzzleState
solve p Nothing _ = p
solve p pos [] 
--	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++", options: []") False = undefined
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
solve p pos [(EmptyPos pos1 n)]
--	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++", options: " ++ show options) False = undefined
	| newPos /= Nothing = tryOptions p newPos options []
	| otherwise = (PuzzleState (puzzle p) (emptyCumbs p) (checkSolution (puzzle p)))
	where
		newPos = Just pos1
		options = "ABCDEFG" \\ getNearCumbs (puzzle p) (fromJust newPos)
	
solve p pos ((EmptyPos pos1 n):es)
--	| trace ("solve: state = " ++ show p ++ " pos = " ++ show pos ++ ", options: " ++ show options) False = undefined
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