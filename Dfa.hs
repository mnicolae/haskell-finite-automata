{- Assignment 2 - Finite Automata (due November 11, noon)
David Eysman, c3eysman
Mihai Nicolae, g1mihai
-}
module Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language',
            epsilonClosure) where

import Data.List

-- Basic data types
type State = Integer
type Symbol = Char
type Transition = (State, Symbol, State)

-- Automaton Data Type
-- Automaton states alphabet transitions initial final
data Automaton = Automaton [State] [Symbol] [Transition] State [State]
-- Some helper functions for you to access the different automaton components
states :: Automaton -> [State]
states (Automaton s _ _ _ _) = s
alphabet :: Automaton -> [Symbol]
alphabet (Automaton _ a _ _ _) = a
transitions :: Automaton -> [Transition]
transitions (Automaton _ _ ts _ _) = ts
initial :: Automaton -> State
initial (Automaton _ _ _ i _) = i
final :: Automaton -> [State]
final (Automaton _ _ _ _ f) = f

-- Return the starting state for the given transition.
start :: Transition -> State
start (a,_,_) = a
-- Return the transition symbol for the given transition.
symbol :: Transition -> Symbol
symbol (_, b, _) = b
-- Return the ending state for the given transition.
end :: Transition -> State
end (_,_,c) = c

filterStart :: [Transition] -> State -> [Transition]
filterStart ts st =
	filter (\t -> if start t == st then True else False) ts 

filterSymbol :: [Transition] -> Symbol -> [Transition]
filterSymbol ts sym =
	filter (\t -> if symbol t == sym then True else False) ts 

filterEnd :: [Transition] -> State -> [Transition]
filterEnd ts st =
	filter (\t -> if end t == st then True else False) ts 

-- Return the ending states for a set of given transitions. 	
-- Duplicates are not removed.
getEndStates :: [Transition] -> [State]
getEndStates ts =
	map (\x -> end x) ts

-- Return the Epsilon transitions for a given set of transitions
getEpsTransitions :: [Transition] -> [Transition]
getEpsTransitions ts = filter (\t -> symbol t == ' ') ts

tupleString :: (String, [State]) -> String
tupleString (a,_) = a

-- Return a list of all strings of length n that can be made
-- from the symbols in the input alphabet.
combos :: [Char] -> Int -> [String]
combos chars 1 = map (:[]) chars
combos chars n = concatMap (\front -> map (front ++) (combos chars 1)) (combos chars (n - 1))

changeState :: Automaton -> [State] -> Automaton
changeState (Automaton _ b c d e) states = Automaton states b c d e

changeInitial :: Automaton -> State -> Automaton
changeInitial (Automaton a b c _ e) state = Automaton a b c state e

removeTrans :: Automaton -> [State] -> Automaton
removeTrans aut states = let ts = transitions aut
			     someUsefullTrans = filter (\t -> (elem (end t) states)) ts
			     usefullTrans = filter (\t -> (elem (start t) states)) someUsefullTrans
			 in changeTrans aut usefullTrans

changeTrans :: Automaton -> [Transition] -> Automaton
changeTrans (Automaton a b _ d e) ts = (Automaton a b ts d e)

-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta ts = \x y ->
        let filteredStarts = filterStart ts x
            filteredSymbols = filterSymbol filteredStarts y
            states = getEndStates filteredSymbols 
        in nub (sort states)

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend tf = \states string ->
	foldl' (\acc sym -> foldl' (++) [] (map (\x -> tf x sym) acc)) [states] string

allStrings :: [Symbol] -> [[String]]
allStrings str = 
	[[""]] ++ map (\x -> combos (sort str) x) [1..]

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes aut q = 
	let strings = allStrings (alphabet aut)
	    etf = extend (tableToDelta (transitions aut)) q
	in map (\x -> (((map (\y -> (y, etf y)) x)))) strings

-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept aut str = let states = extend (tableToDelta (transitions aut)) (initial aut) str
				 in (if null (intersect states (final aut))
						then False
						else True)

language :: Automaton -> [String]
language aut = filter (accept aut) (concat (allStrings (alphabet aut)))

-- Questions 7-9: finiteness
removeUseless :: Automaton -> Automaton
removeUseless aut = let strings = foldl' (++) [] (take (length (states aut)) (allStrings (alphabet aut)))
		    	useful = filter (\st -> elem True (map (accept (changeInitial aut st)) strings)) (states aut)
		    in removeTrans (changeState aut useful) useful

isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage aut =
				let useful = (removeUseless aut)
				    nplus = (length (states useful)) + 1
				    strings = (allStrings (alphabet aut)) !! nplus
				in not(elem True (map (accept useful) strings))

language' :: Automaton -> [String]
language' aut = let useful = removeUseless aut 
		    n = length (states useful) + 2
		in if isFiniteLanguage aut
		   then let states = take n (possibleOutcomes useful (initial useful))
			    strings = map (\l -> foldl' (\acc t -> acc ++ (tupleString t)) [] l) states
		         in filter (accept useful) strings
		   else language aut


-- Question 10: epsilon transitions
epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure = undefined
