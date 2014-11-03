{- Assignment 2 - Finite Automata (due November 11, noon)

Notes:
- You may import Data.List; you may not import any other modules

***Write the names and CDF accounts for each of your group members below.***
<Name>, <CDF>
<Name>, <CDF>
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

start :: Transition -> State
start (a,_,_) = a
symbol :: Transition -> Symbol
symbol (_, b, _) = b
end :: Transition -> State
end (_,_,c) = c

-- Questions 1-4: transitions
tableToDelta :: [Transition] -> State -> Symbol -> [State]
tableToDelta ts = \x y ->
        let     starts = filter (\z -> if start z == x then True else False) ts
                ends = filter (\w -> if symbol w == y then True else False) starts
                states = map (\q -> end q) ends
        in nub (sort states)

extend :: (State -> Symbol -> [State]) -> (State -> String -> [State])
extend tf = \x y ->
        foldl' (next) [x] y

allStrings :: [Symbol] -> [[String]]
allStrings = undefined

possibleOutcomes :: Automaton -> State -> [[(String, [State])]]
possibleOutcomes = undefined


-- Questions 5-6: acceptance
accept :: Automaton -> String -> Bool
accept = undefined

language :: Automaton -> [String]
language = undefined


-- Questions 7-9: finiteness
removeUseless :: Automaton -> Automaton
removeUseless = undefined

isFiniteLanguage :: Automaton -> Bool
isFiniteLanguage = undefined

language' :: Automaton -> [String]
language' = undefined


-- Question 10: epsilon transitions
epsilonClosure :: Automaton -> [State] -> [State]
epsilonClosure = undefined
