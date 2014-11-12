{- Sample tests for Assignment 2 -}
import Test.HUnit
import Dfa (State, Symbol, Transition, Automaton(..),
            allStrings, tableToDelta, extend, possibleOutcomes,
            accept, language,
            removeUseless, isFiniteLanguage, language', epsilonClosure)


tableToDeltaTests = TestList [
    [2] ~=? tableToDelta [(1, 'f', 2)] 1 'f',
    -- Note: a symbol could be passed in that doesn't appear in any transition
    [] ~=? tableToDelta [(1, 'f', 2)] 1 'b'
    ]

extendTests = TestList [
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff",

    [2,3] ~=? extend (tableToDelta [(1, 'f', 2),
					(1, 'f', 3),
					(2, 'f', 2),
					(3, 'f', 3)]) 1 "ff"
    ]

allStringsTests = TestList [
    [""] ~=? allStrings "ab" !! 0,
    ["a", "b"] ~=? allStrings "ab" !! 1,
    ["aa", "ab", "ba", "bb"] ~=? allStrings "ab" !! 2
    ]

possibleOutcomesTests = TestList [
    [("aa",[1]), ("ab",[0,2]), ("ba",[0,2]), ("bb",[1])] ~=?
        (possibleOutcomes (Automaton [0,1,2]
                                     ['a','b']
                                     [(0,'a',1),
                                      (1,'a',2),
                                      (0,'b',0),
                                      (1,'b',1),
                                      (2,'b',2),
                                      (1,'a',0)] 0 [2]) 1) !! 2
    ]

a1 = Automaton [0,1] ['a'] [(0,'a',1),(1,'a',0)] 0 [0]

acceptTests = TestList [
    True ~=? accept a1 "",
    False ~=? accept a1 "a",
    False ~=? accept a1 "b",
    True ~=? accept a1 "aa",
    True ~=? accept a1 "aaaa"
    ]

f = Automaton [0,1] ['a'] [(0,'a',1)] 0 [1]
f2 = Automaton [0,1,2] ['a','b'] [(0,'a',2), (0,'a',1), (1,'a',2)] 0 [2]
f3 = Automaton [0,1] ['a'] [(0,'a',0)] 0 [0]
f4 = Automaton [0,1,2] ['a'] [(0,'a',1)] 0 [2]

languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1),
    ["a"] ~=? take 1 (language f),
    ["a"] ~=? take 1 (language f2),
    [""] ~=? take 1 (language f3)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

a5 = Automaton [0,1,2,3] ['a','b'] [(0, 'a', 1),
				    (0, 'b', 3),
				    (1, 'b', 2),
				    (1, 'a', 3),
				    (2, 'a', 2)] 0 [3]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = let a3 = removeUseless a2
			 a6 = removeUseless a5
    in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0]),
	True ~=? eq a6 (Automaton [0,1,3] ['a', 'b'] [(0, 'a', 1),
							(0, 'b', 3),
				     			(1, 'a', 3)] 0 [3])
        ]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2,
    True ~=? isFiniteLanguage f2,
    False ~=? isFiniteLanguage f3,
    True ~=? isFiniteLanguage f4
    ]

language'Tests = TestList [
    [""] ~=? language' a2,
    ["a"] ~=? language' f,
    [""] ~=? take 1 (language' f3),
    [] ~=? language' f4
    ]

a3 = Automaton [0,1,2,3,4,5] ['a','b'] [(0, ' ', 1),
					(0, ' ', 4),
					(1, 'a', 2),
					(2, ' ', 3),
					(2, 'a', 3),
					(4, 'b', 1),
					(5, ' ', 5)] 0 [3,5]

epsilonClosureTests = TestList [
    [] ~=? epsilonClosure a3 [],
    [1] ~=? epsilonClosure a3 [1],
    [5] ~=? epsilonClosure a3 [5],
    [0,1,4] ~=? epsilonClosure a3 [0],
    [2,3,4] ~=? epsilonClosure a3 [2,4]
    ]

main :: IO ()
main = do
    -- Put each call to "runTestTT" on a separate line
    runTestTT tableToDeltaTests
    runTestTT extendTests
    runTestTT allStringsTests
    runTestTT possibleOutcomesTests
    runTestTT acceptTests
    runTestTT languageTests
    runTestTT removeUselessTests
    runTestTT isFiniteLanguageTests
    runTestTT language'Tests
    runTestTT epsilonClosureTests
    return ()
