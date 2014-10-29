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
    [2] ~=? extend (tableToDelta [(1, 'f', 2), (2, 'f', 2)]) 1 "ff"
    ]

allStringsTests = TestList [
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
    True ~=? accept a1 "aa"
    ]

languageTests = TestList [
    ["","aa"] ~=? take 2 (language a1)
    ]

a2 = Automaton [0,1] ['a'] [(0,'a',1)] 0 [0]

eq :: Automaton -> Automaton -> Bool
eq (Automaton s1 a1 ts1 i1 f1) (Automaton s2 a2 ts2 i2 f2) =
    s1 == s2 &&
    a1 == a2 &&
    ts1 == ts2 &&
    i1 == i2 &&
    f1 == f2

removeUselessTests = let a3 = removeUseless a2
    in
    TestList [
        True ~=? eq a3 (Automaton [0] ['a'] [] 0 [0])
        ]

isFiniteLanguageTests = TestList [
    True ~=? isFiniteLanguage a2
    ]


language'Tests = TestList [
    [""] ~=? language' a2
    ]

a3 = Automaton [0,1,2] ['a','b'] [(0,' ',2),(0,'a',1),(2,'b',0)] 0 [1]


epsilonClosureTests = TestList [
    [0,2] ~=? epsilonClosure a3 [0]
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
