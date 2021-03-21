module Regex (
  Regex,
  regexCompile, -- :: String -> Regex
  regexMatch,   -- :: Regex -> String -> Bool

  -- Extras:
  regexFind,      -- :: Regex -> String -> Maybe Int
  regexExtract,   -- :: Regex -> String -> Maybe String
  regexReplace,   -- :: Regex -> String -> String -> String
  regexReplaceAll -- :: Regex -> String -> String -> String
) where

import Data.List ((\\), nub, inits) -- !!!!! WARNING !!!!! : DO NOT import Data.List.groupBy (see below) !!!!!

-- -- -- -- -- https://hackage.haskell.org/package/groupBy -- -- -- -- --
-- The original Data.List.groupBy has (perhaps unexpected) behaviour, in that it compares elements to the first in the group, not adjacent ones.
-- -- -- https://hackage.haskell.org/package/groupBy-0.1.0.0/docs/Data-List-GroupBy.html
-- This module provides an alternative definition for groupBy which does not require a transitive equivalence predicate.
-- -- -- http://zvon.org/other/haskell/Outputlist/groupBy_f.html
-- similar to group, but it allows the programmers to supply their own equality test
--
groupBy :: (a -> a -> Bool) -> [a] -> [[a]] -- (the signature is the same as Data.List.groupBy)
groupBy _ [] = []
groupBy p' (x':xs') = (x' : ys') : zs'
  where
    (ys',zs') = go p' x' xs'
    go p z (x:xs)
      | p z x = (x : ys, zs)
      | otherwise = ([], (x : ys) : zs)
      where (ys,zs) = go p x xs
    go _ _ [] = ([], [])
-- The above code is stolen from https://hackage.haskell.org/package/groupBy



-- ===== ===== ===== ===== ===== <Exported Functions of this Module>  ===== ===== ===== ===== =====
regexCompile :: String -> Regex
regexCompile ""  = Regex (Automaton {finalStates=[0], transitions=[]}) -- The Automaton that accepts only the empty String/Word. (Q=QF={q0} and no transitions)
regexCompile str = Regex (removeEpsilonTransitions $ (foldr1 appendAutomata) $ (map buildAutomaton) $ splitRegexString' str) -- (new: removeEpsilonTransitions)

regexMatch   :: Regex -> String -> Bool
regexMatch (Regex aut) str = aut `accepts` str

regexFind    :: Regex -> String -> Maybe Int
regexFind reg@(Regex aut) ""     = if (aut `accepts` "")                then Just 0 else Nothing
regexFind reg@(Regex aut) (x:xs) = if (aut `acceptsBeginningOf` (x:xs)) then Just 0
                                   else (+1) <$> (regexFind reg xs)

regexExtract :: Regex -> String -> Maybe String
regexExtract reg str = case (regexFind reg str) of
                         Nothing    -> Nothing
                         Just index -> Just $ (takeUntilMatch reg) $ (drop index) str
  where takeUntilMatch (Regex aut) str = head $ (dropWhile (not . accepts aut)) $ inits str  -- Attention!!: very inefficient approach !!!

regexReplace :: Regex -> String -> String -> String
regexReplace = error "ToDo: regexReplace"

regexReplaceAll :: Regex -> String -> String -> String
regexReplaceAll = error "ToDo: regexReplaceAll"

-- ===== ===== ===== ===== ===== </Exported Functions of this Module> ===== ===== ===== ===== =====



newtype Regex = Regex Automaton

data Automaton = Automaton {
  -- every state of an Automaton is identified with an Int
  -- the initial state is 0
  finalStates :: [Int],
  transitions :: [(Int,[Char],Int)] -- [Char] and not Char: because we allow to write multiple characters onto a single transition! (important for stuff like \w in a Regex)
} deriving (Show) -- (added on Jan 04 2021 for debugging)

--instance Show Automaton where -- added on Jan 04 2021 for debugging
--  show Automaton {finalStates=f,transitions=t} = "Automaton { finalStates = " ++ show f ++ " , transitons = " ++ (map showTrans t) ++ " }"
--    where showTrans (i,chars,j)
--            | (length chars <= 2) = ""
--            | otherwise           = ""

instance Show Regex where
  show (Regex aut) = "[Compiled Regex (" ++ show _states ++ " states, of them " ++ show _final_states ++ " final, " ++ show _transitions ++ " transitions)" ++ _graphic ++ " ]"
    where _states = length $ nub (finalStates aut ++ [x | (x,chars,y) <- transitions aut] ++ [y | (x,chars,y) <- transitions aut]) --number of states in the Automaton aut
          _final_states = length $ finalStates aut
          _transitions  = length $ transitions aut
          state_0_is_final       = 0 `elem` (finalStates aut)
          state_1_is_final       = 1 `elem` (finalStates aut)
          state_0_has_self_trans = not $ null [ (x,c,y) | (x,c,y) <- transitions aut , x==0 && y==0 ]
          state_1_has_self_trans = not $ null [ (x,c,y) | (x,c,y) <- transitions aut , x==1 && y==1 ]
          has_trans_from_0_to_1  = not $ null [ (x,c,y) | (x,c,y) <- transitions aut , x==0 && y==1 ]
          has_trans_from_1_to_0  = not $ null [ (x,c,y) | (x,c,y) <- transitions aut , x==1 && y==0 ]
          print_state_0 = if state_0_is_final then "[0]" else "(0)"
          print_state_1 = if state_1_is_final then "[1]" else "(1)"
          _graphic = "" {-
          _graphic
            | (_states == 2 && _transitions == 1) = case () of
              () | state_0_has_self_trans -> ": " ++ error "ToDo ..."
                 | state_1_has_self_trans -> ": " ++ error "ToDo ..."
                 | has_trans_from_0_to_1  -> ": " ++ error "ToDo ..."
                 | has_trans_from_1_to_0  -> ": " ++ error "ToDo ..."
                 | otherwise              -> error "Show Regex : 1 transition not found."
            | (_states == 2 && _transitions == 2) = ": ToDo "
            | (_states == 2 && _transitions == 3) = case () of
              () | not $ state_0_has_self_trans -> ": " ++ error "ToDo ..."
                 | not $ state_1_has_self_trans -> ": " ++ error "ToDo ..."
                 | not $ has_trans_from_0_to_1  -> ": " ++ error "ToDo ..."
                 | not $ has_trans_from_1_to_0  -> ": " ++ error "ToDo ..."
                 | otherwise                    -> error "Show Regex : 3 and 4 transitions found."
         -- | (_states == 2 && _transitions == 4) = ... -- would not give much more information than "2 states, 4 transitions" already does
            | otherwise = "" -- do not show a graphic for all other kinds of (more compilcated) automata
-}





-- For implementing 'appendAutomata' (and also 'plusAutomaton') our Automaton also needs to support epsilon/tau transitions!
-- However, all epsilon transitions will be removed before the final 'regexCompile' compilaton output! (therefore 'accepts' does NOT have to deal with epsilon transitions!)
_EPSILON_ = ['ε'] -- == ['\949']

accepts :: Automaton -> String -> Bool
accepts aut str = accepts' aut str 0 -- An Automaton accepts a String iff it accepts this String in its inital state (which is defined as State 0).
  where accepts' aut ""     state = state `elem` (finalStates aut) -- Base Case: the Automaton accepts the empty String in state 'state' iff 'state' is one of its final states.
        accepts' aut (x:xs) state = any (\st -> accepts' aut xs st) (followupStates aut state x) -- Does any of the followup states accepts the remaining word?!
          where followupStates aut state char = [y | (x,chars,y) <- transitions aut, x==state, char `elem` chars]
                -- = The states that can be reaches in the Automaton 'aut' from the state 'state' by consuming the character 'char' 

-- NEW function for regexFind (Jan 05 2021): returns True as soon as a final state is reached, even when the given String hasn't been fully read yet
acceptsBeginningOf :: Automaton -> String -> Bool
acceptsBeginningOf aut str = acceptsBeg' aut str 0
  where acceptsBeg' aut ""     state = state `elem` (finalStates aut) 
        acceptsBeg' aut (x:xs) state = state `elem` (finalStates aut) || any (\st -> acceptsBeg' aut xs st) (followupStates aut state x) 
          where followupStates aut state char = [y | (x,chars,y) <- transitions aut, x==state, char `elem` chars] -- (COPY-PASTED from 'accepts' above)



-- https://regexr.com/ Cheatsheet (without anchors & Groups/Lookarounds):
-- .	any character except newline
-- \w\d\s	word, digit, whitespace
-- \W\D\S	not word, digit, whitespace
-- [abc]	any of a, b, or c
-- [^abc]	not a, b, or c
-- [a-g]	character between a & g
-- 
-- \.\*\\	escaped special characters
-- \t\n\r	tab, linefeed, carriage return
--
-- a*a+a?	0 or more, 1 or more, 0 or 1
-- a{5}a{2,}	exactly five, two or more
-- a{1,3}	between one & three
-- a+?a{2,}?	match as few as possible
-- ab|cd	match ab or cd


_dot_characters_            = ['\0'..'\127'] \\ ['\n']                        -- ". Dot. Matches any character except line breaks"
_word_characters_           = ['_'] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'] -- "\w Word. Matches any word character (alphanumeric & underscore)."
_digit_characters_          = ['0'..'9']                                      -- "\d Digit. Matches any digit character (0-9)."
_whitespace_characters_     = [' ','\t','\n']                                 -- "\s Whitespace. Matches any whitespace character (spaces, tabs, linke breaks)."
_not_word_characters_       = ['\0'..'\127'] \\ _word_characters_             -- "\W Not word."
_not_digit_characters_      = ['\0'..'\127'] \\ _digit_characters_            -- "\D Not digit."
_not_whitespace_characters_ = ['\0'..'\127'] \\ _whitespace_characters_       -- "\S Not whitespace."




-- e.g. splitRegexString "\w[a-g]b*" == ["\w","[a-g]","b*",""]
splitRegexString :: String -> [String]

splitRegexString "" = [""]

splitRegexString [singleChar]
  | (singleChar `elem` ['*','?','+','\\']) = error ("regexCompile : splitRegexString : '" ++ [singleChar] ++ "' is dangling alone.")
  | otherwise                              = [[singleChar]]

splitRegexString [char1,char2]
  | (char1 == '\\')                                = [[char1,char2]]
  | (char1 == '*' || char1 == '+' || char1 == '?') = error ("regexCompile : splitRegexString : '" ++ [char1] ++ "' is an invalid beginning for a regex.")
  | (char2 == '*' || char2 == '+' || char2 == '?') = [[char1,char2]]   -- e.g. "a*" becomes ["a*"]
  | otherwise                                      = [[char1],[char2]] -- e.g. "ab" becomes ["a","b"]

splitRegexString str@(x:y:z:zs)
  | ('|' `trueElem` str && '(' `trueNotElem` str) = [str] --WRONG: concat $ (map splitRegexString) $ (splitBy2 '|' str) -- splitBy2 (unlike splitBy) recognizes escaped "\|"'s !!!
  | (x == '\\' && (z == '*' || z == '+' || z == '?')) = [[x,y,z]] ++ splitRegexString zs     -- "\y*", "\y+", "\y?"                     is a "Singleton"-Regex
  | (x == '\\' &&  z == '{')                          = [(takeWhile (/= '}') str) ++ ['}']] ++ (splitRegexString $ tail $ dropWhile (/= '}') str) -- (COPY-PASTED from below)
  | (x == '\\'             )                          = [[x,y]]   ++ splitRegexString (z:zs) -- "\y" is a "Singleton"-Regex when z is neither of '*','+','?','{'
  | (              y == '*')                          = [[x,y]]   ++ splitRegexString (z:zs) -- "x*" (for any character x other than \) is a "Singleton"-Regex
  | (              y == '+')                          = [[x,y]]   ++ splitRegexString (z:zs) -- "x+" (for any character x other than \) is a "Singleton"-Regex
  | (              y == '?')                          = [[x,y]]   ++ splitRegexString (z:zs) -- "x?" (for any character x other than \) is a "Singleton"-Regex
  | (              y == '{') = [(takeWhile (/= '}') str) ++ ['}']] ++ (splitRegexString $ tail $ dropWhile (/= '}') str) -- "x{...}" (for any character x) is a "Singleton"-Regex
  | (x == '['              ) = let (one,two) = takeUntilEitherIncluding str ["]","]*","]+","]?"] in
                                if (last' one == ']' && head' two == '{') then [one ++ takeWhile (/= '}') two ++ "}"] ++ splitRegexString (tail $ dropWhile (/= '}') two) --"]{...}"
                                else [one] ++ splitRegexString two
  | (x == '('              ) = let (one,two) = takeUntilEitherIncluding str [")",")*",")+",")?"] in
                                if (last' one == ')' && head' two == '{') then [one ++ takeWhile (/= '}') two ++ "}"] ++ splitRegexString (tail $ dropWhile (/= '}') two) --"){...}"
                                else [one] ++ splitRegexString two
  | otherwise                = [[x]]   ++ splitRegexString (y:z:zs) -- nothing special at all happening, the first character alone is a single (literal or .) "Singleton"-Regex
  -- BUGFIX: We have to write our own safe versions of head and tail because otherwise we get an "*** Exception: Prelude.head: empty list" on an input like "(aa|bb)"
  where head' xs = if (xs==[]) then '\0' else (head xs)
        last' xs = if (xs==[]) then '\0' else (last xs) -- ('\0' is just a dummy because we have to return something for an empty list and it's the most sensible choice)



-- A wrapper of splitRegexString that removes a trailing empty string from the result if present (unless the result is just [""]):
splitRegexString' :: String -> [String]
splitRegexString' str = removeTrailingEmptyString $ splitRegexString str
  where removeTrailingEmptyString []          = []
        removeTrailingEmptyString [x]         = [x] -- including: removeTrailingEmptyString [""] = [""]
        removeTrailingEmptyString splitString = if (last splitString == "") then (init splitString) else splitString -- remove the trailing ...,""] if it's there



-- Helper function 1 for splitRegexString:
-- e.g. takeUntilEitherIncluding "(foo)bar"  [")",")*"] == ("(foo)" ,"bar")
--      takeUntilEitherIncluding "(foo)*bar" [")",")*"] == ("(foo)*","bar")
--   (the string that's found first takes precedence over the others, when multiple search strings have the same prefix the longest one matching takes precedence)
-- Important current constraint: The searchStrings shall have a length of either 1 or 2 !!! (which is actually enough for our purposes)
takeUntilEitherIncluding :: (Eq a) => [a] -> [[a]] -> ([a],[a])
takeUntilEitherIncluding []  _ = ([] ,[])
takeUntilEitherIncluding [x] _ = ([x],[]) -- no matter what x is (whether it's in the search strings or not), this will always be the result because "INCLUDING" (is in the name)
takeUntilEitherIncluding (x:y:zs) searchStrings
  | ([x,y] `elem` searchStrings) = ([x,y] , zs)   -- (the longer match [x,y] takes precedence over the shorter one [x])
  | ([x]   `elem` searchStrings) = ([x]   , y:zs)
  | otherwise                    = (x:one , two)  -- (no match yet, proceed by one character)
                                   where (one,two) = takeUntilEitherIncluding (y:zs) searchStrings



-- Helper functions 2+3 for splitRegexString (and also buildAutomaton below):
-- e.g. '|' `trueElem` "a|b"   == True
--      '|' `trueElem` "a\\|b" == False
trueElem    :: Char -> [Char] -> Bool -- just like the elem from Prelude, only that it does not count when the character that's searched for occurs right after a backslash!!
trueNotElem :: Char -> [Char] -> Bool
trueNotElem chr str = not $ trueElem chr str
trueElem chr []       = False
trueElem chr [x]      = (x==chr) -- (in a single-character String, escaping is not yet possible)
trueElem chr (x:y:zs)
  | (x == '\\') = trueElem chr zs                   -- if y is escaped using x,     skip checking y (and also the backslash in x of course)
  | otherwise   = (x==chr) || (trueElem chr (y:zs)) -- if y is not escaped using x, check x and continue recursively on (y:zs)



-- can build an Automaton out of a "Singleton"-Regex (e.g. "a*" but not! "a*b+")
buildAutomaton :: String -> Automaton
buildAutomaton ""            = Automaton {finalStates=[0],   transitions=[]} -- The Automaton that accepts only the empty String/Word. (Q=QF={q0} and no transitions)
buildAutomaton "."           = Automaton {finalStates=[1],   transitions=[(0,_dot_characters_,1)]} -- '.' is the only character that has a special meaning standing ALONE
buildAutomaton [singleChar]  = Automaton {finalStates=[1],   transitions=[(0,[singleChar],1)]}
buildAutomaton ['\\',char]   = Automaton {finalStates=[1],   transitions=[(0,resolveCharacterAfterBackslash char,1)]} 
buildAutomaton ".*"          = Automaton {finalStates=[0],   transitions=[(0,_dot_characters_,0)]}                        -- * = 0 or more of this character
buildAutomaton ".+"          = Automaton {finalStates=[1],   transitions=[(0,_dot_characters_,1),(1,_dot_characters_,1)]} -- + = 1 or more of this character
buildAutomaton ".?"          = Automaton {finalStates=[0,1], transitions=[(0,_dot_characters_,1)]}                        -- ? = 0 or 1 of this character
buildAutomaton [char,'*']    = Automaton {finalStates=[0],   transitions=[(0,[char],0)]}              -- * = 0 or more of this character
buildAutomaton [char,'+']    = Automaton {finalStates=[1],   transitions=[(0,[char],1),(1,[char],1)]} -- + = 1 or more of this character
buildAutomaton [char,'?']    = Automaton {finalStates=[0,1], transitions=[(0,[char],1)]}              -- ? = 0 or 1 of this character
buildAutomaton [char1,char2] = error ("buildAutomaton : illegal two-character singleton regex \"" ++ [char1,char2] ++ "\"")
buildAutomaton ['\\',ch,'*'] = Automaton {finalStates=[0],   transitions=[(0,resolveCharacterAfterBackslash ch,0)]}              -- * = 0 or more of this character
buildAutomaton ['\\',ch,'+'] = Automaton {finalStates=[1],   transitions=[(0,resolveCharacterAfterBackslash ch,1),(1,resolveCharacterAfterBackslash ch,1)]} 
buildAutomaton ['\\',ch,'?'] = Automaton {finalStates=[0,1], transitions=[(0,resolveCharacterAfterBackslash ch,1)]}              -- ? = 0 or 1 of this character

buildAutomaton ['[',         a,'-',     b,']']     = Automaton {finalStates=[1],   transitions=[(0,[a..b],1)]} -- [a..b] is the direct Haskell translation of the [a-b] Regex!!
buildAutomaton ['[',         a,'-',     b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,[a..b],0)]}
buildAutomaton ['[',         a,'-',     b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,[a..b],1),(1,[a..b],1)]}
buildAutomaton ['[',         a,'-',     b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,[a..b],1)]}
buildAutomaton ['[',    '\\',a,'-',     b,']']     = Automaton {finalStates=[1],   transitions=[(0,[rcabs a..b],1)]} 
buildAutomaton ['[',    '\\',a,'-',     b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,[rcabs a..b],0)]}
buildAutomaton ['[',    '\\',a,'-',     b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,[rcabs a..b],1),(1,[rcabs a..b],1)]}
buildAutomaton ['[',    '\\',a,'-',     b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,[rcabs a..b],1)]}
buildAutomaton ['[',         a,'-','\\',b,']']     = Automaton {finalStates=[1],   transitions=[(0,[a..rcabs b],1)]} 
buildAutomaton ['[',         a,'-','\\',b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,[a..rcabs b],0)]}
buildAutomaton ['[',         a,'-','\\',b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,[a..rcabs b],1),(1,[a..rcabs b],1)]}
buildAutomaton ['[',         a,'-','\\',b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,[a..rcabs b],1)]}
buildAutomaton ['[',    '\\',a,'-','\\',b,']']     = Automaton {finalStates=[1],   transitions=[(0,[rcabs a..rcabs b],1)]} 
buildAutomaton ['[',    '\\',a,'-','\\',b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,[rcabs a..rcabs b],0)]}
buildAutomaton ['[',    '\\',a,'-','\\',b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,[rcabs a..rcabs b],1),(1,[rcabs a..rcabs b],1)]}
buildAutomaton ['[',    '\\',a,'-','\\',b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,[rcabs a..rcabs b],1)]}

buildAutomaton ['[','^',     a,'-',     b,']']     = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [a..b],1)]}
buildAutomaton ['[','^',     a,'-',     b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,['\0'..'\127'] \\ [a..b],0)]}
buildAutomaton ['[','^',     a,'-',     b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [a..b],1),(1,['\0'..'\127'] \\ [a..b],1)]}
buildAutomaton ['[','^',     a,'-',     b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,['\0'..'\127'] \\ [a..b],1)]}
buildAutomaton ['[','^','\\',a,'-',     b,']']     = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [rcabs a..b],1)]}
buildAutomaton ['[','^','\\',a,'-',     b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,['\0'..'\127'] \\ [rcabs a..b],0)]}
buildAutomaton ['[','^','\\',a,'-',     b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [rcabs a..b],1),(1,['\0'..'\127'] \\ [rcabs a..b],1)]} 
buildAutomaton ['[','^','\\',a,'-',     b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,['\0'..'\127'] \\ [rcabs a..b],1)]}
buildAutomaton ['[','^',     a,'-','\\',b,']']     = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [a..rcabs b],1)]}
buildAutomaton ['[','^',     a,'-','\\',b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,['\0'..'\127'] \\ [a..rcabs b],0)]}
buildAutomaton ['[','^',     a,'-','\\',b,']','+'] = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [a..rcabs b],1),(1,['\0'..'\127'] \\ [a..rcabs b],1)]} 
buildAutomaton ['[','^',     a,'-','\\',b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,['\0'..'\127'] \\ [a..rcabs b],1)]}
buildAutomaton ['[','^','\\',a,'-','\\',b,']']     = Automaton {finalStates=[1],   transitions=[(0,['\0'..'\127'] \\ [rcabs a..rcabs b],1)]}
buildAutomaton ['[','^','\\',a,'-','\\',b,']','*'] = Automaton {finalStates=[0],   transitions=[(0,['\0'..'\127'] \\ [rcabs a..rcabs b],0)]}
buildAutomaton ['[','^','\\',a,'-','\\',b,']','+'] = Automaton {finalStates=[1], transitions=[(0,['\0'..'\127'] \\ [rcabs a..rcabs b],1),(1,['\0'..'\127']\\[rcabs a..rcabs b],1)]} 
buildAutomaton ['[','^','\\',a,'-','\\',b,']','?'] = Automaton {finalStates=[0,1], transitions=[(0,['\0'..'\127'] \\ [rcabs a..rcabs b],1)]}

buildAutomaton str
  | (head   str == '('  && last  str == ')')  =                         buildAutomaton' (init $ tail str) -- just remove parenthesis around
  | (head   str == '('  && last2 str == ")*") = starAutomaton         $ buildAutomaton' (init $ init $ tail str) -- init $ init $ tail removes the first and the last 2 chars
  | (head   str == '('  && last2 str == ")+") = plusAutomaton         $ buildAutomaton' (init $ init $ tail str)
  | (head   str == '('  && last2 str == ")?") = questionmarkAutomaton $ buildAutomaton' (init $ init $ tail str)
  | (first2 str == "[^" && last  str == ']')  = Automaton {finalStates=[1], transitions=[(0,['\0'..'\127'] \\ (init $ tail $ tail str),1)]}
  | (first2 str == "[^" && last2 str == "]*") = starAutomaton         $ buildAutomaton $ init str 
  | (first2 str == "[^" && last2 str == "]+") = plusAutomaton         $ buildAutomaton $ init str
  | (first2 str == "[^" && last2 str == "]?") = questionmarkAutomaton $ buildAutomaton $ init str
  | (head   str == '['  && last  str == ']')  = Automaton {finalStates=[1], transitions=[(0,nub $ init $ tail str,1)]} -- nub just in case the regex is something like "[abba]"
  | (head   str == '['  && last2 str == "]*") = starAutomaton         $ buildAutomaton $ init str 
  | (head   str == '['  && last2 str == "]+") = plusAutomaton         $ buildAutomaton $ init str
  | (head   str == '['  && last2 str == "]?") = questionmarkAutomaton $ buildAutomaton $ init str
  | (second str == '{'  && last2 str == ",}")                         = 
                                       automaton_char_x_at_least_n_times (resolveCharacterAfterBackslash $ head str) (read $ whatsInBetween '{' ',' (tail str)) -- x{2,}
                                                                      --automaton_at_least_n_times (buildAutomaton [head str]) (read $ whatsInBetween '{' ',' (tail str)) -- x{2,}
                                                                      --automaton_char_x_at_least_n_times (head str) (read $ whatsInBetween '{' ',' (tail str)) -- x{2,}
  | (second str == '{'  && last  str == '}' && ',' `elem` (tail str)) = automaton_char_x_n_to_m_times (resolveCharacterAfterBackslash $ head str)
                                                                      --automaton_n_to_m_times (buildAutomaton [head str]) -- INEFFICIENT BUGFIX (Jan 04 2021)
                                                                      --automaton_char_x_n_to_m_times (head str) 
                                                                          (read $ whatsInBetween '{' ',' (tail str)) (read $ whatsInBetween ',' '}' (tail str)) -- x{1,3}
  | (second str == '{'  && last  str == '}')                          =
                                       automaton_char_x_exactly_n_times (resolveCharacterAfterBackslash $ head str) (read $ whatsInBetween '{' '}' (tail str)) -- x{5}
                                                                      --automaton_exactly_n_times (buildAutomaton [head str]) (read $ whatsInBetween '{' '}' (tail str)) -- x{5}
                                                                      --automaton_char_x_exactly_n_times (head str) (read $ whatsInBetween '{' '}' (tail str)) -- x{5}
  
  -- -- -- BUGFIX (Jan 04 2021): no match for singleton regex "\d{1,}" -- -- --
  -- Do the same thing as in the three cases above, the only difference is the extra character in front (the backslash) and the call of the 'rcabs' function.
  -- BUGFIX within the BUGFIX: using 'automaton_char_x_...' and 'rcabs' will work fine for "\t{1,}" but NOT for "\d{1,}"
  --                           -> solution: use 'resolveCharacterAfterBackslash' instead and update 'automaton_char_x_...' to accept multiple chars!!
  | (head   str == '\\' && third str == '{' && last2 str == ",}")                                =
                                                              --automaton_at_least_n_times (buildAutomaton ['\\', second str]) (read $ whatsInBetween '{' ',' (tail $ tail str))
                                               automaton_char_x_at_least_n_times (resolveCharacterAfterBackslash $ second str) (read $ whatsInBetween '{' ',' (tail $ tail str))
  | (head   str == '\\' && third str == '{' && last  str == '}' && ',' `elem` (tail $ tail str)) =
                                                                      --automaton_n_to_m_times (buildAutomaton ['\\', second str])
                                                                        automaton_char_x_n_to_m_times (resolveCharacterAfterBackslash $ second str) 
                                                                          (read $ whatsInBetween '{' ',' (tail $ tail str)) (read $ whatsInBetween ',' '}' (tail $ tail str)) 
  | (head   str == '\\' && third str == '{' && last  str == '}')                                 =
                                                              --automaton_exactly_n_times (buildAutomaton ['\\', second str]) (read $ whatsInBetween '{' '}' (tail $ tail str)) 
                                               automaton_char_x_exactly_n_times (resolveCharacterAfterBackslash $ second str) (read $ whatsInBetween '{' '}' (tail $ tail str)) 
  -- -- -- BUGFIX -- -- --

  -- (...){...}
  | (head   str == '('  && ')' `elem` str && '{' `elem` str && last2 str == ",}")
                       = automaton_at_least_n_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' ',' str)

  | (head   str == '('  && ')' `elem` str && '{' `elem` str && last  str == '}' && ',' `elem` (str `after` '{'))
                       = automaton_n_to_m_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' ',' str) (read $ whatsInBetween ',' '}' str)

  | (head   str == '('  && ')' `elem` str && '{' `elem` str && last  str == '}')
                       = automaton_exactly_n_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' '}' str)

  -- [...]{...}
  | (head   str == '['  && ']' `elem` str && '{' `elem` str && last2 str == ",}")
                       = automaton_at_least_n_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' ',' str) -- (COPY-PASTE)

  | (head   str == '['  && ']' `elem` str && '{' `elem` str && last  str == '}' && ',' `elem` (str `after` '{'))
                       = automaton_n_to_m_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' ',' str) (read $ whatsInBetween ',' '}' str) -- (COPY-PASTE)

  | (head   str == '['  && ']' `elem` str && '{' `elem` str && last  str == '}')
                       = automaton_exactly_n_times (buildAutomaton (str `before` '{')) (read $ whatsInBetween '{' '}' str) -- (COPY-PASTE)

  | ('|' `trueElem` str)                                              = (foldr1 orAutomata) $ (map buildAutomaton') $ (splitBy2 '|') str -- "| Alternation. Acts like a boolean OR."
  | otherwise                                                         = error ("buildAutomaton : no match for singleton regex \"" ++ str ++ "\"")
  where first2 (x:y:zs) = [x,y] -- (string with length at least 2)
        first2 xs       = xs    -- (string of length less than 2)
        last2  []     = []
        last2  [x]    = [x]
        last2  [x,y]  = [x,y]
        last2  (x:xs) = last2 xs
        second []         = '\0'
        second [_]        = '\0'
        second (x:y:zs)   = y
        third  []         = '\0'
        third  [_]        = '\0'
        third  [_,_]      = '\0'   
        third  (x:y:z:zs) = z
        buildAutomaton' str = removeEpsilonTransitions $ (foldr1 appendAutomata) $ (map buildAutomaton) $ splitRegexString' str -- !!! This is a NEW BUGFIX !!! (c.f. regexCompile)
          -- (if we had just called buildAutomaton above (instead of buildAutomaton') we could handle simple stuff like "a|b" but NOT "aa|bb")


-- Parsing helper functions for buildAutomaton:
--
whatsInBetween :: Char -> Char -> String -> String -- e.g. whatsInBetween '(' ')' "foo(bar)" == "bar"
whatsInBetween _ _ "" = ""
whatsInBetween char1 char2 str = (takeWhile (/= char2)) $ tail $ (dropWhile (/= char1)) $ str -- tail call is necessary because dropWhile result will begin with char1
--
after :: String -> Char -> String -- e.g. "foo{bar}" `after` '{' == "bar}"
after ""     _    = ""
after (x:xs) char = if (x==char) then xs else (xs `after` char)
--
before :: String -> Char -> String -- e.g. "foo{bar}" `before` '{' == "foo"
before str char = takeWhile (/= char) str

-- "...{...}" Helper functions for buildAutomaton:
--
automaton_char_x_exactly_n_times  :: [Char] -> Int        -> Automaton -- e.g. x{5}   -- BUGFIX (Jan 04 2021): replaced [Char] with Char (_dot_characters_ etc. now also supported)
automaton_char_x_n_to_m_times     :: [Char] -> Int -> Int -> Automaton -- e.g. x{1,3}
automaton_char_x_at_least_n_times :: [Char] -> Int        -> Automaton -- e.g. x{2,}
--
automaton_exactly_n_times  :: Automaton -> Int        -> Automaton   -- e.g. (a*b+){5}
automaton_n_to_m_times     :: Automaton -> Int -> Int -> Automaton   -- e.g. (a*b+){1,3}
automaton_at_least_n_times :: Automaton -> Int        -> Automaton   -- e.g. (a*b+){2,}
--
-- (0)--x-->(1)--x-->...--x-->[n]
automaton_char_x_exactly_n_times x n  = Automaton { finalStates = [n]    , transitions =           [ (s,x,s+1) | s <- [0..(n-1)] ] }
--
-- (0)--x-->(1)--x-->...--x-->[n]--x-->[n+1]--x-->...--x-->[m]
automaton_char_x_n_to_m_times x n m   = Automaton { finalStates = [n..m] , transitions =           [ (s,x,s+1) | s <- [0..(m-1)] ] }
--
-- (0)--x-->(1)--x-->...--x-->[n]<--|x
--                             |____|
automaton_char_x_at_least_n_times x n = Automaton { finalStates = [n]    , transitions = (n,x,n):[ (s,x,s+1) | s <- [0..(n-1)] ] }
--
--
automaton_exactly_n_times aut n
  | (n <= 0)  = buildAutomaton "" -- could also be (n == 0) as negative values are undefined behaviour anyways
  | otherwise = (foldr1 appendAutomata) $ (replicate n) aut
--
automaton_n_to_m_times aut n m
  | (n == m)  = automaton_exactly_n_times aut n
  | (n >  m)  = error "regexCompile : automaton_n_to_m_times : ERROR : Quantifier minimum is greater than maximum."
  | otherwise = (foldr1 orAutomata) [ automaton_exactly_n_times aut x | x <- [n..m] ] -- Attention!!!: this is probably a very naive, inefficient approach !!!
--
automaton_at_least_n_times aut n
  | (n <= 0)  = starAutomaton aut -- R{0,} is an equivalent Regex to R* (regexr.com: "Match 0 or more of the preceding token.")
  | (n == 1)  = plusAutomaton aut -- R{1,} is an equivalent Regex to R+ (regexr.com: "Match 1 or more of the preceding token.")
  | otherwise = appendAutomata (automaton_exactly_n_times aut n) (starAutomaton aut) -- R{n,} is an equivalent Regex to R{n}R{0,} === R{n}R* (see above)




-- Helper function 1 for buildAutomaton:
resolveCharacterAfterBackslash :: Char -> [Char]
resolveCharacterAfterBackslash 'w' = _word_characters_           -- "\w Word. Matches any word character (alphanumeric & underscore)."
resolveCharacterAfterBackslash 'd' = _digit_characters_          -- "\d Digit. Matches any digit character (0-9)."
resolveCharacterAfterBackslash 's' = _whitespace_characters_     -- "\s Whitespace. Matches any whitespace character (spaces, tabs, linke breaks)."
resolveCharacterAfterBackslash 'W' = _not_word_characters_       -- "\W Not word."
resolveCharacterAfterBackslash 'D' = _not_digit_characters_      -- "\D Not digit."
resolveCharacterAfterBackslash 'S' = _not_whitespace_characters_ -- "\S Not whitespace."
resolveCharacterAfterBackslash 'x' = ['\NUL'] -- "\x Escaped character. Matches a NULL character (char code 0)."
resolveCharacterAfterBackslash '0' = ['\0']   -- "\0 Escaped character. Matches a NULL character (char code 0)."
resolveCharacterAfterBackslash '1' = ['\1']   -- "\1 Escaped character. Matches a SOH character (char code 1)."
resolveCharacterAfterBackslash '2' = ['\2']   -- "\2 Escaped character. Matches a STX character (char code 2)."
resolveCharacterAfterBackslash '3' = ['\3']   -- "\3 Escaped character. Matches a ETX character (char code 3)."
resolveCharacterAfterBackslash '4' = ['\4']   -- "\4 Escaped character. Matches a EOT character (char code 4)."
resolveCharacterAfterBackslash '5' = ['\5']   -- "\5 Escaped character. Matches a ENQ character (char code 5)."
resolveCharacterAfterBackslash '6' = ['\6']   -- "\6 Escaped character. Matches a ACK character (char code 6)."
resolveCharacterAfterBackslash '7' = ['\7']   -- "\7 Escaped character. Matches a BELL character (char code 7)."
resolveCharacterAfterBackslash 't' = ['\t']   -- "\t Escaped character. Matches a TAB character (char code 9)."
resolveCharacterAfterBackslash 'n' = ['\n']   -- "\n Escaped character. Matches a LINE FEED character (char code 10)."
resolveCharacterAfterBackslash 'r' = ['\r']   -- "\r Escaped character. Matches a CARRIAGE RETURN character (char code 13)."
resolveCharacterAfterBackslash chr = [chr]    -- "\a" means the same thing as just "a"

-- Helper function 2 for buildAutomaton:
rcabs :: Char -> Char -- "rcabs" abbreviation for: "resolve chracter after backslash single" (single because it returns a single Char instead of [Char])
rcabs 'x' = '\NUL' -- "\x Escaped character. Matches a NULL character (char code 0)."
rcabs '0' = '\0'   -- "\0 Escaped character. Matches a NULL character (char code 0)."
rcabs '1' = '\1'   -- "\1 Escaped character. Matches a SOH character (char code 1)."
rcabs '2' = '\2'   -- "\2 Escaped character. Matches a STX character (char code 2)."
rcabs '3' = '\3'   -- "\3 Escaped character. Matches a ETX character (char code 3)."
rcabs '4' = '\4'   -- "\4 Escaped character. Matches a EOT character (char code 4)."
rcabs '5' = '\5'   -- "\5 Escaped character. Matches a ENQ character (char code 5)."
rcabs '6' = '\6'   -- "\6 Escaped character. Matches a ACK character (char code 6)."
rcabs '7' = '\7'   -- "\7 Escaped character. Matches a BELL character (char code 7)."
rcabs 't' = '\t'   -- "\t Escaped character. Matches a TAB character (char code 9)."
rcabs 'n' = '\n'   -- "\n Escaped character. Matches a LINE FEED character (char code 10)."
rcabs 'r' = '\r'   -- "\r Escaped character. Matches a CARRIAGE RETURN character (char code 13)."
rcabs chr = chr    -- "\a" means the same thing as just "a"
-- Attention!: This function also resolves "\w" to 'w'::Char (instead of _word_characters_::[Char]) !!!!! (okay because "[x-\w]" is an invalid Regex in our eyes anyway)


-- Helper function 3 for buildAutomaton:
-- Copied from ArdensLemma.hs:
splitBy :: Char -> String -> [String]
splitBy char = (map tail) . (groupBy (\ x y -> y /= char)) . (char:) -- (groupBy does not remove the chars/commas, that's why we need to map tail at the end)
-- Note: groupBy: "Groups adjacent elements according to some relation."
--                groupBy (<=) [1,2,2,3,1,2,0,4,5,2] == [[1,2,2,3],[1,2],[0,4,5],[2]]
--                https://hackage.haskell.org/package/groupBy-0.1.0.0/docs/Data-List-GroupBy.html
-- Idea to use groupBy comes from: https://codereview.stackexchange.com/questions/6992/approach-to-string-split-by-character-in-haskell
--    groupBy (\a b -> b /= '/') "/hejsan/asdas" == ["/hejsan","/asdas"]



-- Helper function 4 for buildAutomaton (NOT splitRegexString anymore, that was WRONG):
-- Unlike splitBy, splitBy2 recognizes escaped characters ("\x") and does NOT split by those
splitBy2 :: Char -> String -> [String]
splitBy2 char = (map tail) . (groupBy (\ x y -> x == '\\' || y /= char)) . (char:) 



-- Takes an Automaton accepting the Regex R and gives back an Automaton accepting the Regex (R)*
-- --> just like plusAutomaton, only that we also make the initial state (number 0) final if it isn't already ( nub (0:f) )
starAutomaton :: Automaton -> Automaton
starAutomaton Automaton {finalStates=f,transitions=t} = Automaton {finalStates = nub (0:f), transitions = t++[(f',_EPSILON_,0) | f' <- f]}

-- Takes an Automaton accepting the Regex R and gives back an Automaton accepting the Regex (R)+
-- --> add an epsilon transition from all the final states back to the initial state (number 0)
plusAutomaton :: Automaton -> Automaton
plusAutomaton Automaton {finalStates=f,transitions=t} = Automaton {finalStates=f,transitions=t++[(f',_EPSILON_,0) | f' <- f]}

-- Takes an Automaton accepting the Regex R and gives back an Automaton accepting the Regex (R)?
-- --> makes the initial state final (if it isn't already), otherwise leaves the Automaton as it is
questionmarkAutomaton :: Automaton -> Automaton
questionmarkAutomaton Automaton {finalStates=f,transitions=t} = Automaton {finalStates = nub (0:f), transitions = t}



-- takes an Automaton accepting the Regex R and an Automaton accepting the Regex S and gives back an Automaton accepting the Regex RS (concatenation)
appendAutomata :: Automaton -> Automaton -> Automaton
appendAutomata aut1@(Automaton {finalStates=finalStates1,transitions=transitions1}) Automaton {finalStates=finalStates2,transitions=transitions2} =
  Automaton {finalStates = (finalStates aut2), transitions = (transitions aut1) ++ (transitions aut2) ++ newTransitions}
    where newTransitions = [(f,_EPSILON_,(max'+1)) | f <- finalStates aut1] -- connect all final states aut 'aut1' with the initial state of 'aut2' (which happens to be 0+max'+1)
          -- The following block of code ensures that the state indices of the two Automata 'aut1' and 'aut2' are disjoint!:
          max' = maximum (finalStates1 ++ [x | (x,chars,y) <- transitions1] ++ [y | (x,chars,y) <- transitions1]) -- the largest state index of all states in Automaton 'aut1' 
          aut2 = Automaton {finalStates = map (+(max'+1)) finalStates2 , transitions = [(x+max'+1,chars,y+max'+1) | (x,chars,y) <- transitions2]}
          -- We're adding (max'+1) to the states of the 2nd Automaton, assuming that it might have a state 0 but no states with a negative index!



-- Takes 2 Automata and connects them using a logical OR (Regex "|"):
-- How?!: Create a new initial state and connect it with an _EPSILON_ transition to the two initial states of the two given Automata !!!
--   (however, of course we first have to make the state indices of the 2 given automata disjoint, giving us the 2 disjoint Automata 'aut1' and 'aut2')
orAutomata :: Automaton -> Automaton -> Automaton
orAutomata Automaton {finalStates=finalStates1,transitions=transitions1} Automaton {finalStates=finalStates2,transitions=transitions2} =
  Automaton {finalStates = (finalStates aut1) ++ (finalStates aut2), transitions = (transitions aut1) ++ (transitions aut2) ++ newTransitions}
    where max' = maximum (finalStates1 ++ [x | (x,chars,y) <- transitions1] ++ [y | (x,chars,y) <- transitions1]) --the largest state index of all states in the 1st aut, NOT aut1! 
          aut1 = Automaton {finalStates = map (+1) finalStates1 , transitions = [(x+1,c,y+1) | (x,c,y) <- transitions1]} -- add 1 to all states of aut1 so there's room for a new 0
          aut2 = Automaton {finalStates = map (+(max'+2)) finalStates2 , transitions = [(x+max'+2,chars,y+max'+2) | (x,chars,y) <- transitions2]} -- !!! +2 !!!
          initialStateAut1 = 1      -- (see definition of aut1 above: every state got added +1, so the inital state no. 0 has now become state no. 1)
          initialStateAut2 = max'+2 -- (see definition of aut2 above: every state got added +(max'+2) ...)
          newTransitions   = [(0,_EPSILON_,initialStateAut1),(0,_EPSILON_,initialStateAut2)] -- at the beginning, we can choose whether to go into 'aut1' OR 'aut2'

removeEpsilonTransitions :: Automaton -> Automaton
removeEpsilonTransitions aut = Automaton { finalStates = finalStates aut, transitions = (until noEpsilonTransitions removeEpsilons (transitions aut)) }
  where noEpsilonTransitions trans = all (\trans -> snd3 trans /= _EPSILON_) trans
        removeEpsilons []     = []
        removeEpsilons (t:ts)  -- The actual magic happens here: translate/"skip" the epsilon transitions found
          | (snd3 t == _EPSILON_) = [(fst3 t, c, y) | (x, c, y) <- transitions aut, x == thd3 t] ++ (removeEpsilons ts)
          | otherwise             =                                                            t :  (removeEpsilons ts)
        fst3 (x,y,z) = x
        snd3 (x,y,z) = y
        thd3 (x,y,z) = z

-- Example on page 53:
-- *Regex> transitions $ removeEpsilonTransitions (Automaton {finalStates=[], transitions=[(0,_EPSILON_,1),(1,"a",2),(2,"b",3),(0,_EPSILON_,4),(4,"b",5),(5,"a",6)]})
-- [(0,"a",2),(1,"a",2),(2,"b",3),(0,"b",5),(4,"b",5),(5,"a",6)]











-- ===== ===== ===== ===== ===== Test Cases (Jan 03 2021 19:34-19:50 & Jan 04 2021 11:32-12:54, 17:16-17:44 & Jan 05 2021 16:41-(19:52)) ===== ===== ===== ===== =====

-- test _test_function_ _inputs_that_should_yield_true_ _inputs_that_should_yield_false ---> returns True if test succeeded for all inputs
-- e.g. test (>=10) [11,12,33,44,55,66] [4,6,7,1,3,2] == True
test :: (a -> Bool) -> [a] -> [a] -> Bool
test f trues falses = all f trues && all (not . f) falses

testRegex :: String -> [String] -> [String] -> Bool
testRegex regexStr = test (regexMatch (regexCompile regexStr))

-- testRegex "a*b+" ["b","ab","bb","aaaabbbbbb"] ["","a","aa","ba","aaaaabbbbba"] == True
-- testRegex "[abc][^abc][x-z]M{2,5}" ["bqyMMM","axxMM","czzMMMMM","aMxMM"] ["xxxMM","aaxMM","axaMM","axxM","bzzMMMMMM"] == True
-- testRegex "(a|b)*(xx|yy)+" ["xx","yy","axx","ayy","bxx","byy","axxxx","bxxyy","ababaayyxxyy"] ["","a","b","x","y","aa","bb","ab","xxa","yyb","abxy","abababxxyyy","xxyyx"] ==True
-- testRegex "\\w+" ["a","A","0","_","Hello","Hello_World"] [""," ","&","$","#","%", "Hello World"] == True
-- 
-- testRegex "(a|b)?" ["","a","b"] ["aa","bb","ab","ba","x","abba"] == True
-- testRegex "(\\+|-)?" ["","+","-"] ["++","--","+-","-+","x"," ","+--+"] == True
-- testRegex "\\d{1,}" ["0","1","9","00","123","4346734","54331190"] [""," ","x","0x","x0","999x999"] == True
-- testRegex "(\\+|-)?\\d{1,}\\.\\d{3,}" ["0.000","9.999","-42.3434","3.14159","+3.14159"] ["","+","-","0","1.23","+123.45","+1234",".1234","+.1234","++0.000","--0.000",
--                                                                                          "1..2345","0x000"] == True
--
-- testRegex "a|b|c" ["a","b","c"] ["","d","x"," ","0","aa","bb","cc","xx","ab","ba","cb","ac"] == True
-- testRegex "colou?r" ["color","colour"] [""," ","color ","colour ","colouur","xyz"] == True            -- https://cs.lmu.edu/~ray/notes/regex/
-- testRegex "gr(a|e)y" ["gray","grey"] ["", " ", "griy", "graey", "greay", "gray ", "grey "] == True
-- testRegex "gray|grey" ["gray","grey"] ["", " ", "griy", "graey", "greay", "gray ", "grey "] == True
-- testRegex "mi.....ft" ["minecraft","microsoft"] ["mycroft",""," ", "mift","mi..ft"] == True
--
--
--
--
--
-- regexCompile "a*b+"                   --> [Compiled Regex (3 states, of them 1 final, 4 transitions)]
-- regexCompile "[abc][^abc][x-z]M{2,5}" --> [Compiled Regex (12 states, of them 4 final, 11 transitions)]
-- regexCompile "[abc][^abc][x-z]M{2,5}" --> [Compiled Regex (37 states, of them 4 final, 42 transitions) ] (temporarily, after bad bugfix)
-- regexCompile "(a|b)*(xx|yy)+"         --> [Compiled Regex (14 states, of them 2 final, 30 transitions)]
-- regexCompile "\\w+"                   --> [Compiled Regex (2 states, of them 1 final, 2 transitions)]
--
-- regexCompile "(a|b)?"                    --> [Compiled Regex (5 states, of them 3 final, 4 transitions)]
-- regexCompile "(\\+|-)?"                  --> [Compiled Regex (5 states, of them 3 final, 4 transitions)]
-- regexCompile "\\d{1,}"                   --> [Compiled Regex (2 states, of them 1 final, 2 transitions) ]
-- regexCompile "(\\+|-)?\\d{1,}\\.\\d{3,}" --> [Compiled Regex (13 states, of them 1 final, 16 transitions) ]
--
-- regexCompile "a|b|c"   --> [Compiled Regex (8 states, of them 3 final, 8 transitions) ]
-- regexCompile "colou?r" --> [Compiled Regex (12 states, of them 1 final, 13 transitions) ]
-- regexCompile "gr(a|e)y" --> [Compiled Regex (11 states, of them 1 final, 12 transitions) ]
-- regexCompile "gray|grey" --> [Compiled Regex (17 states, of them 2 final, 16 transitions) ]
-- regexCompile "mi.....ft" --> [Compiled Regex (18 states, of them 1 final, 17 transitions) ]
--
--
--
--
-- regexExtract (regexCompile "\\d+\\.\\d+\\s") "This package weighs 456.789 kilograms." == Just "456.789 "
