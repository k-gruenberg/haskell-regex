# haskell-regex
A simple Haskell implementation of a Regex compiler / engine.  
Use `regexCompile` to compile a regex from a String and use `regexMatch` to match a String against a compiled regex.

    module Regex (
      Regex,
      regexCompile,   -- :: String -> Regex
      regexMatch,     -- :: Regex -> String -> Bool
    
      -- Extras:
      regexFind,      -- :: Regex -> String -> Maybe Int
      regexExtract,   -- :: Regex -> String -> Maybe String
      regexReplace,   -- :: Regex -> String -> String -> String
      regexReplaceAll -- :: Regex -> String -> String -> String
    )
