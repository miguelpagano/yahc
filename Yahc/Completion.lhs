> module Yahc.Completion where
> import Yahc.Command
> import Yahc.Interaction
> import Yahc.Utils
> import Yahc.Rules
> import Data.List
> import Data.Char
> import Control.Arrow
> import System.Console.Readline

> comms = map longStr commands

> commandCompletion :: String -> [String]
> commandCompletion str =  filter (isPrefixOf str) comms

> argsCompletion cmd arg = case cmd of
>                            "n" -> completeRule arg
>                            "next" -> completeRule arg
>                            _ -> maybe [] 
>                                (filter (isPrefixOf arg) . options)
>                                (find ((cmd ==) . longStr) commands)
>     where completeRule = map (++",") . completeRuleName

> extraCompletion cmd arg third = case cmd of
>                                   "show" -> if arg == "rule" 
>                                            then completeRuleName third
>                                            else if any isSpace arg 
>                                                 then completeRule
>                                                 else return []
>                                   "next" -> completeRule' 
>                                   "n"    -> completeRule'
>                                   _      -> return []
>                                      
>     where completeRule = (map ((drop . (1+) . length) rulePart) . completeRuleName) rule 
>           completeRule' = (map ((++",") . (drop . (1+) . length) arg) . completeRuleName) (arg ++ " " ++ third)
>           rulePart = (strip . snd . break isSpace) arg
>           rule = rulePart ++ " " ++ third


> completeRuleName rule = map (name . snd) . filter (isPrefixOf (map toLower rule) . fst) . ax_names $ []


> cmdCompletion s i j = if i == 0 
>                       then (return . commandCompletion) s >>= compl
>                       else getLineBuffer >>= return . strip . take (i-1) >>=                         
>                                \cmdAr -> return (break isSpace cmdAr) >>=
>                                         \(cmd,arg) -> if null arg then (return . argsCompletion cmd) s >>= compl
>                                                      else (return . (extraCompletion (strip cmd) (strip arg))) s >>= compl

> compl xss = setAttemptedCompletionOver True >> 
>             if null xss then return Nothing
>             else return . Just . options $ xss
>     where options = if length xss == 1 then (head &&& id) else (longestCommPrefix &&& id)