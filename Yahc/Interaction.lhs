> module Yahc.Interaction where
> import Yahc.Derivation (Der, step, conclusion, mkder, addStep, undoStep, result, steps, lhsDer, stepsub)
> import Yahc.Expr (QExpr, ConName, equiv)
> import Yahc.Rules (parseRule, showRules, Axiom(prop), rules, axiom, definition, name)
> import Yahc.Pretty (Pretty, render, pretty)
> import Yahc.Subst (Substitution)
> import Yahc.Parser (parseExp, parseConst)
> import Yahc.Command 
> import Yahc.Error
> import Yahc.Utils
> import Yahc.Checker
> import Yahc.Writer (prettyForm)
> import Yahc.Writer.Latex
> import Yahc.Writer.Yahc

> import Control.Monad
> import Control.Monad.State
> import Control.Arrow
> import Data.Maybe (maybe, isNothing)
> import Data.Char (isSpace, toUpper)
> import System.Exit (exitSuccess)
> import System.IO 
> import System.Console.Readline
> import System.FilePath

> writeC = Command { cmdname = Write
>                 , action  = writeAction
>                 , longStr = "write"
>                 , short = "w"
>                 , help  = helpWrite
>                 , options = []
>                 }


> undoC = Command { cmdname = Undo
>                 , action  = undoAction
>                 , longStr = "undo"
>                 , short = "u"
>                 , help  = helpUndo
>                 , options = []
>                 }

> quitC = Command { cmdname = Quit
>                 , action  = quitAction
>                 , longStr = "quit"
>                 , short = "q"
>                 , help  = "Sale del programa"
>                 , options = []
>                 }

> helpC = Command { cmdname = Help
>                 , action  = helpAction
>                 , longStr = "help"
>                 , short = "h"
>                 , help  = helpHelp
>                 , options = []
>                 } 


> showC = Command { cmdname = Show
>                 , action  = showAction
>                 , longStr = "show"
>                 , short = "sh"
>                 , help  = helpShow
>                 , options = ["derivation", "goal", "rule", "rules"]
>                 } 

> nextC = Command { cmdname = Next
>                 , action  = nextAction
>                 , longStr = "next"
>                 , short = "n"
>                 , help  = helpNext
>                 , options = []
>                 }

> startC = Command { cmdname = Start
>                  , action  = startAction
>                  , longStr = "start"
>                  , short = "s"
>                  , help  = helpStart
>                  , options = []
>                  }

> addRuleC = Command { cmdname = AddRule
>                    , action  = addRuleAction
>                    , longStr = "add"
>                    , short = "a"
>                    , help  = helpAddRule
>                    , options = []
>                    }

> delRuleC = Command { cmdname = DelRule
>                    , action  = delRuleAction
>                    , longStr = "del"
>                    , short = "d"
>                    , help  = helpDelRule
>                    , options = []
>                    }

> addConstC = Command { cmdname = AddDef
>                     , action  = addConstant
>                     , longStr = "def"
>                     , short = "d"
>                     , help  = helpAddDef
>                     , options = []
>               }


> openC = Command { cmdname = Open
>                 , action  = openAction
>                 , longStr = "open"
>                 , short = "o"
>                 , help  = helpOpen
>                 , options = []
>                 }


> commands :: [Command]
> commands = [ quitC
>            , helpC
>            , showC
>            , addRuleC
>            , nextC
>            , startC
>            , undoC
>            , addConstC
>            , writeC
>            , openC
>            ]
             

> putStrLift :: Action
> putStrLift = lift . putStrLn 

> putStrPretty :: Pretty a => a -> ChkState ()
> putStrPretty = putStrLift . render .  pretty 

> putStrErr :: Action
> putStrErr err = gets derState >>= 
>                 maybe (putStrLift err)
>                       (\der -> maybe (return ())
>                        (putStrLift . ("Error en la línea "++) . show) (lineNo der) >>
>                        (putStrLift err)
>                       )

> emptyDer :: ChkState ()
> emptyDer = gets derState >>= \state -> when (isNothing state) (putStrLift errEmptyDer)


> showDerStep :: DerState -> Axiom -> QExpr -> Substitution -> ChkState ()
> showDerStep der rule res sub = putStrPretty $ mkder (lhs der) [stepsub rule res [sub]]
>     where lhs = conclusion . derivation

> showStep :: Axiom -> QExpr -> ChkState ()
> showStep rule res = putStrPretty $ step rule res


> endDer :: ChkState ()
> endDer = guardState (return ()) (\der -> 
>               putStrPretty (derivation der) >>
>               putStrLift "Ha concluido la derivación." >>
>               maybe (return ()) (putStrLift . ("Líneas leídas: " ++) . show) (lineNo der)
>               )

> showDer :: ChkState ()
> showDer = guardState emptyDer (putStrPretty . derivation)

> showThms :: ChkState ()
> showThms = gets theorems >>= mapM_ putStrLift . showRules 

> errorCmd :: String -> Command -> ChkState ()
> errorCmd err cmd = putStrErr err >> (putStrLift . help) cmd

> quitAction :: Action
> quitAction _ = lift exitSuccess 


> writeAction :: Action
> writeAction params = guardState emptyDer (\der -> 
>                         getParams params 2 (\ (fname:title:_) -> 
>                                             (if hasExtension fname 
>                                              then return . tail $ takeExtension fname
>                                              else return "tex") >>= \ext ->
>                                          lift (writeFile (replaceExtension fname ext) (prettyForm (backend ext) (derivation der) title)))
>                                       (errorCmd errFewParams writeC))
>                      where backend "tex" = ppLatex
>                            backend "yahc" = ppYahc
>                            backend _ = ppYahc

> openAction :: Action
> openAction params = guardState emptyDer (\der -> 
>                         getParams params 1 (\ (fname:_) -> undefined)
>                                       (errorCmd errFewParams writeC))



> startAction :: Action
> startAction params = getParams params 2 (\ (e1:e2:_) -> 
>                          parseExpL e1 (\from -> parseExpL e2 
>                             (\goal -> setState (mkder from []) (return goal) >> 
>                                      putStrPretty from)))
>                         (errorCmd errFewParams startC)

> nextAction :: Action
> nextAction params = guardState emptyDer $ \der -> 
>                      getParams params 2 (\ (ruleName:expr:_) -> 
>                        parseRuleL ruleName $ \rule -> 
>                          parseExpL expr $ \res -> 
>                            addStepSt rule res >>= \sub ->
>                                maybe (putStrErr (errBadStep ++ name rule))
>                                      (\s -> maybe (return ())
>                                            (\g -> if res == g then endDer else showDerStep der rule res s)
>                                            (goal der)) sub)
>                        (errorCmd errFewParams nextC)

> addRuleAction :: Action
> addRuleAction params = guardState emptyDer (\der -> 
>                        if (null . steps . derivation) der
>                        then putStrErr errNoStep
>                        else do (s,t) <- (return . (lhsDer &&& result . last . steps) . derivation) der
>                                getParams params 2 (\ (ruleName:ruleAbr:_) -> 
>                                                     (return (axiom ruleName (s `equiv` t) [ruleAbr] ruleName) >>=
>                                                      modify . addRule >>
>                                                      putStrLift ("Se agregó la regla "++ ruleName)))
>                                                  (errorCmd errFewParams addRuleC))


> delRuleAction :: Action
> delRuleAction params = guardState emptyDer (\der -> 
>                        if (null . steps . derivation) der
>                        then putStrErr errNoStep
>                        else getParams params 1 (\ (ruleName:_) -> 
>                                                      (modify (delRule ruleName)) >>
>                                                     putStrLift ("Se eliminó la regla "++ ruleName))
>                                                  (errorCmd errFewParams delRuleC))


> addConstant :: Action
> addConstant params = getParams params 4 (\(ruleName:ruleAbr:constName:constDef:_) ->
>                                                  parseConstL constName (\_ ->
>                                                  parseExpL constDef (\def ->
>                                                   (return (definition ruleName constName def [ruleAbr] ruleName) >>=
>                                                    modify . addRule >>
>                                                    putStrLift ("Se agregó la constante "++ ruleName)))))
>                                         (errorCmd errFewParams addConstC)

> showRule :: Action
> showRule name = parseRuleL name (putStrPretty . prop)

> showAction :: Action
> showAction param = case param of
>                   "rules" -> showThms
>                   "derivation" -> showDer                   
>                   "goal" -> guardState emptyDer ((maybe (return ()) putStrPretty) . goal)
>                   what -> if take 4 what == "rule" then showRule (drop 5 what)
>                          else putStrLift helpShow

> helpAction :: Action
> helpAction = maybe (putStrLift helpHelp) putStrLift . liftM help . readCmd commands
  
> undoAction :: Action
> undoAction = const $ guardState emptyDer (const undoStepState)

> parseExpL :: String -> (QExpr -> ChkState ()) -> ChkState ()
> parseExpL e next = liftM parseExp (return e) >>= either (putStrLift . show) next 

> parseConstL :: String -> (ConName -> ChkState ()) -> ChkState ()
> parseConstL e next = liftM parseConst (return e) >>= either (putStrLift . show) next

> parseRuleL :: String -> (Axiom -> ChkState ()) -> ChkState ()
> parseRuleL r next = gets theorems >>= 
>                     liftM (flip parseRule r) . return >>=
>                     maybe (putStrErr errNoSuchRule) next


> interaction :: ChkState ()
> interaction = lift (readline "yahc> ") >>= 
>               maybe (action quitC "") 
>               (\l -> if null l then interaction else 
>                     (return . parseCommand commands . strip) l >>=
>                     maybe (putStrErr errMissingCmd) (\ m -> addLine l m >>= uncurry ($)) >>
>                     putStrLift "" >>
>                     interaction)

> addLine l m = lift (addHistory l) >> 
>               return m

> checkStdIn :: ChkState () 
> checkStdIn =  checkHandle "stdin" stdin

> readLineY :: Handle -> ChkState String
> readLineY handle = liftedIf (lift (hIsEOF handle))
>                    (lift (return ""))
>                    (gets derState >>= 
>                     maybe (return ()) (\d -> modify (\c -> c { derState = return (nextLine d)})) >> 
>                     lift (hGetLine handle) >>= 
>                     \line -> if commented line then readLineY handle 
>                             else return line)
>     where commented :: String -> Bool
>           commented =  (=="--") . take 2

> compFile :: Handle -> ChkState ()
> compFile h = readLineY h >>= \lineRule ->
>              readLineY h >>= \ lineRes ->
>                 liftedIf (return (null lineRule)) 
>                          (endDer >> lift (hClose h) >> 
>                           gets derState >>= \(Just der) ->
>                           lift (writeFile "example.tex" (prettyForm ppLatex (derivation der) "Ejemplo")) >>
>                           lift exitSuccess)
>                          (parseRuleL (tail lineRule)
>                                      (\ax -> parseExpL lineRes (\res -> 
>                                              liftedIf (liftM isNothing (addStepSt ax res))
>                                                       (putStrErr (errBadStep ++ name ax) >> 
>                                                        lift exitSuccess)
>                                                       (compFile h)))
>                          )
                       
> checkHandle :: String -> Handle -> ChkState ()
> checkHandle name handle = lift (hIsEOF handle) >>= \atEOF ->
>                           unless atEOF 
>                                  (readLineY handle >>= \ exprL ->
>                                   parseExpL exprL (\expr ->
>                                          setStateF expr 1 name >>
>                                          compFile handle))

> checkFile :: FilePath -> ChkState ()
> checkFile fname = lift (openFile fname ReadMode) >>=  checkHandle fname 

> welcome, welcomeInt :: String
> welcome =  "yahc (versión 0.4): un chequeador de derivaciones para lógica proposicional."
> welcomeInt = unlines $
>              [ welcome
>              , "Para ver los posibles comandos y lo que hacen podes hacer \"help\""
>              ]
