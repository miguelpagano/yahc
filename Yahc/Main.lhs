
> module Main where
> import Yahc.Rules (showRules)
> import Yahc.Interaction (checkFile, checkStdIn, interaction, welcome, welcomeInt)
> import Yahc.Checker(initialSt)
> import Yahc.Completion

> import Data.Char
> import Control.Monad.State
> import System.Environment
> import System.Console.GetOpt
> import System.Console.Readline
> import System.Posix.Files

> data Flag = Rules 
>           | Version 
>           | Help
>           | Interactive
>    deriving Show
> 
> options :: [OptDescr (IO ())]
> options =
>  [ Option ['r'] ["rules"]       (NoArg optRules)         "muestra las reglas"
>  , Option ['V'] ["version"]     (NoArg optVersion)       "muestra la versión"
>  , Option ['i'] ["interactive"] (NoArg optInteractive)   "inicia una sesión interactiva"
>  , Option ['h'] ["help"]        (NoArg optHelp)          "muestra esta ayuda"
>  ]
> 

> optRules :: IO ()
> optRules = mapM_ putStrLn $ showRules []

> optVersion :: IO ()
> optVersion = putStrLn welcome

> optInteractive :: IO ()
> optInteractive = putStrLn welcomeInt >>
>                  (setAttemptedCompletionFunction . return) (cmdCompletion) >>
>                  initFile >>
>                  execStateT interaction initialSt >> 
>                  return ()

> optHelp :: IO ()
> optHelp = putStrLn $ usageInfo header options
>     where header = welcome

> main :: IO ()
> main = getArgs >>= yahcOpts
 
> yahcOpts :: [String] -> IO ()
> yahcOpts argv = 
>    case getOpt Permute options argv of
>       ([],[],[])  -> execStateT checkStdIn initialSt >> return ()
>       ([],fname:_,[]) -> execStateT (checkFile fname) initialSt >> return ()
>       (o,[],[]  ) -> head o
>       (_,_,errs)  -> optHelp


> userHome :: IO (Maybe String)
> userHome = getEnvironment >>= return . lookup "HOME"

> initFile :: IO ()
> initFile = getEnvironment >>= return . lookup "HOME" >>=
>            maybe defaultInitFile (\path -> withFile (path ++ "/.yahc")
>                                           readInitFile
>                                           defaultInitFile)

> defaultInitFile = withFile "/usr/share/yahc/yahc.inputrc" 
>                            readInitFile 
>                            (return ())

> withFile :: String -> (String -> IO ()) -> IO () -> IO ()
> withFile fname act def = fileExist fname >>=
>                          \exists -> if exists then act fname
>                                    else def