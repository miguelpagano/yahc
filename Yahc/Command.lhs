> module Yahc.Command where
> import Yahc.Checker
> import Control.Arrow
> import Data.Char (isSpace, toUpper)
> import Control.Monad

> data CmdName = Quit
>              | Help
>              | Show
>              | Start
>              | AddRule
>              | DelRule
>              | Next
>              | Undo
>              | AddDef
>              | Write
>              | Open
>                deriving (Eq, Show)

> data Command = Command { cmdname :: CmdName
>                        , action  :: Action
>                        , longStr :: String
>                        , short :: String
>                        , help  :: String
>                        , options :: [String]
>                        } 

> helpWrite = unlines [
>              "uso: write achivo, titulo"
>            , "escribe un archivo LaTeX con la derivación."
>            ]

> helpShow = unlines [
>              "uso: show [rules | goal | derivation | rule regla]"
>            , "muestran las reglas existentes,"
>            , "         el objetivo,"
>            , "         la derivación hasta ahora,"
>            , "         la fórmula correspondiente regla."
>            , "respectivamente."
>            ]

> helpUndo = unlines [
>              "uso: undo"
>            , "deshace la última regla aplicada."
>            ]

> helpStart = unlines [
>              "uso: start formula, formula'"
>            , "Comienza una nueva derivación y descarta" ++
>            "la derivación actual."
>            ]


> helpAddRule = unlines [
>                "uso: add nombre, abreviatura"
>               , "Agrega la última derivación como una nueva regla."
>               ]


> helpDelRule = unlines [
>                "uso: del nombre"
>               , "Elimina una regla agregada por el usuario."
>               ]


> helpNext = unlines [
>              "uso: next regla, resultado"
>            , "Trata de aplicar la regla "++
>            "a la conclusión de la derivación actual. "++
>            "Si tiene éxito agrega este paso a la "++
>            "derivación; si no tiene éxito no modifica la derivación."
>            ]


> helpHelp = unlines [
>            "Los comandos existentes son (entre paréntesis las versiones cortas)"
>            , "  show  (sh)"
>            , "  help  (h)"
>            , "  start (s)"
>            , "  next  (n)"
>            , "  quit  (q)"
>            , "  add   (a)"
>            , "  def   (d)"
>            , "  undo  (u)"
>            , "  write  (w)"
>            , "Puedes obtener más información sobre cada comando "++
>            "haciendo \"help comando\""
>            ]

> helpAddDef = unlines [
>               "uso: def nombre, abr, const, definición ",
>               "Permite agregar la definición de una constante."
>              ]

> helpOpen = unlines [
>              "uso: open archivo "
>            , "Lee un archivo con una derivación (puede ser parcial)."
>            ]

> readCmd :: [Command] -> String -> Maybe Command
> readCmd cmds = uncurry mplus <<< lookupCmd short &&& lookupCmd longStr
>     where lookupCmd on what = lookup what . map (on &&& id) $ cmds

> parseCommand :: [Command] -> String -> Maybe (Action, String)
> parseCommand cmds line = lookupL line >>= \act -> return (action act,rest)
>     where (cmd, rest) = (readCmd cmds *** tail') . break isSpace $ line
>           lookupL = return cmd
>           tail' xs = if null xs then xs else tail xs
