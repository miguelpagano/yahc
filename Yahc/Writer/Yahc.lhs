> module Yahc.Writer.Yahc (ppYahc) where

> import Yahc.Writer
> import Control.Monad.State
> import Data.Map (assocs)

> ppYahc :: Pretty a => a -> String -> String
> ppYahc d title = unlines preamble
>                    ++ mkTitle title "yahc"
>                    ++ (fst $ (runState . ppLaTeX . pretty) d MMath)


> data LatexMode = MText | MMath | MProof

> type LatexState a = State LatexMode a

> ppLaTeXBracket :: Bracket -> (String,String)
> ppLaTeXBracket Paren = ("(",")")
> ppLaTeXBracket Square = ("[","]")
> ppLaTeXBracket Curly = ("{","}")
> ppLaTeXBracket Angle = ("\\langle","\\rangle")

> ppLaTeXDec :: Decoration -> (String,String)
> ppLaTeXDec Underline = ("","")

> ppLaTeX :: DocForm  -> LatexState String
> ppLaTeX Empty = return ""
> ppLaTeX (Text s) = return s
> ppLaTeX (Enclosed p doc) = ppLaTeX doc >>= \t -> 
>                            return $ b ++ " " ++ t ++ " " ++ e
>     where (b,e) = ppLaTeXBracket p
> ppLaTeX (MathText doc) =  return doc
> ppLaTeX (Math doc) = ppLaTeX doc
> ppLaTeX (doc :> doc') = ppLaTeX doc >>= \t ->
>                         ppLaTeX doc' >>= \t' ->
>                         return (t ++ t')
> ppLaTeX (doc :^ doc') = ppLaTeX doc >>= \t ->
>                        ppLaTeX doc' >>= \t' ->
>                        return (t ++ "\n" ++ t')
> ppLaTeX (Dec dec doc) = ppLaTeX doc >>= \t ->
>                         return $ b ++ t ++ e
>     where (b,e) = ppLaTeXDec dec

> ppLaTeX (Space i) = return . concat . take i . repeat $ " "


> -- | @preamble@ contains the heading of the document.
> preamble :: [String]
> preamble = [ "-- Archivo generado por yahc."
>            ]

> mkTitle title author = "-- Título: " ++ title ++ "\n"
>                        ++ "-- Autor: " ++ author

