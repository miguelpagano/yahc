> module Yahc.Writer.Latex (ppLatex) where

> import Yahc.Writer
> import Control.Monad.State
> import Data.Map (assocs)

> ppLatex :: Pretty a => a -> String -> String
> ppLatex d title = unlines preamble
>                    ++ unlines abbrevs
>                    ++ mkTitle title "yahc"
>                    ++ unlines begin
>                    ++ beginProof
>                    ++ (fst $ (runState . ppLaTeX . pretty) d MMath)
>                    ++ endProof
>                    ++ unlines closing


> data LatexMode = MText | MMath | MProof

> type LatexState a = State LatexMode a

> ppLaTeXBracket :: Bracket -> (String,String)
> ppLaTeXBracket Paren = ("(",")")
> ppLaTeXBracket Square = ("[","]")
> ppLaTeXBracket Curly = ("\\{","\\}")
> ppLaTeXBracket Angle = ("\\langle","\\rangle")

> ppLaTeXDec :: Decoration -> (String,String)
> ppLaTeXDec Underline = ("\\underline{","}")

> ppLaTeX :: DocForm  -> LatexState String
> ppLaTeX Empty = return ""
> ppLaTeX (Text s) = get >>= \m -> case m of
>                                 MMath -> return $ "\\text{"++s++"}"
>                                 MText -> return s
> ppLaTeX (Enclosed p doc) = ppLaTeX doc >>= \t -> 
>                            return $ b ++ " " ++ t ++ " " ++ e
>     where (b,e) = ppLaTeXBracket p
> ppLaTeX (MathText doc) = get >>= \m -> case m of
>                                MMath -> return doc
>                                MText -> return $ "$" ++ doc ++ "$"

> ppLaTeX (MathSymbol s) = return $ latexSym s
> ppLaTeX (Math doc) = get >>= \m -> case m of
>                                     MMath -> ppLaTeX doc
>                                     MText -> put MMath >> 
>                                             ppLaTeX doc >>=
>                                             \t -> return $ "$" ++ t ++ "$"
> ppLaTeX (doc :> doc') = get >>= \m ->
>                         ppLaTeX doc >>= \t ->
>                         put m >>
>                         ppLaTeX doc' >>= \t' ->
>                         put m >> 
>                         return (t ++ t')
> ppLaTeX (doc :^ doc') = get >>= \m -> case m of
>                             MMath -> return "\\\\"
>                             MText -> return "\\n\\n"
>                             >>= \sep ->
>                             ppLaTeX doc >>= \t ->
>                             put m >>
>                             ppLaTeX doc' >>= \t' ->
>                             put m >>
>                             return (t ++ sep ++ t' ++ "\n")
> ppLaTeX (Dec dec doc) = ppLaTeX doc >>= \t ->
>                         return $ b ++ t ++ e
>     where (b,e) = ppLaTeXDec dec

> ppLaTeX (Space i) = get >>= \m -> return . concat . take i . repeat $
>                      case m of 
>                        MMath -> "\\ "
>                        MText -> " "


> ppLaTeX (Optional d) = ppLaTeX d
> ppLaTeX (Tabular []) = return ""
> ppLaTeX (Tabular [d]) = ppLaTeX d >>= \b -> return  ( " & "++b)
> ppLaTeX (Tabular (d:ds)) = ppLaTeX d >>= \beg ->
>                            ppLaTeX (cat ds) >>= \end ->
>                            return (beg++" & "++end)


> -- | @preamble@ contains the heading of the document.
> preamble :: [String]
> preamble = [ "\\documentclass{article}"
>            , "\\usepackage[spanish]{babel}"
>            , "\\usepackage{amsmath}"
>            , "\\usepackage[utf8]{inputenc}"
>            ]

> begin =  [ "\\begin{document}"
>          , "\\maketitle"
>          , "\\yahc"
>          ]

> -- | @closing@ contains the closing of the document.
> closing :: [String]
> closing = ["\\end{document}"]

> -- | @abbrevs@
> abbrevs = [ "\\newcommand{\\EQ}[2]{ \\equiv &  \\{#1\\ (#2)\\ \\} &}"
>           , "\\newcommand{\\yahc}{Este documento ha sido generado a partir" 
>             ++ " de una derivación hecha en \\textbf{yahc}.}"
>           ]

> beginProof = "\\begin{align*}"
> endProof = "\\end{align*}" 

> mkTitle title author = "\\title{" ++ title ++ "}\n"
>                        ++ "\\author{}"

