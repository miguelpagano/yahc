> module Yahc.PrettyExpr where
> import Yahc.Pretty
> import Yahc.Expr

-- Pretty-printing expressions.

> instance Pretty QExpr where
>     prettyPrec _ (QVar v)     = text v
>     prettyPrec _ (QCon k)     = text k
>     prettyPrec n (QUnSym s e) = if n > prec s
>                                 then parens ((text . show) s <> prettyPrec 0 e)
>                                 else (text . show) s <> prettyPrec (prec s) e
>     prettyPrec n (QSym s es)  = if n > prec s
>                                 then parens . cat . sep (show s) . map (prettyPrec (prec s)) $ es
>                                 else cat . sep (show s) . map (prettyPrec (prec s)) $ es
>         where spaced :: Symbol -> Doc
>               spaced s = text $ " " ++ show s ++ " " 
>               sep = punctuate . text . (\s -> " "++s++" ")

