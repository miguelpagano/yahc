> -- | Writes a complete derivation in a LaTeX file.
> {-# OPTIONS -XTypeSynonymInstances #-}
> {-# OPTIONS -XFlexibleInstances #-}
> {-# OPTIONS -XOverloadedStrings #-}
> {-# OPTIONS -XRank2Types #-}
> {-# OPTIONS -XFlexibleContexts #-}

> module Yahc.Writer where

> import Yahc.Derivation
> import Yahc.Rules
> import Yahc.Subst
> import Yahc.Expr 
> import Yahc.Utils
> import Control.Arrow
> import Control.Monad.State
> import Data.Map (assocs)

> type Backend a = Pretty a => a -> String -> String

> prettyForm :: forall a . Pretty a => Backend a -> a -> String -> String
> prettyForm backend = backend

> latexSym :: SymbolName -> String
> latexSym And = "\\wedge" 
> latexSym Or = "\\vee"   
> latexSym Not = "\\neg"   

> latexSym Equiv = "\\equiv"      
> latexSym Impl = "\\Rightarrow"
> latexSym Consq = "\\LeftArrow" 
> latexSym NotEq = "\\not\\equiv"

> data DocForm = Empty
>              | Space Int
>              | Text String
>              | MathText String
>              | MathSymbol SymbolName
>              | Enclosed Bracket DocForm
>              | Math DocForm
>              | DocForm :> DocForm
>              | DocForm :^ DocForm
>              | Dec Decoration DocForm
>              | Tabular [DocForm]
>              | Optional DocForm

> data Bracket = Paren
>              | Square
>              | Curly
>              | Angle

> data Decoration = Underline

> class Pretty a where
>     pretty :: a -> DocForm

> (.!>) :: DocForm -> DocForm -> DocForm
> Empty .!> _ = Empty
> _ .!> Empty = Empty
> d .!> d' = d :> d'

> text :: String -> DocForm
> text = Text

> parens,angles,curly,square :: DocForm -> DocForm
> parens = Enclosed Paren
> square = Enclosed Square
> curly = Enclosed Curly
> angles = Enclosed Angle

> math :: String -> DocForm
> math = MathText

> mathenv = Math

> underline = Dec Underline
> space = Space

> cat :: [DocForm] -> DocForm
> cat = foldl (:>) Empty

> vcat :: [DocForm] -> DocForm
> vcat = foldl (:^) Empty

> punctuate :: DocForm -> [DocForm] -> [DocForm]
> punctuate _ [] = []
> punctuate _ [x] = [x]
> punctuate s (x:xs) = x:s:punctuate s xs


> sep = (:> math " ") . pretty
> sepBy = punctuate . sep

> instance Pretty Symbol where
>     pretty = MathSymbol . symName

> instance Pretty QExpr where
>     pretty = prettyLoc 0 Nothing

> prettyLoc :: Int -> Maybe [Int] -> QExpr -> DocForm
> prettyLoc _ l (QVar v)  = maybe id (const underline) l (math v)
> prettyLoc _ l (QCon k) = maybe id (const underline) l (math k)
> prettyLoc n l (QUnSym s e) = maybe (simply n) (underline' n) l (QUnSym s e)
> prettyLoc n l (QSym s es) =  maybe (simply n) (underline' n) l (QSym s es)

> underline' :: Int -> [Int] -> QExpr -> DocForm
> underline' _ _ (QVar v) = underline (math v)
> underline' _ _ (QCon k) = underline (math k)
> underline' n [] e = underline (simply n e)
> underline' n (0:xs) (QUnSym s e) = if n > prec s
>                                    then parens $ sep s :> underline' 0 xs e
>                                    else sep s :> underline' (prec s) xs e
> underline' n (i:xs) (QSym s es) = if n > prec s
>                                   then parens . cat . sepBy s $ subexps 0 es1 ++ underline' 0 xs e:subexps 0 es2
>                                   else cat . sepBy s $ subexps (prec s) es1++ underline' (prec s) xs e:subexps (prec s) es2
>     where (es1,e,es2) = splitAt' i es
>           subexps m = map (simply m)


> simply :: Int -> QExpr -> DocForm
> simply _ (QVar v) = math v
> simply _ (QCon k) = math k
> simply n (QUnSym s e) = if n > prec s
>                         then parens $ sep s :> simply 0 e
>                         else sep s :> simply (prec s) e
> simply n (QSym s es) = if n > prec s
>                        then parens . cat . sepBy s . map (simply 0) $ es
>                        else cat . sepBy s . map (simply (prec s)) $ es

> instance Pretty Der where
>     pretty d = vcat $ formulas (steps d) (lhsDer d)

> formulas :: [Step] -> QExpr -> [DocForm]
> formulas [] f = [Tabular [pretty f]]
> formulas (s:ss) f = (Tabular [f'] :^ expl) : formulas ss nf
>     where f' = prettyLoc 0 loc f
>           (loc,sub) = (Just . fst *** pretty) . head . substitutions $ s
>           nf = result s
>           axiom = (text . reprAx . law) s
> --          expl = mathenv $ math "\\EQ" :> curly' axiom :> curly' sub
>           curly' d  = math "{" :> d :> math "}"
>           expl = Tabular [MathSymbol Equiv, axiom, Optional (parens sub)]

\\equiv &  \\{#1\\ (#2)

> instance Pretty (Subst QExpr) where
>  pretty = bvcat . (cat . sep . map math *** cat . sep . map pretty) . unzip . assocs
>         where sep = punctuate . math $ ","
>               bvcat (p,q) =  p .!> math ":=" .!> q
