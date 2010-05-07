> {-# OPTIONS -XTypeSynonymInstances #-}
> {-# OPTIONS -XFlexibleInstances #-}

> module Yahc.Subst where
> import Prelude hiding (lookup)
> import Yahc.Expr
> import Yahc.Parser
> import Yahc.Rules
> import Yahc.Utils
> import Data.List (intersect, permutations, nub, subsequences, intersperse)
> import Yahc.Pretty hiding (empty)
> import Yahc.PrettyExpr
> import Control.Monad
> import Control.Arrow
> import Data.Map (Map, findWithDefault, union, intersection, assocs, singleton, empty, keys)

TODO: Necesitamos Substituible?

> class Substituible a where
>     mkvar :: VarName -> a
>     (-/-) :: a -> Subst a -> a

> type Subst a = Map VarName a

> lookup :: (Eq a, Substituible a) => Subst a -> VarName -> a
> lookup m x = findWithDefault (mkvar x) x m

> instance Substituible QExpr where
>     mkvar = QVar
>     (QVar x) -/- sigma = lookup sigma x
>     (QCon k) -/- sigma = con k
>     (QUnSym s e) -/- sigma = QUnSym s (e -/- sigma)
>     (QSym s es) -/- sigma = QSym s $ map (-/- sigma) es

> -- | Return the union of two substitutions if they match in their
> -- common domain.
> merge      :: Subst QExpr -> Subst QExpr -> [Subst QExpr]
> merge s1 s2 = if agree then return (s1 `union` s2) else []
>  where agree = all (\v -> (var v) -/- s1 == (var v) -/- s2) (keys s1 `intersect` keys s2)

> -- | Lifting of @merge@ to lists of substitutions.
> merge' :: [Subst QExpr] -> [Subst QExpr] -> [Subst QExpr]
> merge' ss = join . (liftM2 merge ss) 

> -- |Matching of two expressions.
> match :: QExpr -> QExpr -> [Subst QExpr]
> match (QVar v) e = return . singleton v $ e
> match (QCon k) (QCon k') | k == k'   = return empty
>                          | otherwise = []
> match (QUnSym s e) (QUnSym s' e') | s == s' = match e e'
>                                   | otherwise = [] 

> match (QSym s es) (QSym s' es') | s == s'    = matches s (terms es) (terms es')
>                                 | otherwise = []
> match _ _ = []

> -- terms :: [QExpr] -> [([QExpr],[QExpr])]
> -- terms es = [splitAt i es | i <- [1..length es-1]]

> terms :: [QExpr] -> [([QExpr],[QExpr])]
> terms [] = []
> terms (e:es) = ([e],es):concatMap (pairs e) (terms es)
>     where pairs _ ([],_) = []
>           pairs _ (_,[]) = []
>           pairs e (xs,ys) = [(e:xs,ys),(e:ys,xs)]


> matches :: Symbol -> [([QExpr],[QExpr])] -> [([QExpr],[QExpr])] -> [Subst QExpr]
> matches s ps es | not (isAC s) = concat [merge' (matchReb (p,e)) (matchReb (p',e'))
>                                        | (p,p') <- ps, (e,e') <- es] 
>                 | otherwise  = concat [merge' (matchReb (p,e)) (matchReb (p',e'))
>                                        ++ merge' (matchReb (p',e)) (matchReb (p,e'))
>                                        | (p,p') <- ps
>                                        , (e,e') <- es]
>     where matchReb :: ([QExpr],[QExpr]) -> [Subst QExpr]
>           matchReb = uncurry match . (reb s *** reb s)


> reb        :: Symbol -> [QExpr] -> QExpr
> reb s (e:[]) = e
> reb s es     = QSym s es
> 

> type Location = ([Int],[Int])

> subqexprs :: QExpr -> [(Location,QExpr)]
> subqexprs e@(QVar v) = [(([],[]),e)]
> subqexprs e@(QCon k) = [(([],[]),e)]
> subqexprs e@(QUnSym s e') = (([],[]),e): map (first (first (0:))) (subqexprs e')
> subqexprs e@(QSym s fs) = (([],[]),e) : 
>                           (map (((,) []) &&& body) . filter ((>=2) . length) . init . subsequences) indices
>                            ++ concat [map (first (first (i:))) (subqexprs (fs!!i)) | i <- indices]
>     where indices = [0..length fs-1]
>           body = flatten . (QSym s) . keepIndices fs


> matchings ::  Rule -> QExpr -> [(Location, [Subst QExpr])]
> matchings rule = filter (not . null . snd) . map (second (match (lhs rule))) . subqexprs


> replace :: QExpr -> Location -> QExpr -> QExpr
> replace _ ([],[]) e' = e'
> replace (QUnSym s e) (0:is,children) e' = QUnSym s $  replace e (is,children) e'
> replace (QSym s es) (i:is,children) e' = QSym s (ys++replace e (is,children) e':xs)
>     where (ys,e:xs) = splitAt i es
> replace (QSym s es) ([],children) e' = QSym s (e':elimIndices es children)


Las funciones que siguen son las que hacen el trabajo interesante:

> -- | Given an expression @e1@, one rule @r@, and the conclusion
> -- @e2@,  @useRule e1 r e2@ returns all the possibles substitutions
> -- @subs@ such that for each @s@ in @subs@, 
> -- @rewrite r e1 (fst s) (snd s) == e2@.

> useRule :: QExpr -> Rule -> QExpr -> [(Substitution, QExpr)]
> useRule e1 rule e2 = concatMap remove
>                      [(l,filter ((e2==) . snd) (map (id &&& rewrite rule e1 l) ss)) | (l,ss) <- matchings rule e1]
>     where remove p = if (null . snd) p then mzero else return . assLeft . (fst &&& head . snd) $ p 


> rewrite :: Rule -> QExpr -> Location -> Subst QExpr -> QExpr
> rewrite r e l s = flatten . replace e l $ (rhs r -/- s) 


> type Substitution = (Location, Subst QExpr)

> instance Pretty Substitution where
>     pretty = pretty . snd

> instance Pretty (Subst QExpr) where
>   pretty = parens . bvcat . (cat . sep . map text *** cat . sep . map pretty) . unzip . assocs
>         where (<+>) = (Yahc.Pretty.<+>)
>               sep = punctuate . text $ ","
>               bvcat (p,q) = (p <+> text ":=") <+> q

> neg = rules (axioms!!3)!!6
> t = var "p" `equiv` non (var "q") `equiv` var "s"
> t' = non $ var "p" `equiv` var "q" `equiv` var "s"

> bt = [var "p", non (var "q"),var "s"]
> bn = [var "q", non (var "p")]