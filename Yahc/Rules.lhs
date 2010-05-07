``Las leyes están para ser violadas.'' (Alguien)

> module Yahc.Rules (
>                Axiom(..)
>              , Rule(..)
>              , axioms
>              , axiom
>              , ax_names
>              , parseRule
>              , showRules
>              , definition
>              )
>     where
> import Yahc.Expr
> import Yahc.Utils
> import Control.Arrow
> import Control.Monad
> import Data.List (nub, find, intercalate)
> import Data.Char (isSpace, isAlpha, toLower)

Cada regla es un par de expresiones junto con un nombre, además
distinguimos entre axiomas y teoremas.

Por el momento sólo consideramos equivalencias, es decir no podemos
escribir el axioma: p ⇒ p.

> data Rule = Rule { lhs  :: QExpr
>                  , rhs  :: QExpr
>                  }
>           deriving (Eq)

> data Axiom = Axiom { reprAx :: String
>                    , prop   :: QExpr
>                    , name   :: String
>                    , shortNames :: [String]
>                    , rules  :: [Rule]
>                    }

> indent :: Int -> String -> String
> indent n = ((take n . repeat) ' ' ++)

> instance Show Axiom where
>     show = (indent 4 . reprAx)

> rule :: QExpr -> QExpr -> Rule
> rule lhs rhs = Rule { lhs = lhs, rhs = rhs }

> axiom :: String -> QExpr -> [String] -> String -> Axiom
> axiom name qe sNames repr = Axiom { reprAx = repr
>                                   , prop = qe
>                                   , name = name
>                                   , shortNames = sNames
>                                   , rules = metaAxioms qe ++ mkAxioms qe
>                                   }

> metaAxioms :: QExpr -> [Rule]
> metaAxioms e = [rule veritas e, rule e veritas]

> mkAxioms :: QExpr -> [Rule]
> mkAxioms e@(QSym s es) | isEquiv s = nub $ map rebuild (unlifts e)
>                        | otherwise = []
>     where rebuild (Sym _ e e') = rule (lift e) (lift e')

> definition :: String -> ConName -> QExpr -> [String] -> String -> Axiom
> definition name k def sNames repr = Axiom { reprAx = repr
>                                           , prop = equiv (QCon k) def
>                                           , name = name
>                                           , shortNames = sNames
>                                           , rules = [rule (QCon k) def]
>                                           }

Algunos axiomas:

> p,q,r :: QExpr
> p = var "p"
> q = var "q"
> r = var "r"

> assoc_equiv = axiom "Asociatividad Equivalencia" (((p `equiv` q) `equiv` r) `equiv` p `equiv` (q `equiv` r))
>               ["A =","A Eq"]
>               "Asociatividad de la Equivalencia"

> comm_equiv = axiom "Conmutatividad Equivalencia" (p `equiv` q `equiv` (q `equiv` p))
>              ["C =","C Eq"]
>              "Conmutatividad de la Equivalencia"


> neutral_equiv :: Axiom
> neutral_equiv = axiom "Neutro equivalencia" (p `equiv` veritas `equiv` p)
>                 ["N =","N Eq"]
>                 "Neutro de la Equivalencia"

> negation_def :: Axiom
> negation_def = axiom "Definicion de negacion" (non (p `equiv` q) `equiv` non p `equiv` q)
>                ["D -","D Neg"]
>                "Definición de la Negación"

> false_def :: Axiom
> false_def = axiom "Definicion de false" (falsum `equiv` non veritas)
>             ["D F","D False"]
>             "Definición de false"

> diseq_def :: Axiom
> diseq_def = axiom "Definicion de discrepancia" ((p `notequiv` q) `equiv` non (p `equiv` q))
>             ["D =/=","D Disc"]
>             "Definición de la Discrepancia"

> assoc_or = axiom "Asociatividad Disyuncion" (((p `disj` q) `disj` r) `equiv` (p `disj` (q `disj` r)))
>            ["A \\/","A Disy"]
>            "Asociatividad de la Disyunción"

> comm_or = axiom "Conmutatividad Disyuncion" ((p `disj` q) `equiv` (q `disj` p))
>           ["C \\/","C Disy"]
>           "Conmutatividad de la Disyunción"

> idemp_or :: Axiom
> idemp_or = axiom "Idempotencia disyuncion" ((p `disj` p) `equiv` p)
>            ["I \\/","I Disy"]
>            "Idempotencia de la Disyunción"

> distr_or_equiv = axiom "Distributividad de disyuncion con la equivalencia"
>                         (p `disj` (q `equiv` r) `equiv`
>                         (p `disj` q) `equiv` (p `disj` r))
>                  ["D \\/ =","D Disy Eq"]
>                  "Distributividad de la disyunción respecto de la equivalencia"

> tertium_non_datur = axiom "Tercero excluido" (p `disj` non p) 
>                     ["TE"]
>                     "Tercero excluido"

> regula_aurea :: Axiom
> regula_aurea = axiom "Regla dorada" ((p `conj` q)  `equiv` p `equiv`
>                                     q `equiv` (p `disj` q))
>                ["RD"]
>                "Regla dorada"

> implication_def :: Axiom
> implication_def = axiom "Definicion de implicacion" ((p `implies` q) `equiv`
>                                                      (p `disj` q) `equiv` q)
>                   ["D =>","D Impl"]
>                   "Definición de la implicación"

> consequence_def :: Axiom
> consequence_def = axiom "Definicion de consecuencia" ((p `consequences` q) `equiv`
>                                                      (p `disj` q) `equiv` p)
>                   ["D <=","D Cons"]
>                   "Definición de la consecuencia"


> double_negation :: Axiom 
> double_negation = axiom "Doble negacion" (non (non p) `equiv` p)
>                   ["--","Neg Neg"]
>                   "Doble Negación"

> negation_equiv :: Axiom
> negation_equiv = axiom "Equivalencia y negacion" (p `equiv` non p `equiv` falsum)
>                  ["= -","Eq Neg"]
>                  "Equivalencia y negación"

> abs_disjunction :: Axiom
> abs_disjunction = axiom "Absorbente de la disyuncion" ((p `disj` veritas) `equiv` veritas)
>                   ["EA \\/","EA Disy"]
>                   "Absorbente de la disyunción"

> id_disjunction :: Axiom
> id_disjunction = axiom "Neutro de la disyuncion" ((p `disj` falsum) `equiv` p)
>                   ["N \\/","N Disy"]
>                  "Neutro de la disyunción"

> star_theorem :: Axiom
> star_theorem = axiom "Teorema (*)" ((p `disj` q) `equiv` (p `disj` non q) `equiv` p)
>                ["T *"]
>                "Teorema estrella"

> neg_implication :: Axiom
> neg_implication = axiom "Negacion de la implicacion" ((non (p `implies` q)) `equiv` 
>                                                           (p `conj` non q))
>                   ["- =>","Neg Impl"]
>                   "Negación de la implicación"

> axioms = [ assoc_equiv
>          , comm_equiv
>          , neutral_equiv
>          , negation_def
>          , false_def
>          , diseq_def
>          , idemp_or
>          , assoc_or
>          , comm_or
>          , distr_or_equiv
>          , tertium_non_datur
>          , regula_aurea
>          , implication_def
>          , consequence_def
>          , double_negation 
>          , negation_equiv 
>          , abs_disjunction
>          , id_disjunction
>          , star_theorem
>          ]

> ax_names :: [Axiom] -> [(String, Axiom)]
> ax_names thms = map (map toLower . name &&& id) $ axioms ++ thms

> parseRule :: [Axiom] -> String -> Maybe Axiom
> parseRule thms = uncurry mplus <<< (lookupShort thms &&& flip lookup (ax_names thms)) . map toLower . strip . stripRules 

> lookupShort :: [Axiom] -> String -> Maybe Axiom
> lookupShort thms s = liftM snd . find (elem s . fst) . map ((map (map toLower) . shortNames) &&& id) $ axioms ++ thms

> stripRules :: String -> String
> stripRules = rstrip . filter (not . flip elem "}") . dropWhile (flip elem "{ ") 

> showRules :: [Axiom] -> [String]
> showRules thms = map (\ax -> (name ax ++ " [" ++ intercalate "," (shortNames ax) ++ "]")) $ axioms ++ thms