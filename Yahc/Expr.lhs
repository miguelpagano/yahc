> {-| En este módulo hay tipos para fórmulas y para clases de
>     equivalencias de fórmulas; las clases de equivalencia se
>     usan para representar expresiones que tienen símbolos
>     asociativos-conmutativos.
> -}
> module Yahc.Expr ( Expr(Var,Con,UnSym,Sym)
>             , QExpr(QVar,QCon,QUnSym,QSym)
>             , VarName
>             , ConName
>             , SymbolName(..)
>             , Symbol(..)
>             , equivSymbol
>             , lift
>             , unlifts
>             , flatten
>             , isEquiv
>             , var
>             , con
>             , disj
>             , conj
>             , implies
>             , consequences
>             , equiv
>             , notequiv
>             , non
>             , veritas
>             , falsum
>             , body
>             , sort
>             ) where
> import Data.Char
> import Data.List(permutations, sort)
> import Control.Arrow

> type VarName = String
> type ConName = String

> data SymbolName = And
>                 | Or
>                 | Equiv
>                 | NotEq
>                 | Impl
>                 | Consq
>                 | Not

> {-| Una expresión puede ser una variable, una constante o un símbolo
>   binario aplicado a un par de expresiones.
> -}

> data Expr = Var VarName | Con ConName | UnSym Symbol Expr | Sym Symbol Expr Expr
>           deriving (Eq, Show)

> {-| Una QExpr es una expresión en la que los conectivos asociativos-conmutativos
>   son tratados como conectivos n-arios.
>  -}
> data QExpr = QVar VarName | QCon ConName | QUnSym Symbol QExpr | QSym Symbol [QExpr]
>           deriving (Show)

> body :: QExpr -> [QExpr]
> body (QVar _) = []
> body (QCon _) = []
> body (QUnSym _ e) = return e
> body (QSym s es) = es

> data Symbol = Symbol { repr :: String
>                      , prec :: Int
>                      , isAC :: Bool
>                      , symName :: SymbolName
>                      }

> instance Show Symbol where
>     show = repr

> instance Eq Symbol where
>     s == s' = repr s == repr s'

> instance Ord Symbol where
>     s <= s' = prec s <= prec s'

> instance Ord QExpr where
>     QVar v <= QVar w = v <= w
>     QVar _ <= _      = True
>     QCon _ <= QVar _ = False
>     QCon k <= QCon l = k <= l
>     QCon _ <= _      = True
>     QUnSym _ _ <= QVar _       = False
>     QUnSym _ _ <= QCon _       = False
>     QUnSym s e <= QUnSym s' e' = s < s' || (s == s' && e <= e')
>     QUnSym _ _ <= _            = True
>     QSym _ _   <= QVar _       = False
>     QSym _ _   <= QCon _       = False
>     QSym _ _   <= QUnSym _ _   = False
>     QSym s es  <= QSym s' es' = s < s' || (s == s' && if isAC s then sort es <= sort es' else es <= es')

> instance Eq QExpr where
>     QVar v == QVar w = v == w
>     QCon k == QCon l = k == l
>     QUnSym s e == QUnSym s' e' = s == s' && e == e'
>     QSym s es == QSym s' es' = s == s' && (if isAC s then sort es == sort es' else es == es')
>     _ == _ = False


> andSymbol = Symbol { repr = "∧", prec = 5, isAC = True , symName = And } 
> orSymbol  = Symbol { repr = "∨", prec = 6, isAC = True , symName = Or }
> notSymbol = Symbol { repr = "¬", prec = 7, isAC = True , symName = Not }

> equivSymbol  = Symbol { repr = "≡", prec = 2, isAC = True  , symName = Equiv }
> implSymbol   = Symbol { repr = "⇒", prec = 4, isAC = False , symName = Impl }
> consSymbol   = Symbol { repr = "⇐", prec = 3, isAC = False , symName = Consq }
> nEquivSymbol = Symbol { repr = "≢", prec = 1, isAC = False , symName = NotEq }



> -- | La proyección canónica de expresiones a clases de equivalencias.

> lift :: Expr -> QExpr
> lift (Var v) = QVar v
> lift (Con k) = QCon k
> lift (UnSym s e)  = QUnSym s . lift $ e
> lift (Sym s e e') | isAC s = flatten . QSym s . map lift $ [e,e']
>                   | otherwise = QSym s . map lift $ [e,e']


> -- | Elección de un representante de la clase de equivalencia.

> unlift :: QExpr -> Expr
> unlift (QVar v) = Var v
> unlift (QCon k) = Con k
> unlift (QUnSym s e) = UnSym s . unlift $ e
> unlift (QSym s (e:e':[])) = Sym s (unlift e) (unlift e')
> unlift (QSym s (e:es))    = Sym s (unlift e) (unlift (QSym s es))

> -- | Generación del conjunto de todos los elementos relacionados.

> unlifts :: QExpr -> [Expr]
> unlifts qe@(QSym s es) | isAC s = concatMap (glue s) ess
>                        | otherwise = (return . unlift) qe
>     where ess = (concatMap permutations . choices . map unlifts) es
>           
> unlifts (QUnSym s e) = map (UnSym s) (unlifts e)
> unlifts t = (return . unlift) t
> {-^   
>   Propiedades:
>   
>    > lift . unlift = id
>    > x `elem` (unlifts . lift) x
>   Más aun, si @s@ es no AC, entonces
>
>   > (unlift . lift) (Sym s e e') = Sym s e e'
>   > (unlifts . lift) s = return s
>  -} 


> -- | Todas las formas posibles de elegir un elemento de cada una
> -- de las listas.

> choices :: [[a]] -> [[a]]
> choices [] = return []
> choices (e:es) = concat [map (a:) (choices es) | a <- e]

> -- | Reconstrucción de todas las formas de parsear una expresión
> -- con un conectivo AC a partir de una lista de sus subexpresiones.

> glue :: Symbol -> [Expr] -> [Expr]
> glue s [e]    = return e
> glue s [e,e'] = return $ Sym s e e'
> glue s es = concat [(uncurry (zipWith (Sym s)) . (glue s *** glue s)) ps 
>                     | ps <- [splitAt i es | i <- [1..length es-1]]]
>                    


> -- | Definimos combinadores para construir (clases de equivalencia)
> -- | de expresiones.

> -- | Combinador para variables.
> var :: VarName -> QExpr
> var = QVar

> -- | Combinador para constantes.
> con :: ConName -> QExpr
> con = QCon

> -- | Combinador para construir la disyunción entre dos expresiones.
> disj :: QExpr -> QExpr -> QExpr
> e `disj` e' = flatten $ QSym orSymbol [e,e']

> -- | Combinador para construir la conjunción entre dos expresiones.
> conj :: QExpr -> QExpr -> QExpr
> e `conj` e' = flatten $ QSym andSymbol [e,e']

> -- | Combinador para construir la implicación.
> implies :: QExpr -> QExpr -> QExpr
> e `implies` e' = QSym implSymbol [e, e']

> -- | @consequences@ es @implies@ con argumentos dados vuelta.
> consequences :: QExpr -> QExpr -> QExpr
> e `consequences` e' = QSym consSymbol [e, e']

> -- | Combinador para construir la equivalencia de dos
> -- | expresiones.
> equiv :: QExpr -> QExpr -> QExpr
> e `equiv` e' = flatten $ QSym equivSymbol [e, e']

> isEquiv :: Symbol -> Bool
> isEquiv = (==equivSymbol)

> -- | @notequiv@ es @non . uncurry equiv@
> notequiv :: QExpr -> QExpr -> QExpr
> e `notequiv` e' = flatten $ QSym nEquivSymbol [e, e']

> -- | Negación de una fórmula.
> non :: QExpr -> QExpr
> non = QUnSym notSymbol

> -- | La constante $True$.
> veritas :: QExpr
> veritas = con "True"

> -- | La constante $False$.
> falsum :: QExpr
> falsum = con "False"

> -- | Flattening de expresiones binarias cuyo head symbol es asociativo.

> flatten :: QExpr -> QExpr
> flatten qe@(QSym s es) | isAC s = QSym s (foldr (flatteng s) [] (map flatten es))
>                        | otherwise = qe
> flatten (QUnSym s e) = QUnSym s . flatten $ e
> flatten e = e

> flatteng :: Symbol -> QExpr -> [QExpr] -> [QExpr]
> flatteng s a@(QSym s' es) as | s==s'     = es++as
>                              | otherwise = a:as
> flatteng s e as = e:as

> isVar :: QExpr -> Bool
> isVar (QVar _) = True
> isVar _        = False
