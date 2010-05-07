Una derivación consiste en una expresión junto con una secuencia de
pasos; en cada paso aplicamos una regla.

> module Yahc.Derivation ( Der
>                        , Step
>                        , steps
>                        , law
>                        , substitutions
>                        , result
>                        , mkder
>                        , conclusion
>                        , step
>                        , stepsub
>                        , addStep
>                        , undoStep
>                        , lhsDer
>                        )
>     where
> import Yahc.Rules (Axiom(..))
> import Yahc.Expr
> import Control.Arrow hiding (first)
> import Yahc.Subst
> import Yahc.Pretty
> import Yahc.PrettyExpr


> data Der = Der {
>       first :: QExpr
>     , steps :: [Step]
>     }

> data Step = Step {
>       law :: Axiom
>     , result :: QExpr
>     , substitutions :: [Substitution] -- ^ Possible ways to rewrite.
> --    , wrong :: [(Axiom, QExpr)]     Wrong tries done by the user at this step.
>     }

La conclusión de una regla es la última regla aplicada.

> conclusion :: Der -> QExpr
> conclusion d = if (null . steps) d then first d
>                else (result . last . steps) d

> mkder :: QExpr -> [Step] -> Der
> mkder e s = Der { first = e
>                 , steps = s
>                 }

> step :: Axiom -> QExpr -> Step
> step r e = Step { law = r
>                 , result = e
>                 , substitutions = []
>                 }

> stepsub :: Axiom -> QExpr -> [Substitution] -> Step
> stepsub r e sub = Step { law = r
>                        , result = e
>                        , substitutions = sub
>                        }


> addStep :: Step -> Der -> Der
> addStep s d = mkder (first d) (steps d++[s])

> undoStep :: Der -> Der
> undoStep d = if (not . null . steps) d then d { steps = (init . steps) d}
>              else d

Pretty printing de derivaciones:

> instance Pretty Der where
>     pretty = (nest 3 . pretty . first) &&& (vcat . map pretty . steps) >>> (arr . uncurry) ($+$)

> instance Pretty Step where
>     pretty = (decorate . (law &&& head . substitutions)) &&& (nest 3 . pretty . result) >>> (arr . uncurry)($+$)
>         where decorate :: (Axiom, Substitution) -> Doc
>               decorate (ax,subs) = text "≡ " <> braces ((text . reprAx) ax $$ pretty subs)

> lhsDer :: Der -> QExpr
> lhsDer = first