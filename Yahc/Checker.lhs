
> module Yahc.Checker where
> import Yahc.Derivation (Der, step, conclusion, mkder, addStep, undoStep, result, steps, lhsDer, stepsub)
> import Yahc.Subst (Substitution, useRule)
> import Yahc.Expr (QExpr, equiv)
> import Yahc.Rules (parseRule, showRules, Axiom, rules, axiom, name)
> import Yahc.Pretty (Pretty, render, pretty)
> import Yahc.Parser (parseExp)
> import Yahc.Error
> import Yahc.Utils

> import Control.Monad
> import Control.Monad.State
> import Control.Arrow
> import Data.Maybe 
> import Data.Char (isSpace, toUpper)

> data DerState = DerState { derivation :: Der
>                          , goal :: Maybe QExpr
>                          , lineNo :: Maybe Int
>                          , file :: Maybe FilePath
>                          }

> data CM = CM { derState :: Maybe DerState
>              , theorems :: [Axiom]
>              , flags    :: [Flag]
>              }

> data Flag = Verbosity Int 
>           | AllowedThms

> type ChkState a = StateT CM IO a
> type Action = String -> ChkState ()

> nextLine :: DerState -> DerState
> nextLine ds = ds { lineNo = liftM (+1) (lineNo ds) }


> mkstate :: Der -> Maybe QExpr -> DerState
> mkstate d q = DerState { derivation = d
>                        , goal = q 
>                        , lineNo = Nothing
>                        , file = Nothing
>                        }

> mkstateF :: Der -> Int -> FilePath -> DerState
> mkstateF d l f = DerState { derivation = d
>                           , goal = Nothing
>                           , lineNo = return l
>                           , file = return f
>                           }

> unlessNothing :: (a -> ChkState ()) -> Maybe a -> ChkState ()
> unlessNothing c = maybe (return ()) c

> alterState :: (DerState -> ChkState ()) -> ChkState ()
> alterState comp = gets derState >>= unlessNothing comp 

> setState :: Der -> Maybe QExpr -> ChkState ()
> setState d q = modify (\c -> c { derState = return (mkstate d q)})

> setStateF :: QExpr -> Int -> FilePath -> ChkState ()
> setStateF e l f = modify (\c -> c { derState = return (mkstateF (mkder e []) l f)})

> undoStepState :: ChkState ()
> undoStepState = alterDerState (\d -> d {derivation = (undoStep . derivation) d})

> alterDerState :: (DerState -> DerState) -> ChkState ()
> alterDerState f = gets derState >>= unlessNothing (\der -> modify (\c -> c { derState = (return . f) der}))

> modifyDerivation :: (Der -> Der) -> ChkState ()
> modifyDerivation f = alterDerState (\d -> d { derivation = f (derivation d)})

> addRule :: Axiom -> CM -> CM
> addRule thm ds = ds { theorems = (thm:theorems ds)}

> addRules :: [Axiom] -> CM -> CM
> addRules thms ds = ds { theorems = thms }

> -- | @delRule@ removes an added theorem.
> delRule :: String -> CM -> CM
> delRule thm ds = ds { theorems = delete' thm (theorems ds)}
>     where delete' thm [] = []
>           delete' thm (t:ts) = if name t == thm 
>                                then ts 
>                                else t:delete' thm ts

> addStepSt :: Axiom -> QExpr -> ChkState (Maybe Substitution)
> addStepSt rule res = do 
>                      Just der <- gets derState
>                      subst <- liftM (concatMap (flip (useRule (lhs der)) res)) $ return (rules rule)
>                      if null subst
>                        then return Nothing 
>                        else modifyDerivation (addStep (stepsub rule res (subs subst))) >> 
>                               (return . Just . fst . head) subst
>     where lhs = conclusion . derivation
>           subs = map fst



> liftedIf :: Monad m => m Bool -> m b -> m b -> m b
> liftedIf cond doTrue doFalse = cond >>= \c -> if c then doTrue else doFalse

> guardState :: ChkState () -> (DerState -> ChkState ()) -> ChkState ()
> guardState ifEmpty ifJust = gets derState >>= maybe ifEmpty ifJust 

> initialSt :: CM
> initialSt = CM { derState = Nothing
>                , theorems = []
>                , flags = []
>                }
