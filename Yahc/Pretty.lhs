> module Yahc.Pretty
>     ( module Yahc.Pretty
>     , module Text.PrettyPrint
>     ) where
> 
> import Text.PrettyPrint
> 
> class Pretty a where
>     pretty	 :: a -> Doc
>     prettyPrec :: Int -> a -> Doc
> 
>     pretty	 = prettyPrec 0
>     prettyPrec = const pretty
> 
> instance Pretty Doc where
>     pretty = id

