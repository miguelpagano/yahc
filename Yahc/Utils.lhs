> module Yahc.Utils ( split
>                   , strip
>                   , lstrip
>                   , rstrip
>                   , splitParams
>                   , getParams
>                   , keepIndices
>                   , elimIndices
>                   , assLeft
>                   , assRight
>                   , longestCommPrefix
>                   , splitAt'
>                   )
>     where
> import Control.Monad
> import Control.Arrow
> import Data.List

> split :: (Char -> Bool) -> String ->  [String]
> split p "" = []
> split p xs = if p x then split p xs'
>              else (arr . uncurry) (:) <<< (id *** split p) . break p $ xs
>    where (x,xs') = (head &&& tail) xs

> lstrip,rstrip :: String -> String
> lstrip (' ':xs) = lstrip xs
> lstrip xs = xs
> rstrip "" = ""
> rstrip xs = if last xs == ' ' then rstrip (init xs) else xs

> strip :: String -> String
> strip = lstrip . rstrip

> splitParams :: String -> [String]
> splitParams = map strip . split (==',')

> getParams :: Monad m => String -> Int -> ([String] -> m ()) -> m () -> m ()
> getParams string n cont fail = (return . splitParams) string >>= 
>                                \parts -> if length parts >= n
>                                         then cont parts
>                                         else fail

Los enteros no deben estár repetidos.

> elimIndices :: [a] -> [Int] -> [a]
> elimIndices xs [] = xs
> elimIndices [] _ = []
> elimIndices (_:xs) (0:is) = elimIndices xs (map pred is)
> elimIndices (x:xs) is = x:elimIndices xs (map pred is)


> keepIndices :: [a] -> [Int] -> [a]
> keepIndices _ [] = []
> keepIndices [] _ = []
> keepIndices (x:xs) (0:is) = x:keepIndices xs (map pred is)
> keepIndices (_:xs) is = keepIndices xs (map pred is)

> assLeft :: (a,(b,c)) -> ((a,b),c)
> assLeft = (fst &&& fst . snd) &&& snd . snd

> assRight ::  ((a,b),c) -> (a,(b,c))
> assRight = fst . fst &&& (snd . fst &&& snd)

> longestCommPrefix :: Eq a => [[a]] -> [a]
> longestCommPrefix [] = []
> longestCommPrefix xss = maximumBy length' . filter (\pre -> all (isPrefixOf pre) xss) $ prefixes
>     where prefixes = reverse $ inits shortest
>           length' a b = compare (length a) (length b)
>           shortest = minimumBy length' xss


> splitAt' :: Int -> [a] -> ([a],a,[a])
> splitAt' _ [] = error "Index too large"
> splitAt' i xs = let (bs,es) = splitAt i xs
>                 in if not (null es) then (bs,head es, tail es)
>                    else error "Index too large"
