> {-| En este módulo hay tipos para fórmulas y para clases de
>     equivalencias de fórmulas; las clases de equivalencia se
>     usan para representar expresiones que tienen símbolos
>     asociativos-conmutativos.
> -}
> module Yahc.Parser ( parseExp
>                    , parseSubst
>                    , parseConst
>                    ) where
> import Yahc.Expr
> import Text.Parsec
> import Text.Parsec.Expr
> import Text.Parsec.Language 
> import Text.Parsec.Prim
> import qualified Text.Parsec.Token as P
> import Data.Char
> import Data.List
> import Control.Arrow

> operators = [ "/\\","\\/","=>","<="
>             , "&&", "||"
>             , "==","=/=", "/="
>             , "-"
>             ]

> lexer :: P.TokenParser()
> lexer = P.makeTokenParser $
>           emptyDef { P.reservedOpNames = operators
>                      }

> startsWith p f k = p >>= \st -> if (not . null) st && (f . head) st
>                                 then return (k st)
>                                 else parserZero


> varexp = try $ startsWith identifier isLower var
> constexp = startsWith identifier isUpper con


> whiteSpace = P.whiteSpace lexer
> lexeme     = P.lexeme lexer
> symbol     = P.symbol lexer            
> natural    = P.natural lexer
> parens     = P.parens lexer
> semi       = P.semi lexer
> identifier = P.identifier lexer
> reserved   = P.reserved lexer
> reservedOp = P.reservedOp lexer

> expr  = whiteSpace >> buildExpressionParser table term
>       <?> "expression"

> term = parens expr
>        <|> varexp
>        <|> constexp
>        <|> negation
>        <?> "expression"

> negation = oper "-" >> (term <|> expr) >>= return . non

> table = [[op "\\/" disj AssocLeft, op "||" disj AssocLeft
>          , op "/\\"  conj AssocLeft, op "&&"  conj AssocLeft]
>         ,[op "=>" implies AssocRight, op "<=" consequences AssocLeft]
>         ,[op "==" equiv AssocLeft, op "=/=" notequiv AssocLeft, op "/=" notequiv AssocLeft]
>         ]

> op s f assoc = Infix (oper s >> return f <?> "operator") assoc
> prefix s f = Prefix (oper s >> return f <?> "operator")
> oper s = try $ symbol s >> return ()

> run p = parse (p >>= \e -> eof >> return e) "" 


> charSep c = spaces >> char c >> spaces


> varExp = varexp >>= \v -> charSep ':' >> expr >>= \e -> return (v,e)

> -- | Parseo de sustituciones. Una sustitución es un conjunto de 
> -- pares $(var,exp)$.
> subst = parens $ varExp `sepBy` (charSep ',')

> parseSubst = run subst

> -- | Parseo de expresiones. 
> parseExp = run expr

> parseConst = run (startsWith identifier isUpper id)
