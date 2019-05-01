import Data.Maybe
import Data.Char

-- Parser combinators
(parser1 <|> parser2) s =
   let parser2IfNothing Nothing = parser2 s
       parser2IfNothing x       = x
   in
     parser2IfNothing (parser1 s)

(parser `modify` f) s =
   let modResult Nothing      = Nothing
       modResult (Just (x,y)) = Just (f x,y)
   in
     modResult (parser s)

(parser1 <&> parser2) s =
   let parser2After Nothing      = Nothing
       parser2After (Just (x,s)) = (parser2 `modify` (\y -> (x,y))) s
   in
     parser2After (parser1 s)

emptyseq s = Just ([],s)

optional pr = (pr `modify` (consonto [])) <|> emptyseq
               where consonto [] x = [x]
-- /end parser combinators


-- Type declarations for our AST
type Variable = String
type Val = Int
type Store = Variable -> Val

data Expr = Const Val
          | Var Variable
          | Minus Expr Expr
          | Times Expr Expr
          | Greater Expr Expr
          deriving Show

data Command = Assign Variable Expr
             | Seq Command Command
             | Cond Expr Command Command
             | While Expr Command
             deriving Show

-- Initial store for our program (everything is 0)
initial :: Store
initial _ = 0

-- Get value of var from store `s`
fetch :: Store -> Variable -> Val
fetch = id

-- Update store `s` with new value of `var`
update :: Store -> Variable -> Val -> Store
update s var val x | x == var = val
                   | otherwise = s x

-- Takes a 1/0 val, 2 Store->Store functions, and an input store.
-- If first arg is 1, apply input store to first function
-- If first arg is 0, apply input store to second function
-- (Used for conditionals and whiles)
switch :: Val -> (Store -> Store) -> (Store -> Store) -> Store -> Store
switch 1 ifTrue ifFalse = ifTrue
switch 0 ifTrue ifFalse = ifFalse

-- Evaluate expression `e` relative to store `s`
eval :: Expr -> Store -> Val
eval (Const x) _       = x
eval (Var v) s         = fetch s v
eval (Minus e1 e2) s   = eval e1 s - eval e2 s
eval (Times e1 e2) s   = eval e1 s * eval e2 s 
eval (Greater e1 e2) s | bool      = 1
                       | otherwise = 0
                       where bool  = eval e1 s > eval e2 s

-- Interprets a command relative to store `s`, returns new store
interpret :: Command -> Store -> Store
interpret (Assign v e) s = update s v (eval e s)
interpret (Seq c1 c2)  s = interpret c2 (interpret c1 s)
interpret (Cond e ifc elsec) s = switch (eval e s) (interpret ifc) (interpret elsec) s
interpret (While e c) s = switch (eval e s) (interpret (Seq c (While e c))) id s

-- ====================================
-- |            Parser Code           |
-- ====================================
data Token = Ident String
           | Number Int
           | Symbol String
           deriving Show

type Parser a = [Token] -> Maybe (a, [Token])

-- [Token] -> Maybe (Int, [Token])
number   :: Parser Val
number (Number n : s) = Just (n, s)
number _              = Nothing

-- [Token] -> Maybe (String, [Token])
variable :: Parser Variable
variable (Ident i : is) = Just (i, is)
variable _              = Nothing

-- String -> [Token] -> Maybe (String, [Token])
literal :: String -> Parser String
literal s1 (Symbol s2 : s) | s1 == s2  = Just (s2, s)
                           | otherwise = Nothing
literal _ _                            = Nothing

-- expr ::= aexp [“>” aexp]
expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater
  where
    -- Generate Expr for greater than
    optGreater (e1, []) = e1
    optGreater (e1, [(_, e2)]) = Greater e1 e2
    optGreater _ = error "impossible"

-- aexp ::= bexp [“−” aexp]
aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optMinus
  where
    -- Generate Expr for subtraction
    optMinus (e1, []) = e1
    optMinus (e1, [(_, e2)]) = Minus e1 e2
    optMinus _ = error "impossible"

-- bexp ::= cexp [“*” bexp]
bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optTimes
  where
    -- Generate Expr for multiplication
    optTimes (e1, []) = e1
    optTimes (e1, [(_, e2)]) = Times e1 e2
    optTimes _ = error "impossible"

-- cexp ::= "(" expr ")"
--      | number
--      | variable
cexp = (literal "(" <&> expr <&> literal ")") `modify` unparenth
        <|> (number `modify` Const)
        <|> (variable `modify` Var)
        where unparenth ((_, e), _) = e

-- command ::= unitcom [“;” command]
command = (unitcom <&> optional (literal ";" <&> command)) `modify` optSeq
  where 
    -- Generate the Command for sequential statements
    optSeq (c1, []) = c1
    optSeq (c1, [(_, c2)]) = Seq c1 c2
    optSeq _ = error "impossible"

unitcom = whilecom <|> (ifcom <|> assign)

-- Parser for WHILE statements
whilecom = (literal "WHILE" <&> expr <&> literal "DO" <&> command <&> literal "END") `modify` mkWhileNode
  where
    -- Generate the Command for WHILE statements
    mkWhileNode :: ((((String, Expr), String), Command), String) -> Command
    mkWhileNode ((((_, e), _), c), _) = While e c

-- Parser for IF statements
ifcom = (literal "IF" <&> expr <&> literal "THEN" <&> command <&> literal "ELSE" <&> command <&> literal "ENDIF") `modify` mkIfNode
  where
    -- Generate the Command for IF statements
    mkIfNode :: ((((((String, Expr), String), Command), String), Command), String) -> Command
    mkIfNode ((((((_, e), _), ifc), _), elsec), _) = Cond e ifc elsec

-- Parser for ASSIGNMENT statements
assign = (variable <&> literal ":=" <&> expr) `modify` mkAssignNode
  where
    -- Generate the Command for ASSIGNMENT statements
    mkAssignNode :: ((Variable, String), Expr) -> Command
    mkAssignNode ((v,_), e) = Assign v e

lit :: Token -> String
lit (Ident s) = s ++ " "
lit (Symbol s) = s ++ " "
lit (Number s) = show s ++ " "

report :: Maybe (a, [Token]) -> a
report Nothing = error "Parse error"
report (Just(c, [])) = c
report (Just(c, xs)) = error (stringwith
                                ("Syntax error \n Unparsed:-\n",
                                 " ",
                                 "\n")
                                 (map lit xs))

stringwith (front, sep, back) ls =
  let sepback [] = back
      sepback [a] = a ++ back
      sepback (a:xs) = a ++ sep ++ sepback xs
  in
    front ++ sepback ls

mainParser :: [Token] -> Command
mainParser = report . command

-- ====================================
-- |            Lexer Code            |
-- ====================================

-- Check if keyword
keyword :: String -> Bool
keyword s = s `elem` ["IF", "THEN", "ELSE", "ENDIF", "WHILE", "DO", "END"]

-- Convert string to appropriate token type
-- This allows us to share some code between keywords (IF/WHILE etc) and varnames
keycheck :: String -> Token
keycheck s | keyword s = Symbol s
           | otherwise = Ident s

-- It is a letter, number, ', or _
letDigEtc :: Char -> Bool
letDigEtc c = isLetter c || isDigit c || c == '\'' || c == '_'

-- Is it whitespace?
layout :: Char -> Bool
layout c = c == ' ' || c == '\t' || c == '\n'

-- Is is one of our special symbols?
symbolchar :: Char -> Bool
symbolchar c = c `elem` "*->:=;"

-- Convert char to number 
intOfDigit :: Char -> Int
intOfDigit c = ord c - 48

-- Top-level lexical analyzer function
-- Takes an L string as input and tokenizes it to be passed on to the parser
-- (I rewrote this with guards because I found the if statements hard to read)
lexer :: String -> [Token]
lexer [] = []
lexer (a:x) | layout a     = lexer x
            | a == '('     = Symbol "(" : (lexer x)
            | a == ')'     = Symbol ")" : (lexer x)
            | isLetter a   = getword [a] x
            | isDigit a    = getnum (intOfDigit a) x
            | symbolchar a = getsymbol [a] x
            | otherwise    = error ("Lexical error : unrecognized token " ++ (a:x))

-- Tokenize a varname/keyword
-- (Again, I rewrote this with guards as a personal preference)
getword :: String -> String -> [Token]
getword l [] = [keycheck (reverse l)]
getword l (a:x) | letDigEtc a = getword (a:l) x
                | otherwise   = (keycheck (reverse l)) : (lexer (a:x))

-- Tokenize a symbol
getsymbol :: String -> String -> [Token]
getsymbol l [] = [Symbol (reverse l)]
getsymbol l (a:x) | symbolchar a = getsymbol (a:l) x
                  | otherwise    = (Symbol (reverse l)) : (lexer (a:x))

-- Tokenize an integer literal
getnum :: Int -> String -> [Token]
getnum n [] = [Number n]
getnum n (a:x) | isDigit a = getnum (n*10 + (intOfDigit a)) x
               | otherwise = (Number n) : (lexer (a:x))
