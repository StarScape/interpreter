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
-- (This is the actual function called in the pipeline)
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

-- The actual parser function used in our pipeline
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

-- ====================================
-- |                Run               |
-- |           (Main Program)         |
-- ====================================
-- run s = interpret (mainParser (lexer s))
run :: String -> Store -> Store
run = interpret . mainParser . lexer





-- ====================================
-- |              Tests               |
-- ====================================

-- I copy-pasted results from GHCI below each test

testStr1 = "X := 10;\n" ++
           "Z := 20;\n" ++
           "WHILE X > 1 DO\n" ++
             "X := X - 1;\n" ++
             "Z := Z - 1\n" ++ 
           "END"

testTokens1 = [Ident "X",Symbol ":=",Number 10,Symbol ";",Ident "Z",Symbol ":=",Number 20,Symbol ";",Symbol "WHILE",Ident "X",Symbol ">",Number 1,Symbol "DO",Ident "X",Symbol ":=",Ident "X",Symbol "-",Number 1,Symbol ";",Ident "Z",Symbol ":=",Ident "Z",Symbol "-",Number 1,Symbol "END"]

-- GHCI
-- *Main> (run testStr1 initial) "X"
-- 1
-- *Main> (run testStr1 initial) "Z"
-- 11
-- *Main> lexer testStr1
-- [Ident "X",Symbol ":=",Number 10,Symbol ";",Ident "Z",Symbol ":=",Number 20,Symbol ";",Symbol "WHILE",Ident "X",Symbol ">",Number 1,Symbol "DO",Ident "X",Symbol ":=",Ident "X",Symbol "-",Number 1,Symbol ";",Ident "Z",Symbol ":=",Ident "Z",Symbol "-",Number 1,Symbol "END"]
-- *Main> mainParser testTokens1
-- Seq (Assign "X" (Const 10)) (Seq (Assign "Z" (Const 20)) (While (Greater (Var "X") (Const 1)) (Seq (Assign "X" (Minus (Var "X") (Const 1))) (Assign "Z" (Minus (Var "Z") (Const 1))))))

-- Sums numbers from 1 to 10
testStr2 = "X := 10;\n" ++
           "SUM := 0;\n" ++
           "NEGONE := 0;" ++
           "NEGONE := NEGONE - 1;" ++
           "WHILE X > 0 DO\n" ++
             "NEGX := X * NEGONE;\n" ++
             "SUM := SUM - NEGX;\n" ++
             "X := X - 1\n" ++
           "END" 

testTokens2 = [Ident "X",Symbol ":=",Number 10,Symbol ";",Ident "SUM",Symbol ":=",Number 0,Symbol ";",Ident "NEGONE",Symbol ":=",Number 0,Symbol ";",Ident "NEGONE",Symbol ":=",Ident "NEGONE",Symbol "-",Number 1,Symbol ";",Symbol "WHILE",Ident "X",Symbol ">",Number 0,Symbol "DO",Ident "NEGX",Symbol ":=",Ident "X",Symbol "*",Ident "NEGONE",Symbol ";",Ident "SUM",Symbol ":=",Ident "SUM",Symbol "-",Ident "NEGX",Symbol ";",Ident "X",Symbol ":=",Ident "X",Symbol "-",Number 1,Symbol "END"]

-- GHCI
-- *Main> (run testStr2 initial) "X"
-- 0
-- *Main> (run testStr2 initial) "SUM"
-- 55
-- *Main> (run testStr2 initial) "NEGONE"
-- -1
-- *Main> (run testStr2 initial) "NEGX"
-- -1
-- *Main> lexer testStr2
-- [Ident "X",Symbol ":=",Number 10,Symbol ";",Ident "SUM",Symbol ":=",Number 0,Symbol ";",Ident "NEGONE",Symbol ":=",Number 0,Symbol ";",Ident "NEGONE",Symbol ":=",Ident "NEGONE",Symbol "-",Number 1,Symbol ";",Symbol "WHILE",Ident "X",Symbol ">",Number 0,Symbol "DO",Ident "NEGX",Symbol ":=",Ident "X",Symbol "*",Ident "NEGONE",Symbol ";",Ident "SUM",Symbol ":=",Ident "SUM",Symbol "-",Ident "NEGX",Symbol ";",Ident "X",Symbol ":=",Ident "X",Symbol "-",Number 1,Symbol "END"]
-- *Main>
-- *Main> mainParser testTokens2
-- Seq (Assign "X" (Const 10)) (Seq (Assign "SUM" (Const 0)) (Seq (Assign "NEGONE" (Const 0)) (Seq (Assign "NEGONE" (Minus (Var "NEGONE") (Const 1))) (While (Greater (Var "X") (Const 0)) (Seq (Assign "NEGX" (Times (Var "X") (Var "NEGONE"))) (Seq (Assign "SUM" (Minus (Var "SUM") (Var "NEGX"))) (Assign "X" (Minus (Var "X") (Const 1)))))))))

testStr3 = "Y := 12;" ++
           "IF Y > 13 THEN\n" ++
             "TEST := 1;\n" ++
             "Y := 1\n" ++
           "ELSE\n" ++
             "TEST := 2;\n" ++
             "Y := 2\n" ++
           "ENDIF"
-- Should end with test=0, y=3

testTokens3 = [Ident "Y",Symbol ":=",Number 12,Symbol ";",Symbol "IF",Ident "Y",Symbol ">",Number 13,Symbol "THEN",Ident "TEST",Symbol ":=",Number 1,Symbol ";",Ident "Y",Symbol ":=",Number 1,Symbol "ELSE",Ident "TEST",Symbol ":=",Number 2,Symbol ";",Ident "Y",Symbol ":=",Number 2,Symbol "ENDIF"]

-- GHCI
-- *Main> (run testStr3 initial) "TEST"
-- 2
-- *Main> (run testStr3 initial) "Y"
-- 2
-- *Main> lexer testStr3
-- [Ident "Y",Symbol ":=",Number 12,Symbol ";",Symbol "IF",Ident "Y",Symbol ">",Number 13,Symbol "THEN",Ident "TEST",Symbol ":=",Number 1,Symbol ";",Ident "Y",Symbol ":=",Number 1,Symbol "ELSE",Ident "TEST",Symbol ":=",Number 2,Symbol ";",Ident "Y",Symbol ":=",Number 2,Symbol "ENDIF"]
-- *Main> mainParser testTokens3
-- Seq (Assign "Y" (Const 12)) (Cond (Greater (Var "Y") (Const 13)) (Seq (Assign "TEST" (Const 1)) (Assign "Y" (Const 1))) (Seq (Assign "TEST" (Const 2)) (Assign "Y" (Const 2))))
-- *Main> 

-- Compute 10th Fibnonacci Number (Stored in FIB)
testStr4 = "N := 11;\n" ++
           "MIN2 := 0;\n" ++
           "MIN1 := 1;\n" ++
           "FIB := 0;\n" ++
           "I := 0;\n" ++
           "WHILE (N - 2) > I DO\n" ++
             "NEGMIN1 := MIN1 * (0-1);\n" ++
             "FIB := MIN2 - NEGMIN1;\n" ++
             "MIN2 := MIN1;\n" ++
             "MIN1 := FIB;\n" ++
             "I := I - (0-1)\n" ++
           "END"

testTokens4 = [Ident "N",Symbol ":=",Number 11,Symbol ";",Ident "MIN2",Symbol ":=",Number 0,Symbol ";",Ident "MIN1",Symbol ":=",Number 1,Symbol ";",Ident "FIB",Symbol ":=",Number 0,Symbol ";",Ident "I",Symbol ":=",Number 0,Symbol ";",Symbol "WHILE",Symbol "(",Ident "N",Symbol "-",Number 2,Symbol ")",Symbol ">",Ident "I",Symbol "DO",Ident "NEGMIN1",Symbol ":=",Ident "MIN1",Symbol "*",Symbol "(",Number 0,Symbol "-",Number 1,Symbol ")",Symbol ";",Ident "FIB",Symbol ":=",Ident "MIN2",Symbol "-",Ident "NEGMIN1",Symbol ";",Ident "MIN2",Symbol ":=",Ident "MIN1",Symbol ";",Ident "MIN1",Symbol ":=",Ident "FIB",Symbol ";",Ident "I",Symbol ":=",Ident "I",Symbol "-",Symbol "(",Number 0,Symbol "-",Number 1,Symbol ")",Symbol "END"]


-- GHCI
-- *Main> (run testStr4 initial) "FIB"
-- 55
-- *Main> (run testStr4 initial) "N"
-- 11
-- *Main> (run testStr4 initial) "MIN2"
-- 34
-- *Main> (run testStr4 initial) "MIN1"
-- 55
-- *Main> (run testStr4 initial) "I"
-- 9
-- *Main> (run testStr4 initial) "NEGMIN1"
-- -34
-- *Main> lexer testStr4
-- [Ident "N",Symbol ":=",Number 11,Symbol ";",Ident "MIN2",Symbol ":=",Number 0,Symbol ";",Ident "MIN1",Symbol ":=",Number 1,Symbol ";",Ident "FIB",Symbol ":=",Number 0,Symbol ";",Ident "I",Symbol ":=",Number 0,Symbol ";",Symbol "WHILE",Symbol "(",Ident "N",Symbol "-",Number 2,Symbol ")",Symbol ">",Ident "I",Symbol "DO",Ident "NEGMIN1",Symbol ":=",Ident "MIN1",Symbol "*",Symbol "(",Number 0,Symbol "-",Number 1,Symbol ")",Symbol ";",Ident "FIB",Symbol ":=",Ident "MIN2",Symbol "-",Ident "NEGMIN1",Symbol ";",Ident "MIN2",Symbol ":=",Ident "MIN1",Symbol ";",Ident "MIN1",Symbol ":=",Ident "FIB",Symbol ";",Ident "I",Symbol ":=",Ident "I",Symbol "-",Symbol "(",Number 0,Symbol "-",Number 1,Symbol ")",Symbol "END"]
-- *Main> mainParser testTokens4
-- Seq (Assign "N" (Const 11)) (Seq (Assign "MIN2" (Const 0)) (Seq (Assign "MIN1" (Const 1)) (Seq (Assign "FIB" (Const 0)) (Seq (Assign "I" (Const 0)) (While (Greater (Minus (Var "N") (Const 2)) (Var "I")) (Seq (Assign "NEGMIN1" (Times (Var "MIN1") (Minus (Const 0) (Const 1)))) (Seq (Assign "FIB" (Minus (Var "MIN2") (Var "NEGMIN1"))) (Seq (Assign "MIN2" (Var "MIN1")) (Seq (Assign "MIN1" (Var "FIB")) (Assign "I" (Minus (Var "I") (Minus (Const 0) (Const 1)))))))))))))

testStr5 = "X := 3; Y := 2; Z := 1"
testTokens5 = [Ident "X",Symbol ":=",Number 3,Symbol ";",Ident "Y",Symbol ":=",Number 2,Symbol ";",Ident "Z",Symbol ":=",Number 1]

-- GHCI
-- *Main> (run testStr5 initial) "X"
-- 3
-- *Main> (run testStr5 initial) "Y"
-- 2
-- *Main> (run testStr5 initial) "Z"
-- 1
-- *Main> lexer testStr5
-- [Ident "X",Symbol ":=",Number 3,Symbol ";",Ident "Y",Symbol ":=",Number 2,Symbol ";",Ident "Z",Symbol ":=",Number 1]
-- *Main> mainParser testTokens5
-- Seq (Assign "X" (Const 3)) (Seq (Assign "Y" (Const 2)) (Assign "Z" (Const 1)))
-- *Main> 

-- ====================================================================
--                             Test ASTs.
-- ====================================================================

-- GHCI results at very bottom

-- TestProg 1:
-- x := 10
-- while x > 1 do
--   x := x - 1
-- end
-- Should end with x=1
testAst1 = (Seq (Assign "x" (Const 10)) (While (Greater (Var "x") (Const 1)) (Assign "x" (Minus (Var "x") (Const 1)))))
testStore1 = interpret testAst1 initial

-- TestProg 2:
-- y := 12
-- if y > 13
--   test := 1
-- else
--   test := 0
-- Should end with test=0, y=12
testAst2 = (Seq (Assign "y" (Const 12)) (Cond (Greater (Var "y") (Const 13)) (Assign "test" (Const 1)) (Assign "test" (Const 0))))
testStore2 = interpret testAst2 initial

-- TestProg 3 (evaluated with store from TestProg 2):
-- if 1 > test
--   z := 12 * 12
-- else
--   z := 0
-- Should end with z = 12
testAst3 = (Cond (Greater (Const 1) (Var "test")) (Assign "z" (Times (Const 12) (Const 12))) (Assign "z" (Const 0)))
testStore3 = interpret testAst3 testStore2

-- TestProg 4 (sums ints from 1 to 10):
-- x := 10
-- sum := 0 (implied since vals in store default to 0)
-- while x > 0:
-- do
--   negx := x * -1
--   sum := sum - negx
--   x := x - 1
-- end
-- Should end with sum=55, x=0
testAst4 = (Seq 
  (Assign "x" (Const 10))
  (While (Greater (Var "x") (Const 0))
    (Seq
      (Assign "negx" (Times (Var "x") (Const (-1))))
      (Seq
        (Assign "sum" (Minus (Var "sum") (Var "negx")))
        (Assign "x" (Minus (Var "x") (Const 1)))))))
testStore4 = interpret testAst4 initial

-- TestProg 5 (compute the 11th fibonacci number using a loop):
-- n := 11
-- min2 := 0 (implied)
-- min1 := 1
-- fib := 0 (implied)
-- i := 0 (implied)

-- while (n-1) > i:
-- do
--   negmin1 := min1 * -1
--   fib := min2 - negmin1
--   min2 := min1
--   min1 := fib
--   i := i - (-1)
-- end
-- Should end with fib=89
testAst5 = (Seq (Assign "n" (Const 11))
              (Seq (Assign "min1" (Const 1))
                (While
                  (Greater (Minus (Var "n") (Const 1)) (Var "i")) 
                  (Seq
                    (Assign "negmin1" (Times (Var "min1") (Const (-1))))
                    (Seq
                      (Assign "fib" (Minus (Var "min2") (Var "negmin1")))
                      (Seq
                        (Assign "min2" (Var "min1"))
                        (Seq
                          (Assign "min1" (Var "fib"))
                          (Assign "i" (Minus (Var "i") (Const (-1)))))))))))
testStore5 = interpret testAst5 initial

-- GHCI
-- *Main> testStore1 "x"
-- 1
-- *Main> testStore2 "y"
-- 12
-- *Main> testStore2 "test"
-- 0
-- *Main> testStore3 "test"
-- 0
-- *Main> testStore3 "z"
-- 144
-- *Main> testStore4 "x"
-- 0
-- *Main> testStore4 "negx"
-- -1
-- *Main> testStore4 "sum"
-- 55
-- *Main> testStore5 "n"
-- 11
-- *Main> testStore5 "min1"
-- 89
-- *Main> testStore5 "i"
-- 10
-- *Main> testStore5 "negmin1"
-- -55
-- *Main> testStore5 "min2"
-- 55
-- *Main> testStore5 "fib"
-- 89
-- *Main> 
