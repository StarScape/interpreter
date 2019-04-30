-- ======================================
-- |           Parser Code              |
-- ======================================

-- Types needed for parser
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

-- -- expr ::= aexp [“>” aexp]
-- expr = (aexp <&> optional (literal ">" <&> aexp)) `modify` optGreater

-- optGreater (e1, []) = e1
-- optGreater (e1, [(_, e2)]) = Greater e1 e2
-- optGreater _ = error "impossible"

-- -- aexp ::= bexp [“−” aexp]
-- aexp = (bexp <&> optional (literal "-" <&> aexp)) `modify` optMinus

-- optMinus (e1, []) = e1
-- optMinus (e1, [(_, e2)]) = Minus e1 e2
-- optMinus _ = error "impossible"

-- bexp ::= cexp [“*” bexp]
-- bexp = (cexp <&> optional (literal "*" <&> bexp)) `modify` optTimes

-- (((String, Expr), String), [(String, Expr)])
-- optTimes (((_, e1), _), []) = e1
-- optTimes (((_, e1), _), [(_, e2)]) = Times e1 e2
-- optTimes _ = error "impossible"

-- cexp ::= "(" expr ")"
--      | number
--      | variable
-- Parser Variable == [Token] -> (Variable, )
cexp = (literal "(" <&> expr <&> literal ")") `modify` unparenth
       <|> number
       <|> variable

-- Parser (Parser (String, Expr))

-- unparenth :: ((String, Expr), String) -> Expr
unparenth ((_, e), _) = e

-- :t number = Parser Val = [Token] -> (String, [Token])

-- constify :: (Int, [Token]) -> Expr
constify n = Const n

-- :t variable = Parser Variable = [Token] -> (Int, [Token])

-- varify :: (String, [Token]) -> Expr
-- varify (name, _) = Var name
varify name = Var name

-- command ::= unitcom [“;” command]
-- command = (unitcom <&> optional (literal ";" <&> command)) `modify` optSeq

-- -- Generate the Command for sequential statements
-- optSeq (c1, []) = c1
-- optSeq (c1, [(_, c2)]) = Seq c1 c2
-- optSeq _ = error "impossible"

-- unitcom = whilecom <|> (ifcom <|> assign)

-- -- Parser for WHILE statements
-- whilecom = (literal "WHILE" <&> expr <&> literal "DO" <&> command <&> literal "END") `modify` mkWhileNode

-- -- Generate the Command for WHILE statements
-- mkWhileNode :: (((String, Expr), String), Command) -> Command
-- mkWhileNode ((((_, e), _), c), _) = While e c

-- -- Parser for IF statements
-- ifcom = (literal "IF" <&> expr <&> literal "THEN" <&> command <&> literal "ELSE" <&> command <&> literal "ENDIF") `modify` mkIfNode

-- -- Generate the Command for IF statements
-- mkIfNode :: ((((((String, Expr), String), Command), String), Command), String) -> Command
-- mkIfNode ((((((_, e), _), ifc), _), elsec), _) = Cond e ifc elsec

-- -- Parser for ASSIGNMENT statements
-- assign = (variable <&> literal ":=" <&> expr) `modify` mkAssignNode

-- -- Generate the Command for ASSIGNMENT statements
-- mkAssignNode :: ((Variable, String), Expr) -> Command
-- mkAssignNode ((v,_), e) = Assign v e