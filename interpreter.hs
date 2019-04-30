import Data.Maybe

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

data Command = Assign Variable Expr
             | Seq Command Command
             | Cond Expr Command Command
             | While Expr Command

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
