-- ====================================================================
--                             Test ASTs.
-- ====================================================================

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
-- Should end with fib=55
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


-- A little main function to test that the outputs are what they should be
main = do putStr "Result of `fetch testStore1 \"x\"` (should be 1): "
          putStrLn (show (fetch testStore1 "x"))

          putStr "Result of `fetch testStore2 \"test\"` (should be 0): "
          putStrLn (show (fetch testStore2 "test"))

          putStr "Result of `fetch testStore2 \"y\"` (should be 12): "
          putStrLn (show (fetch testStore2 "y"))

          putStr "Result of `fetch testStore3 \"z\"` (should be 144): "
          putStrLn (show (fetch testStore3 "z"))

          putStr "Result of `fetch testStore4 \"sum\"` (should be 55): "
          putStrLn (show (fetch testStore4 "sum"))

          putStr "Result of `fetch testStore5 \"fib\"` (should be 89): "
          putStrLn (show (fetch testStore5 "fib"))