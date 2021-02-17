## Description

ℓ (pronounced "ehl") is a tiny imperative programming language. It supports exactly 5 constructions:

* Variable assignment (numbers only)
* Subtraction
* Multiplication
* If statements
* While loops

...and nothing else.

If that sounds difficult to program in, you're correct! The point is not so much the design of the language, but to learn about language implementation and functional programming.

And it *is* Turing-complete, so the sky is the limit...technically...

#### Example program (computes the 11th fibonacci number):
Note that *every* variable, if not previously assigned, will automatically resolve to `0`, so assignments to zero are simply there for clarity.

```
n := 11
min2 := 0
min1 := 1
fib := 0
i := 0
while (n-1) > i:
do
  negmin1 := min1 * -1
  fib := min2 - negmin1
  min2 := min1
  min1 := fib
  i := i - (-1)
end
```

The interpreter is written in Haskell, and was done as a final project for Dr. Patricia Johann's Functional Programming course at Appalachian State University.
