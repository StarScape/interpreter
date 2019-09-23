ℓ (pronounced "ehl") is a tiny imperative programming language. It supports exactly 5 constructions:

* Variable assignment
* Subtraction
* Multiplication
* If statements
* While loops

It's a long way from being useful, but it *is* Turing-complete :)

Example program (computes the 11th fibonacci number):

```
n := 11
min2 := 0 (implied)
min1 := 1
fib := 0 (implied)
i := 0 (implied)
while (n-1) > i:
do
  negmin1 := min1 * -1
  fib := min2 - negmin1
  min2 := min1
  min1 := fib
  i := i - (-1)
end
```

The interpreter is written in Haskell, and was done for Dr. Patricia Johann's Functional Programming course at Appalachian State.
