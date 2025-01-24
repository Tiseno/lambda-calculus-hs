# [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

```
$ make build
...
$ ./lc -h
Evaluator of lambda calculus expressions

Usage  lc              Enter into a repl
       lc [EXPR]       Evaluate a lambda expression
       ... | lc        Evaluate a lambda expression from stdin
       lc -y           Evaluate the y combinator applied to g: ((λf.((λx.(f (x x))) (λx.(f (x x))))) g)
                       and exit after 20 reductions


  A lambda expression is on one of the following forms:

  1. x       A variable
  2. (λx.M)  A lambda abstraction (function definition), taking a parameter x, returning the lambda expression M
  3. (M N)   An application of an expression M to the expression N

  \ or / can be used instead of λ

  An expression is evaluated by repeated β-reduction (function application) until it is irreducible

Example
  > ((\z.(z (o i))) (\g.g))
  ((λz.(z (o i))) (λg.g))
  ((λg.g) (o i))
  (o i)

$ ./lc -y
((λf.((λx.(f (x x))) (λx.(f (x x))))) g)
((λx.(g (x x))) (λx.(g (x x))))
(g ((λx.(g (x x))) (λx.(g (x x)))))
(g (g ((λx.(g (x x))) (λx.(g (x x))))))
(g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))
(g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))
(g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))
(g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))
(g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))
(g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))
(g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))
(g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))))))))))
(g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))))))))))
```
