# [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

```
$ make build
...
$ ./lc -h
Evaluator of lambda expressions.

Usage  lc              Enter into a repl.
       lc [EXPR]       Evaluate a lambda expression.
       ... | lc        Evaluate a lambda expression from stdin.
       lc -y           Evaluate the y combinator applied to g:
                         ((λf.((λx.(f (x x))) (λx.(f (x x))))) g)
                       and exit after 20 reductions.


  A lambda expression is on one of the following forms:

  1. x       A variable.
  2. (λx.M)  A lambda abstraction (function definition), taking a parameter x, returning the lambda expression M.
  3. (M N)   An application of an expression M to the expression N.

  \ or / can be used instead of λ

  An expression is evaluated by repeated β-reduction (function application) until it is irreducible.

Example
  > ((\z.(z (o i))) (\g.g))
  ((λz.(z (o i))) (λg.g))
  ((λg.g) (o i))
  (o i)

  The following variables are bound globally: ID, CONST, TRUE, FALSE, AND, ZERO, SUCC, ADD, Y.
  Disjunction and multiplaction is left out as an exercise to the reader.

  A global name can be bound with NAME = M and cannot contain any free variables.

Example
  > ONE = (SUCC ZERO)
  ((λn.(λf.(λx.(f ((n f) x))))) (λf.(λx.x)))
  (λf.(λx.(f (((λf.(λx.x)) f) x))))
  (λf.(λx.(f ((λx.x) x))))
  ONE = (λf.(λx.(f x)))
  > TWO = ((ADD ONE) ONE)
  (((λm.(λn.(λf.(λx.((m f) ((n f) x)))))) (λf.(λx.(f x)))) (λf.(λx.(f x))))
  ((λn.(λf.(λx.(((λf.(λx.(f x))) f) ((n f) x))))) (λf.(λx.(f x))))
  (λf.(λx.(((λf.(λx.(f x))) f) (((λf.(λx.(f x))) f) x))))
  (λf.(λx.((λx.(f x)) (((λf.(λx.(f x))) f) x))))
  (λf.(λx.(f (((λf.(λx.(f x))) f) x))))
  (λf.(λx.(f ((λx.(f x)) x))))
  TWO = (λf.(λx.(f (f x))))

```
