# [Lambda Calculus](https://en.wikipedia.org/wiki/Lambda_calculus)

```
$ make
((λf.((λx.(f (x x))) (λx.(f (x x))))) g)
  -> ((λx.(g (x x))) (λx.(g (x x))))
  -> (g ((λx.(g (x x))) (λx.(g (x x)))))
  -> (g (g ((λx.(g (x x))) (λx.(g (x x))))))
  -> (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))
  -> (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))
  -> (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))
  -> (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))
  -> (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))
  -> (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))
  -> (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))
  -> (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x))))))))))))))
  -> (g (g (g (g (g (g (g (g (g (g (g ((λx.(g (x x))) (λx.(g (x x)))))))))))))))
...
```
