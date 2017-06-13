eval-exp: comprehensive test with mupl-length function (MUPL call non-closure) [error]
eval-exp: recursive function call (car: contract violation   expected: pair?   given: '()) [error]
eval-exp: complex call expression (car: contract violation   expected: pair?   given: '()) [error]
eval-exp: complex mlet expression (unbound variable during evaluation "x") [error]
eval-exp: evaluating a fun results in a correct closure (fun must be saved in closure) [incorrect answer]
mlet*: sequential bindings (unbound variable during evaluation "x") [error]
mlet*: should not evaluate any MUPL or contain an explicit closure (var-string: contract violation   expected: var?   given: (add (int 1) (var "x"))) [error]
mlet*: empty bindings list (var-string: contract violation   expected: var?   given: (add (int 2) (int 1))) [error]
ifeq: resulting expression uses _x and _y variables (ifeq e1 applied to non-int) [error]
ifeq: resulting expression evaluates to e4 if e1 and e2 evaluate to non-equal integers (ifeq e1 applied to non-int) [error]
ifeq: resulting expression evaluates to e3 if e1 and e2 evaluate to equal integers (ifeq e1 applied to non-int) [error]
ifeq: should not evaluate any MUPL (ifeq e1 applied to non-int) [error]
ifeq: resulting expression evaluates e1 and e2 exactly once (ifeq e1 applied to non-int) [error]
mupl-map: multiple element list (found bad MUPL expression: #<procedure:mupl-map>) [error]
mupl-map: single element list (found bad MUPL expression: #<procedure:mupl-map>) [error]
mupl-map: empty list (found bad MUPL expression: #<procedure:mupl-map>) [error]
mupl-mapAddN: multiple element list (found bad MUPL expression: #<procedure:mupl-map>) [error]
mupl-mapAddN: single element list (found bad MUPL expression: #<procedure:mupl-map>) [error]
mupl-mapAddN: empty list (found bad MUPL expression: #<procedure:mupl-map>) [error]
compute-free-vars: correctly computes free vars [incorrect answer]
compute-free-vars: no free vars case [incorrect answer]
eval-under-env-c: correctly filters closure environments [incorrect answer]