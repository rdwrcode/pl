eval-exp: comprehensive test with mupl-length function (MUPL call non-closure) [error]
eval-exp: isaunit evaluates its expression ((isaunit (fst (apair (aunit) (aunit)))) should evaluate to (int 1) but evaluated to (int 0)) [incorrect answer]
eval-exp: isaunit works correctly on values ((isaunit (aunit)) should evaluate to (int 1) but evaluated to (int 0)) [incorrect answer]
eval-exp: complex fst/snd operations ((fst (apair (add (int 1) (int 3)) (aunit))) should evaluate to (int 4) but evaluated to (add (int 1) (int 3))) [incorrect answer]
eval-exp: recursive function call (car: contract violation   expected: pair?   given: '()) [error]
eval-exp: complex call expression (car: contract violation   expected: pair?   given: '()) [error]
eval-exp: complex mlet expression (unbound variable during evaluation "x") [error]
eval-exp: evaluating a fun results in a correct closure (fun must be saved in closure) [incorrect answer]
ifaunit: first expression is aunit ('(ifaunit (fst (apair (aunit) (int 0))) (int 4) (int 10)) should result in MUPL that evaluates to (int 4) but resulted in (int 10)) [incorrect answer]
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
