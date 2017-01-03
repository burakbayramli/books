#!/usr/bin/env python
"""Test what we gain by compiling eval expressions."""

from math import *
import sys

def formula2func(formula, compile_expression=True):
    formula_compiled = compile(formula, '<string>', 'eval')
    if compile_expression:
        def f(x, y):
            return eval(formula_compiled)
    else:
        def f(x, y):
            return eval(formula)
    return f

def _test():
    # Usage: eval_compile.py xy-expression 0/1 repetitions
    formula = sys.argv[1];  compile_expr = int(sys.argv[2])
    func = formula2func(formula, compile_expr)
    x = 0.1; y = 1.1
    # test efficiency:
    from scitools.misc import timer
    timer(func, args=(x,y), repetitions=int(sys.argv[3]))

_test()

