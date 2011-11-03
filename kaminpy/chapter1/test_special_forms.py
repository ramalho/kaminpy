#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises
import sys

from kamin1 import *

def setup():
    global evaluate
    eva = Evaluator()
    evaluate = lambda exp: eva.evaluate({}, exp)

def test_eval_if_true():
    eq_(evaluate(parse_source('(if 1 2 3)')), 2)

def test_eval_if_false():
    eq_(evaluate(parse_source('(if 0 2 3)')), 3)

def test_eval_if_true_dont_eval_alternative():
    eq_(evaluate(parse_source('(if 1 2 (/ 1 0))')), 2)

def test_print_value():
    eq_(evaluate(parse_source('(print 5)')), 5)

def test_print_output():
    eq_(evaluate(parse_source('(print 5)')), 5)
    eq_(sys.stdout.getvalue(), '5\n')

def test_print_expression_value():
    eq_(evaluate(parse_source('(print (* 5 2))')), 10)
    eq_(sys.stdout.getvalue(), '10\n')

def test_begin_simple_value():
    eq_(evaluate(parse_source('(begin 1 2 3)')), 3)

def test_begin_expr_value():
    eq_(evaluate(parse_source('(begin (* 1 10) (* 2 10) (* 3 10))')), 30)

def test_begin_print_log():
    eq_(evaluate(parse_source('(begin (print 10) (print 20) (print 30))')), 30)
    eq_(sys.stdout.getvalue(), '10\n20\n30\n')

def test_check_args_ok():
    def f(a, b): pass
    check_args(f, [1, 2])

@raises(MissingArguments)
def test_check_args_missing():
    def f(a, b): pass
    check_args(f, [1])

def test_check_args_skip_params():
    def f(self, a, b): pass
    check_args(f, [1, 2], ['self'])

@raises(TooManyArguments)
def test_check_args_missing():
    def f(a, b): pass
    check_args(f, [1, 2, 3])

@raises(TooManyArguments)
def test_if_too_many_args():
    evaluate(parse_source('(if 1 2 3 4)'))

@raises(MissingArguments)
def test_if_too_few_args():
    evaluate(parse_source('(if 1 2)'))

@raises(TooManyArguments)
def test_print_too_many_args():
    evaluate(parse_source('(print 1 2)'))

@raises(MissingArguments)
def test_print_too_few_args():
    evaluate(parse_source('(print)'))

@raises(MissingArguments)
def test_begin_empty():
    evaluate(parse_source('(begin)'))

def test_eval_local_symbol():
    eva = Evaluator()
    local_env = {'x':3}
    eq_(eva.evaluate(local_env, parse_source('x')), 3)

def test_eval_local_set_const():
    eva = Evaluator()
    local_env = {'x': 3}
    expr = parse_source('(set x 7)')
    eq_(eva.evaluate(local_env, expr), 7)
    eq_(local_env['x'], 7)

def test_eval_local_set_expr():
    eva = Evaluator()
    local_env = {'x': 3}
    expr = parse_source('(set x (+ 4 7))')
    eq_(eva.evaluate(local_env, expr), 11)
    eq_(local_env['x'], 11)

def test_eval_global_set():
    eva = Evaluator()
    local_env = {}
    expr = parse_source('(set x 7)')
    eq_(eva.evaluate(local_env, expr), 7)
    eq_(len(local_env), 0)
    eq_(eva.get(local_env, 'x'), 7)

def test_eval_while():
    eva = Evaluator()
    local_env = {}
    source = """
        (begin
            (set n 1)
            (while n
                (begin
                    (print n)
                    (set n 0)))
            (print n))
    """
    expr = parse_source(source)
    eva.evaluate(local_env, expr)
    eq_(sys.stdout.getvalue(), '1\n0\n')

def test_eval_while_2():
    eva = Evaluator()
    local_env = {}
    source = """
        (begin
            (set n 3)
            (while n
                (begin
                    (print n)
                    (set n (- n 1)))))
    """
    expr = parse_source(source)
    eva.evaluate(local_env, expr)
    eq_(sys.stdout.getvalue(), '3\n2\n1\n')
