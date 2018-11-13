#!/usr/bin/env python
# coding: utf-8

from pytest import raises

import sys

from kamin1 import *

def setup():
    global evaluate
    eva = Evaluator()
    evaluate = lambda exp: eva.evaluate({}, exp)

def test_eval_if_true():
    assert evaluate(parse_source('(if 1 2 3)')) == 2

def test_eval_if_false():
    assert evaluate(parse_source('(if 0 2 3)')) == 3

def test_eval_if_true_dont_eval_alternative():
    assert evaluate(parse_source('(if 1 2 (/ 1 0))')) == 2

def test_print_value():
    assert evaluate(parse_source('(print 5)')) == 5

def test_print_output(capsys):
    assert evaluate(parse_source('(print 5)')) == 5
    captured = capsys.readouterr()
    assert captured.out == '5\n'

def test_print_expression_value(capsys):
    assert evaluate(parse_source('(print (* 5 2))')) == 10
    captured = capsys.readouterr()
    assert captured.out == '10\n'

def test_begin_simple_value():
    assert evaluate(parse_source('(begin 1 2 3)')) == 3

def test_begin_expr_value():
    assert evaluate(parse_source('(begin (* 1 10) (* 2 10) (* 3 10))')) == 30

def test_begin_print_log(capsys):
    assert evaluate(parse_source('(begin (print 10) (print 20) (print 30))')) == 30
    captured = capsys.readouterr()
    assert captured.out == '10\n20\n30\n'

def test_check_args_ok():
    def f(a, b): pass
    check_args(f, [1, 2])

def test_check_args_missing():
    with raises(MissingArguments):
        def f(a, b): pass
        check_args(f, [1])

def test_check_args_skip_params():
    def f(self, a, b): pass
    check_args(f, [1, 2], ['self'])

def test_too_many_args():
    with raises(TooManyArguments):
        def f(a, b): pass
        check_args(f, [1, 2, 3])

def test_if_too_many_args():
    with raises(TooManyArguments):
        evaluate(parse_source('(if 1 2 3 4)'))

def test_if_too_few_args():
    with raises(MissingArguments):
        evaluate(parse_source('(if 1 2)'))

def test_print_too_many_args():
    with raises(TooManyArguments):
        evaluate(parse_source('(print 1 2)'))

def test_print_too_few_args():
    with raises(MissingArguments):
        evaluate(parse_source('(print)'))

def test_begin_empty():
    with raises(MissingArguments):
        evaluate(parse_source('(begin)'))

def test_eval_local_symbol():
    eva = Evaluator()
    local_env = {'x':3}
    assert eva.evaluate(local_env, parse_source('x')) == 3

def test_eval_local_set_const():
    eva = Evaluator()
    local_env = {'x': 3}
    expr = parse_source('(set x 7)')
    assert eva.evaluate(local_env, expr) == 7
    assert local_env['x'] == 7

def test_eval_local_set_expr():
    eva = Evaluator()
    local_env = {'x': 3}
    expr = parse_source('(set x (+ 4 7))')
    assert eva.evaluate(local_env, expr) == 11
    assert local_env['x'] == 11

def test_eval_global_set():
    eva = Evaluator()
    local_env = {}
    expr = parse_source('(set x 7)')
    assert eva.evaluate(local_env, expr) == 7
    assert len(local_env) == 0
    assert eva.get(local_env, 'x') == 7

def test_eval_while(capsys):
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
    captured = capsys.readouterr()
    assert captured.out == '1\n0\n'


def test_eval_while_2(capsys):
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
    captured = capsys.readouterr()
    assert captured.out == '3\n2\n1\n'
