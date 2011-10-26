#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises
from StringIO import StringIO

from kamin1 import *

def test_eval_if_true():
    eq_(evaluate(parse('(if 1 2 3)')), 2)

def test_eval_if_false():
    eq_(evaluate(parse('(if 0 2 3)')), 3)

def test_eval_if_true_dont_eval_alternative():
    eq_(evaluate(parse('(if 1 2 (/ 1 0))')), 2)

def test_print_value():
    eq_(evaluate(parse('(print 5)')), 5)

def test_print_output():
    output = StringIO()
    eq_(evaluate(parse('(print 5)'), output), 5)
    eq_(output.getvalue(), '5\n')

def test_print_expression_value():
    output = StringIO()
    eq_(evaluate(parse('(print (* 5 2))'), output), 10)
    eq_(output.getvalue(), '10\n')

def test_begin_simple_value():
    eq_(evaluate(parse('(begin 1 2 3)')), 3)

def test_begin_expr_value():
    eq_(evaluate(parse('(begin (* 1 10) (* 2 10) (* 3 10))')), 30)

def test_begin_print_log():
    output = StringIO()
    eq_(evaluate(parse('(begin (print 10) (print 20) (print 30))'),
        output), 30)
    eq_(output.getvalue(), '10\n20\n30\n')

@raises(TooManyArguments)
def test_if_too_many_args():
    evaluate(parse('(if 1 2 3 4)'))

@raises(MissingArguments)
def test_if_too_few_args():
    evaluate(parse('(if 1 2)'))

@raises(TooManyArguments)
def test_print_too_many_args():
    evaluate(parse('(print 1 2)'))

@raises(MissingArguments)
def test_print_too_few_args():
    evaluate(parse('(print)'))

@raises(MissingArguments)
def test_begin_empty():
    evaluate(parse('(begin)'))

