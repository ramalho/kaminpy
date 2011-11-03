#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises

from kamin1 import *

def setup():
    global evaluate
    eva = Evaluator()
    evaluate = lambda exp: eva.evaluate({}, exp)

def test_tokenize_atom():
    eq_(tokenize('3'), ['3'])

def test_tokenize_call():
    eq_(tokenize('(+ 2 3)'), ['(','+','2','3',')'])

def test_tokenize_call_with_call():
    eq_(tokenize('(+ 2 (* 3 4))'), ['(','+','2','(','*','3','4',')',')'])

def test_parse_number():
    eq_(parse_source('3'), 3)

def test_parse_negative_number():
    eq_(parse_source('-3'), -3)

def test_parse_symbol():
    eq_(parse_source('+'), '+')

def test_parse_plus_number_as_symbol():
    eq_(parse_source('+1'), '+1')

def test_parse_numexp():
    eq_(parse_source('(+ 2 3)'), ['+', 2, 3])

def test_parse_numexp_inner():
    eq_(parse_source('(+ 2 (* 3 4))'), ['+', 2, ['*', 3, 4]])

def test_parse_empty_paren():
    eq_(parse_source('()'), [])

@raises(UnexpectedEndOfInput)
def test_parse_empty():
    parse_source('')

@raises(UnexpectedRightParen)
def test_parse_right_paren():
    parse_source(')')

@raises(UnexpectedEndOfInput)
def test_parse_open_paren():
    parse_source('(')

def test_parse_right_paren_detail():
    try:
        parse_source(')')
    except UnexpectedRightParen as exc:
        eq_(str(exc), 'unexpected )')

def test_parse_plus_one():
    eq_(parse_source('(++ 2)'), ['++', 2])

def test_eval_int():
    eq_(evaluate(parse_source('3')), 3)

def test_eval_op():
    assert callable(evaluate(parse_source('+')))

def test_eval_numexp():
    eq_(evaluate(parse_source('(+ 2 3)')), 5)

def test_eval_numexp_inner():
    eq_(evaluate(parse_source('(+ 2 (* 3 4))')), 14)

@raises(InvalidOperator)
def test_eval_no_operator():
    evaluate(parse_source('(2)'))

@raises(InvalidOperator)
def test_eval_no_operator2():
    evaluate(parse_source('(2 3)'))

def test_eval_no_operator_detail():
    try:
        evaluate(parse_source('(2 3)'))
    except InvalidOperator as exc:
        eq_(str(exc), "invalid operator: 2")

def test_eval_multi_add():
    eq_(evaluate(parse_source('(+ 1 2 3 4)')), 10)

def test_eval_sub():
    eq_(evaluate(parse_source('(- 2 3)')), -1)

def test_eval_div():
    eq_(evaluate(parse_source('(/ 6 2)')), 3)

def test_eval_div_returns_int():
    eq_(evaluate(parse_source('(/ 6 4)')), 1)

@raises(ZeroDivisionError)
def test_eval_div_by_zero():
    evaluate(parse_source('(/ 6 0)'))

def test_eval_eq_true():
    eq_(evaluate(parse_source('(= 2 2)')), 1)

def test_eval_eq_false():
    eq_(evaluate(parse_source('(= 2 3)')), 0)

def test_eval_lt_true():
    eq_(evaluate(parse_source('(< 2 3)')), 1)

def test_eval_lt_false():
    eq_(evaluate(parse_source('(< 2 2)')), 0)

def test_eval_lt_false_2():
    eq_(evaluate(parse_source('(< 3 2)')), 0)

def test_eval_gt_true():
    eq_(evaluate(parse_source('(> 4 3)')), 1)

def test_eval_gt_false():
    eq_(evaluate(parse_source('(> 4 4)')), 0)

def test_eval_gt_false_2():
    eq_(evaluate(parse_source('(> 3 4)')), 0)

@raises(UnknownIdentifier)
def test_plus_number():
    print evaluate(parse_source('+1'))

@raises(MissingArguments)
def test_eval_too_few_args():
    evaluate(parse_source('(* 1)'))

@raises(TooManyArguments)
def test_eval_too_many_args():
    evaluate(parse_source('(* 1 2 3)'))
