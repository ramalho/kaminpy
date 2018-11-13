#!/usr/bin/env python
# coding: utf-8

from pytest import raises

from kamin1 import tokenize, parse_source, Evaluator
from kamin1 import UnexpectedEndOfInput, UnexpectedRightParen
from kamin1 import InvalidOperator, MissingArguments, TooManyArguments
from kamin1 import UnknownIdentifier


def setup():
    global evaluate
    eva = Evaluator()
    evaluate = lambda exp: eva.evaluate({}, exp)

def test_tokenize_atom():
    assert tokenize('3') == ['3']

def test_tokenize_call():
    assert tokenize('(+ 2 3)') == ['(','+','2','3',')']

def test_tokenize_call_with_call():
    assert tokenize('(+ 2 (* 3 4))') == ['(','+','2','(','*','3','4',')',')']

def test_parse_number():
    assert parse_source('3') == 3

def test_parse_negative_number():
    assert parse_source('-3') == -3

def test_parse_symbol():
    assert parse_source('+') == '+'

def test_parse_plus_number_as_symbol():
    assert parse_source('+1') == '+1'

def test_parse_numexp():
    assert parse_source('(+ 2 3)') == ['+', 2, 3]

def test_parse_numexp_inner():
    assert parse_source('(+ 2 (* 3 4))') == ['+', 2, ['*', 3, 4]]

def test_parse_empty_paren():
    assert parse_source('()') == []

def test_parse_empty():
    with raises(UnexpectedEndOfInput):
        parse_source('')

def test_parse_right_paren():
    with raises(UnexpectedRightParen):
        parse_source(')')

def test_parse_open_paren():
    with raises(UnexpectedEndOfInput):
        parse_source('(')

def test_parse_right_paren_detail():
    with raises(UnexpectedRightParen) as excinfo:
        parse_source(')')

    assert str(excinfo.value) == 'unexpected )'

def test_parse_plus_one():
    assert parse_source('(++ 2)') == ['++', 2]

def test_eval_int():
    assert evaluate(parse_source('3')) == 3

def test_eval_op():
    assert callable(evaluate(parse_source('+')))

def test_eval_numexp():
    assert evaluate(parse_source('(+ 2 3)')) == 5

def test_eval_numexp_inner():
    assert evaluate(parse_source('(+ 2 (* 3 4))')) == 14

def test_eval_no_operator():
    with raises(InvalidOperator):
        evaluate(parse_source('(2)'))

def test_eval_no_operator2():
    with raises(InvalidOperator):
        evaluate(parse_source('(2 3)'))

def test_eval_no_operator_detail():
    with raises(InvalidOperator) as excinfo:
        evaluate(parse_source('(2 3)'))

    assert str(excinfo.value) == 'invalid operator: 2'

def test_eval_sub():
    assert evaluate(parse_source('(- 2 3)')) == -1

def test_eval_div():
    assert evaluate(parse_source('(/ 6 2)')) == 3

def test_eval_div_returns_int():
    assert evaluate(parse_source('(/ 6 4)')) == 1

def test_eval_div_by_zero():
    with raises(ZeroDivisionError):
        evaluate(parse_source('(/ 6 0)'))

def test_eval_eq_true():
    assert evaluate(parse_source('(= 2 2)')) == 1

def test_eval_eq_false():
    assert evaluate(parse_source('(= 2 3)')) == 0

def test_eval_lt_true():
    assert evaluate(parse_source('(< 2 3)')) == 1

def test_eval_lt_false():
    assert evaluate(parse_source('(< 2 2)')) == 0

def test_eval_lt_false_2():
    assert evaluate(parse_source('(< 3 2)')) == 0

def test_eval_gt_true():
    assert evaluate(parse_source('(> 4 3)')) == 1

def test_eval_gt_false():
    assert evaluate(parse_source('(> 4 4)')) == 0

def test_eval_gt_false_2():
    assert evaluate(parse_source('(> 3 4)')) == 0

def test_plus_number1():
    with raises(UnknownIdentifier):
        evaluate(parse_source('+1'))

def test_eval_too_few_args():
    with raises(MissingArguments):
        evaluate(parse_source('(* 1)'))

def test_eval_too_many_args():
    with raises(TooManyArguments):
        evaluate(parse_source('(* 1 2 3)'))
