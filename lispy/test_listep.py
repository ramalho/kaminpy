#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises

from listep import *

def test_tokenize_atom():
    eq_(tokenize('3'), ['3'])

def test_tokenize_call():
    eq_(tokenize('(+ 2 3)'), ['(','+','2','3',')'])

def test_tokenize_call_with_call():
    eq_(tokenize('(+ 2 (* 3 4))'), ['(','+','2','(','*','3','4',')',')'])

def test_parse_number():
    eq_(parse('3'), 3)

def test_parse_numexp():
    eq_(parse('(+ 2 3)'), ['+', 2, 3])

def test_parse_numexp_inner():
    eq_(parse('(+ 2 (* 3 4))'), ['+', 2, ['*', 3, 4]])

def test_parse_empty_paren():
    eq_(parse('()'), [])

@raises(UnexpectedEndOfInput)
def test_parse_empty():
    parse('')

@raises(UnexpectedRightParen)
def test_parse_right_paren():
    parse(')')

@raises(UnexpectedEndOfInput)
def test_parse_open_paren():
    parse('(')

def test_parse_right_paren2():
    try:
        parse(')')
    except UnexpectedRightParen as urp:
        eq_(str(urp), 'Unexpected )')

def test_eval_int():
    eq_(evaluate(parse('3')), 3)

def test_eval_op():
    import operator
    eq_(evaluate(parse('+')), operator.add)

#def test_eval_numexp():
#    eq_(evaluate(parse('(+ 2 3)')), 5)
