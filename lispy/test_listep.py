#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_

from listep import *

def test_tokenize_atom():
    eq_(tokenize('3'), ['3'])

def test_tokenize_call():
    eq_(tokenize('(+ 2 3)'), ['(','+','2','3',')'])

def test_tokenize_call_with_call():
    eq_(tokenize('(+ 2 (* 3 4))'), ['(','+','2','(','*','3','4',')',')'])

def test_parse_number():
    eq_(parse('3'), 3)
