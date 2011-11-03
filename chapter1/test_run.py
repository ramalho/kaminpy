#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises
import sys

from kamin1 import Evaluator

from StringIO import StringIO

def test_expr_no_output():
    eva = Evaluator()
    src = StringIO('(+ 2 3)')
    eva.run(src)
    eq_(sys.stdout.getvalue(), '')

def test_expr_with_output():
    eva = Evaluator()
    src = StringIO('(print 7)')
    eva.run(src)
    eq_(sys.stdout.getvalue(), '7\n')

def test_run_file():
    eva = Evaluator()
    with open('gcd1.ch1') as src:
        eva.run(src)
        eq_(sys.stdout.getvalue(), '3\n')

expected = '''5
120
6
720
7
5040
8
40320
9
362880
10
3628800
'''

def test_run_file_multiline_output():
    eva = Evaluator()
    with open('factorials.ch1') as src:
        eva.run(src)
        eq_(sys.stdout.getvalue(), expected)

