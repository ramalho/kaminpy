#!/usr/bin/env python
# coding: utf-8

from kamin1 import Evaluator

from io import StringIO

def test_expr_no_output(capsys):
    eva = Evaluator()
    src = StringIO('(+ 2 3)')
    eva.run(src)
    captured = capsys.readouterr()
    assert captured.out == ''

def test_expr_with_output(capsys):
    eva = Evaluator()
    src = StringIO('(print 7)')
    eva.run(src)
    captured = capsys.readouterr()
    assert captured.out == '7\n'

def test_run_file(capsys):
    eva = Evaluator()
    with open('gcd1.ch1') as src:
        eva.run(src)
        captured = capsys.readouterr()
        assert captured.out == '3\n'

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

def test_run_file_multiline_output(capsys):
    eva = Evaluator()
    with open('factorials.ch1') as src:
        eva.run(src)
        captured = capsys.readouterr()
        assert captured.out == expected
