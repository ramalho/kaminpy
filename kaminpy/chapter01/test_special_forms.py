#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises

from kamin1 import *

def test_eval_if_true():
    eq_(evaluate(parse('(if 1 2 3)')), 2)

def test_eval_if_false():
    eq_(evaluate(parse('(if 0 2 3)')), 3)

def test_eval_if_true_dont_eval_alternative():
    eq_(evaluate(parse('(if 1 2 (/ 1 0))')), 2)

def test_eval_if_false_dont_eval_consequent():
    eq_(evaluate(parse('(if 0 (/ 1 0) 3)') ), 3)
