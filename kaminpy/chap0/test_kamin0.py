#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_

from kamin0 import *

def test_tokenize_atom():
    eq_(tokenize('3'), ['3'])

def test_tokenize_call():
    eq_(tokenize('(+ 2 3)'), ['(','+','2','3',')'])
