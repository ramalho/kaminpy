#!/usr/bin/env python
# coding: utf-8

'''
This is a step-by-step reconstruction of Peter Norvig's lis.py [1] 
interpreter, here accompanied by tests which hopefully illustrate 
how each part of the interpreter works.

The original source code of lis.py is at [2]

[1] http://norvig.com/lispy.html
[2] http://norvig.com/lis.py
'''

def tokenize(source_code):
    "Convert a string into a list of tokens."
    return source_code.replace('(',' ( ').replace(')',' ) ').split()
