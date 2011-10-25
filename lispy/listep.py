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

def parse(source_code):
    tokens = tokenize(source_code)
    return read(tokens)

def read(tokens):
    token = tokens.pop(0)
    if token == '(':
        parsed = []
        while tokens[0] != ')':
            parsed.append(read(tokens))
        tokens.pop(0) # pop off ')'
        return parsed
    else:
        try:
            return int(token)
        except ValueError:
            return token

