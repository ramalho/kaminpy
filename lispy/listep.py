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

class ParseError(Exception):
    """Generic syntax error"""
    def __init__(self, msg=None):
        self.msg = msg if msg else self.__class__.__doc__
    def __str__(self):
        return str(self.msg)

class UnexpectedEndOfInput(ParseError):
    """Unexpected end of input"""

class UnexpectedRightParen(ParseError):
    """Unexpected )"""

def tokenize(source_code):
    "Convert a string into a list of tokens."
    return source_code.replace('(',' ( ').replace(')',' ) ').split()

def parse(source_code):
    tokens = tokenize(source_code)
    return read(tokens)

def read(tokens):
    if len(tokens) == 0:
        raise UnexpectedEndOfInput()
    token = tokens.pop(0)

    if token == '(':
        parsed = []
        if len(tokens) == 0:
            raise UnexpectedEndOfInput()
        while tokens[0] != ')':
            parsed.append(read(tokens))
        tokens.pop(0) # pop off ')'
        return parsed
    elif token == ')':
        raise UnexpectedRightParen()
    else:
        try:
            return int(token)
        except ValueError:
            return token

def evaluate(expression):
    if isinstance(expression, int):
        return expression
