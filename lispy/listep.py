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

import operator as op

class InputError(Exception):
    """Generic syntax error"""
    def __init__(self, value=None):
        self.value = value
    def __str__(self):
        msg = self.__class__.__doc__
        if self.value:
            return msg + ': ' + repr(self.value)
        return msg

class UnexpectedEndOfInput(InputError):
    """Unexpected end of input"""

class UnexpectedRightParen(InputError):
    """Unexpected )"""

class InvalidOperator(InputError):
    """Invalid operator"""

def tokenize(source_code):
    """Convert a string into a list of tokens."""
    return source_code.replace('(',' ( ').replace(')',' ) ').split()

def parse(source_code):
    """Convert a string into expressions represented as (nested) lists."""
    tokens = tokenize(source_code)
    return read(tokens)

def read(tokens):
    """Recursively read tokens building nested expressions"""
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

operators = {
    '+': op.add,
    '-': op.sub,
    '*': op.mul,
    '/': op.div,
}

def evaluate(expression):
    """Calculate the value of an expression"""
    if isinstance(expression, int):
        return expression
    elif isinstance(expression, str): # operator
        return operators[expression]
    else:
        exps = [evaluate(exp) for exp in expression]
        operator = exps.pop(0)
        if not callable(operator):
            raise InvalidOperator(operator)
        return operator(*exps)
