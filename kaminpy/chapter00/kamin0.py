#!/usr/bin/env python
# coding: utf-8

'''
This is a simple arithmetic expression interpreter very much inspired
by Peter Norvig's lis.py [1]. It implements the arithmetic expression
subset of the language described in Chapter 1 of Samuel Kamin's book
Programming Languages book [2]. 

[1] http://norvig.com/lispy.html
[2] Samuel Kamin, "Programming Languages, An Interpreter-Based Approach",
    Addison-Wesley, Reading, MA, 1990. ISBN 0-201-06824-9.

BNF of this mini-language:

<expression> ::= <integer>
               | `(` <value-op> <expression>* `)`
<value-op>   ::= `+` | `-` | `*` | `/` | `=` | `<` | `>`
<integer>    ::= sequence of digits, possibly preceded by minus sign 

'''

import operator as op
import re

REGEX_INTEGER = re.compile(r'-?\d+$')

class InputError(StandardError):
    """generic syntax error"""
    def __init__(self, value=None):
        self.value = value
    def __str__(self):
        msg = self.__class__.__doc__
        if self.value is not None:
            return msg + ': ' + repr(self.value)
        return msg

class UnexpectedEndOfInput(InputError):
    """unexpected end of input"""

class UnexpectedRightParen(InputError):
    """unexpected )"""

class InvalidOperator(InputError):
    """invalid operator"""

class EmptyExpression(InputError):
    """empty expression"""

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
            if len(tokens) == 0:
                raise UnexpectedEndOfInput()
        tokens.pop(0) # pop off ')'
        return parsed
    elif token == ')':
        raise UnexpectedRightParen()
    else:
        return atom(token)

def atom(token):
    """Return numbers as numbers, everything else as symbols"""
    if REGEX_INTEGER.match(token):
        return int(token)
    else:
        return token

operators = {
    '+': op.add,
    '-': op.sub,
    '*': op.mul,
    '/': op.div,
    '=': lambda a, b: 1 if a == b else 0,
    '<': lambda a, b: 1 if a < b else 0,
    '>': lambda a, b: 1 if a > b else 0,
}

def evaluate(expression):
    """Calculate the value of an expression"""
    if isinstance(expression, int):
        return expression
    elif isinstance(expression, str): # operator
        try:
            return operators[expression]
        except KeyError:
            raise InvalidOperator(expression)
    else:
        exps = [evaluate(exp) for exp in expression]
        if len(exps) == 0:
            raise EmptyExpression()
        operator = exps.pop(0)
        if callable(operator):
            return operator(*exps)
        else:
            raise InvalidOperator(operator)

def repl(prompt='> '):
    """A read-eval-print loop."""
    while True:
        try:
            value = evaluate(parse(raw_input(prompt)))
        except (InputError, TypeError, ZeroDivisionError) as exc:
            print('! ' + str(exc)) 
        except KeyboardInterrupt:
            print()
            raise SystemExit
        else:
            print(value)

if __name__=='__main__':
    repl()
