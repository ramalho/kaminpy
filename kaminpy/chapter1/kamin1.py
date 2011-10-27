#!/usr/bin/env python
# coding: utf-8

'''
This interpreter implements the language described in Chapter 1 of Samuel 
Kamin's Programming Languages book [1]. This implementation is based on
Peter Norvig's lis.py [2]. 

[1] Samuel Kamin, "Programming Languages, An Interpreter-Based Approach",
    Addison-Wesley, Reading, MA, 1990. ISBN 0-201-06824-9.
[2] http://norvig.com/lispy.html

BNF of this mini-language (so far):

<expression> ::= <integer>
               | `(` <value-op> <expression1>  <expression2> `)`
               | `(` `if` <expression1>  <expression2> <expression3> `)`
               | `(` `print` <expression> `)`
               | `(` `begin` <expression>+ `)`
               | `(` `set` <identifier> <expression>`)`
<value-op>   ::= `+` | `-` | `*` | `/` | `=` | `<` | `>`
<integer>    ::= sequence of digits, possibly preceded by minus sign
<identifier> ::= any sequence of characters not an integer, and not
                 containing a blank or `(`, `)`, `;`

'''

import re
import sys
import inspect

REGEX_INTEGER = re.compile(r'-?\d+$')

class InterpreterError(StandardError):
    """generic interpreter error"""
    def __init__(self, value=None):
        self.value = value
    def __str__(self):
        msg = self.__class__.__doc__
        if self.value is not None:
            return msg + ': ' + repr(self.value)
        return msg

class UnexpectedEndOfInput(InterpreterError):
    """unexpected end of input"""

class UnexpectedRightParen(InterpreterError):
    """unexpected )"""

class InvalidOperator(InterpreterError):
    """invalid operator"""

class NullExpression(InterpreterError):
    """null expression"""

class MissingArguments(InterpreterError):
    """missing arguments"""

class TooManyArguments(InterpreterError):
    """too many arguments"""

class UnknownIdentifier(InterpreterError):
    """unknown identifier"""

def tokenize(source_code):
    """Convert a string into a list of tokens"""
    return source_code.replace('(',' ( ').replace(')',' ) ').split()

def parse(source_code):
    """Convert source code into syntax tree"""
    tokens = tokenize(source_code)
    return read(tokens)

def read(tokens):
    """Read tokens building a syntax tree of nested lists of expressions"""
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
    """Return integers as integers, everything else as symbols"""
    if REGEX_INTEGER.match(token): # -1 is an int, +1 is a symbol
        return int(token)
    else:
        return token

def check_args(function, args, skip_params=None):
    """Compare arguments with parameters expected by function"""
    fixed_args, var_args = inspect.getargspec(function)[:2]
    if isinstance(skip_params, (list, tuple)):
        fixed_args = [arg for arg in fixed_args 
                          if arg not in skip_params]
    min_args = max_args = len(fixed_args)
    if len(args) < min_args:
        raise MissingArguments()
    elif len(args) > max_args and var_args is None:
        raise TooManyArguments()

class Evaluator(object):

    def __init__(self):
        self.commands = {}
        for name in dir(self): # collect methods with _cmd suffix
            if name.endswith('_cmd'):
                self.commands[name[:-4]] = getattr(self, name)

        # use lambdas and not the operator module because inspect.getargspec 
        # only works with functions defined in Python
        operators = {
            '+': lambda *a: sum(a), 
            '-': lambda a, b: a - b, 
            '*': lambda a, b: a * b, 
            '/': lambda a, b: a / b, 
            '=': lambda a, b: 1 if a == b else 0,
            '<': lambda a, b: 1 if a < b else 0,
            '>': lambda a, b: 1 if a > b else 0,
        }
        self.global_env = operators.copy()

    def evaluate(self, local_env, expression):
        """Calculate the value of an expression"""
        if isinstance(expression, int):
            return expression
        elif isinstance(expression, str): # symbol
            return self.get(local_env, expression)
        elif expression[0] in self.commands:
            # special forms evaluate (or not) their args
            command = self.commands[expression.pop(0)]
            check_args(command, expression, ['self', 'local_env'])
            return command(local_env, *expression)
        else: 
            # evaluate operator and args
            exps = [self.evaluate(local_env, exp) for exp in expression]
            if len(exps) == 0:
                raise NullExpression()
            operator = exps.pop(0)
            if callable(operator):
                check_args(operator, exps)
                return operator(*exps) # apply operator to args
            else:
                raise InvalidOperator(operator)

    def get(self, local_env, name):
        if name in local_env:
            return local_env[name]
        elif name in self.global_env:
            return self.global_env[name]
        else:
            raise UnknownIdentifier(name)


    #######################################################################
    # commands of the language

    def if_cmd(self, local_env, test, conseq, alt):
        result = conseq if self.evaluate(local_env, test) else alt
        return self.evaluate(local_env, result)

    def print_cmd(self, local_env, arg):
        result = self.evaluate(local_env, arg)
        print(result)
        return result

    def begin_cmd(self, local_env, first, *rest):
        for exp in (first,)+rest:
            result = self.evaluate(local_env, exp)
        return result

    #######################################################################


def repl(prompt='> '):
    """A read-eval-print loop"""
    while True:
        try:
            value = evaluate(parse(raw_input(prompt)))
        except (InterpreterError, ZeroDivisionError) as exc:
            print('! ' + str(exc))
        except KeyboardInterrupt:
            print()
            raise SystemExit
        else:
            print(value)

if __name__=='__main__':
    repl()
