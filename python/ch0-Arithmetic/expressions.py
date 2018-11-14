"""
Simple interpreter implementing the arithmetic expression subset of
the language described in Chapter 1 of Samuel Kamin's PLIBA book [1].
This Python code is heavily inspired by Peter Norvig's lis.py [2],
but does not try to be as concise.

[1] Samuel Kamin, "Programming Languages, An Interpreter-Based Approach",
    Addison-Wesley, Reading, MA, 1990. ISBN 0-201-06824-9.
[2] http://norvig.com/lispy.html

BNF of this mini-language:

<expression> ::= <integer>
               | `(` <value-op> <expression>* `)`
<value-op>   ::= `+` | `-` | `*` | `/` | `=` | `<` | `>`
<integer>    ::= sequence of digits, possibly preceded by minus sign

"""

import collections
import operator

class InterpreterError(Exception):
    """generic interpreter error"""
    def __init__(self, value=None):
        self.value = value

    def __str__(self):
        msg = self.__class__.__doc__
        if self.value is not None:
            msg = msg.rstrip('.')
            msg += ': ' + repr(self.value) + '.'
        return msg

class UnexpectedEndOfInput(InterpreterError):
    """Unexpected end of input."""

class UnexpectedRightParen(InterpreterError):
    """Unexpected ')'."""

class UnknownOperator(InterpreterError):
    """Unknown operator."""

class EvaluationError(InterpreterError):
    """Generic evaluation error."""

class OperatorNotCallable(EvaluationError):
    """Operator is not callable."""

class NullExpression(EvaluationError):
    """Null expression."""

class MissingArgument(EvaluationError):
    """Not enough arguments for operator."""

class TooManyArguments(EvaluationError):
    """Too many arguments for operator."""

class InvalidOperator(EvaluationError):
    """Invalid operator."""

def tokenize(source_code):
    """Convert string into a list of tokens."""
    return source_code.replace('(',' ( ').replace(')',' ) ').split()


def parse(tokens):
    """Read tokens building recursively nested expressions."""
    try:
        token = tokens.pop(0)
    except IndexError:
        raise UnexpectedEndOfInput()
    if token == '(':  # s-expression
        ast = []
        if len(tokens) == 0:
            raise UnexpectedEndOfInput()
        while tokens[0] != ')':
            ast.append(parse(tokens))
        tokens.pop(0)  # pop off ')'
        return ast
    elif token == ')':
        raise UnexpectedRightParen()
    else:  # single atom
        try:
            return int(token)
        except ValueError:
            return token

Operator = collections.namedtuple('Operator', 'symbol function')

operators = [
    Operator('+', operator.add),
    Operator('-', operator.sub),
    Operator('*', operator.mul),
    Operator('/', operator.floordiv),
]

operator_map = {op.symbol:op for op in operators}

def evaluate(expression):
    """Calculate the value of an expression"""
    if isinstance(expression, int):  # integer
        return expression
    elif isinstance(expression, str):  # operator
        try:
            return operator_map[expression]
        except KeyError:
            raise UnknownOperator(expression)
    else:  # s-expression
        exps = [evaluate(exp) for exp in expression]
        if len(exps) == 0:
            raise NullExpression()
        op = exps.pop(0)
        if isinstance(op, Operator):
            if len(exps) == 2:
                arg1, arg2 = exps
                return op.function(arg1, arg2)
            elif len(exps) < 2:
                raise MissingArgument(op.symbol)
            else:
                raise TooManyArguments(op.symbol)
        else:
            raise InvalidOperator(op)
