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
               | `(` <operator> <expression>* `)`
<operator>   ::= `+` | `-` | `*` | `/`
<integer>    ::= sequence of digits, possibly preceded by - or +

"""

import collections
import operator
import sys

QUIT_COMMAND = '.q'


class InterpreterError(Exception):
    """generic interpreter error"""

    def __init__(self, value=None):
        self.value = value

    def __str__(self):
        msg = self.__class__.__doc__
        if self.value is not None:
            msg = msg.rstrip(".")
            msg += ": " + repr(self.value) + "."
        return msg


class UnexpectedEndOfInput(InterpreterError):
    """Unexpected end of input."""


class UnexpectedCloseParen(InterpreterError):
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
    return source_code.replace("(", " ( ").replace(")", " ) ").split()


def parse(tokens):
    """Read tokens building recursively nested expressions."""
    try:
        token = tokens.pop(0)
    except IndexError as exc:
        raise UnexpectedEndOfInput() from exc
    if token == "(":  # s-expression
        ast = []
        if len(tokens) == 0:
            raise UnexpectedEndOfInput()
        while tokens[0] != ")":
            ast.append(parse(tokens))
            if len(tokens) == 0:
                raise UnexpectedEndOfInput()
        tokens.pop(0)  # pop off ')'
        return ast
    elif token == ")":
        raise UnexpectedCloseParen()
    else:  # single atom
        try:
            return int(token)
        except ValueError:
            return token


Operator = collections.namedtuple("Operator", "symbol function")

OPERATORS = [
    Operator("+", operator.add),
    Operator("-", operator.sub),
    Operator("*", operator.mul),
    Operator("/", operator.floordiv),
]

OPERATOR_MAP = {op.symbol: op for op in OPERATORS}


def evaluate(expression):
    """Calculate the value of an expression"""
    if isinstance(expression, int):  # integer
        return expression
    elif isinstance(expression, str):  # operator
        try:
            return OPERATOR_MAP[expression]
        except KeyError as exc:
            raise UnknownOperator(expression) from exc
    else:  # multi-part expression
        if len(expression) == 0:
            raise NullExpression()
        parts = [evaluate(subexp) for subexp in expression]
        op = parts.pop(0)
        if isinstance(op, Operator):
            if len(parts) == 2:
                arg1, arg2 = parts
                return op.function(arg1, arg2)
            elif len(parts) < 2:
                raise MissingArgument(op.symbol)
            else:
                raise TooManyArguments(op.symbol)
        else:
            raise InvalidOperator(op)


def repl():
    prompt = '>'
    pending_lines = []
    print(f'To exit, type: {QUIT_COMMAND}', file=sys.stderr)
    while True:
        # ______________________________ Read
        try:
            current = input(prompt + ' ').strip(' ')
        except EOFError:
            break
        if current == QUIT_COMMAND:
            break
        if current == '':
            prompt = '...'
            continue
        pending_lines.append(current)
        # ______________________________ Parse
        source = ' '.join(pending_lines)
        expr = None
        try:
            expr = parse(tokenize(source))
        except UnexpectedEndOfInput:
            prompt = '...'
            continue
        except UnexpectedCloseParen as exc:
            print(f'! {exc}')
        # ______________________________ Evaluate & Print
        if expr is not None:
            try:
                result = evaluate(expr)
            except ZeroDivisionError:
                print('! Division by zero.')
            except (UnknownOperator, InvalidOperator,
                    MissingArgument, TooManyArguments,
                    ) as exc:
                print(f'! {exc}')
            else:
                print(result)
        prompt = '>'
        pending_lines = []
        # ______________________________ Loop


if __name__ == '__main__':
    repl()
