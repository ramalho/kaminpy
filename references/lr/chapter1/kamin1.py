#!/usr/bin/env python
# coding: utf-8

'''
This interpreter implements the language described in Chapter 1 of Samuel
Kamin's Programming Languages book [1]. This implementation is based on
Peter Norvig's lis.py [2].

[1] Samuel Kamin, "Programming Languages, An Interpreter-Based Approach",
    Addison-Wesley, Reading, MA, 1990. ISBN 0-201-06824-9.
[2] http://norvig.com/lispy.html

BNF of this mini-language:

<input>      ::= <expression> | <fundef>
<fundef>     ::= `(` `define` <function> <arglist> <expression> `)`
<arglist>    ::= `(` <name>* `)`
<expression> ::= <integer>
               | <var-name>
               | `(` <operator> <expression>* `)`
               | `(` `if` <expression1>  <expression2> <expression3> `)`
               | `(` `print` <expression> `)`
               | `(` `begin` <expression>+ `)`
               | `(` `set` <name> <expression> `)`
               | `(` `while` <expression1>  <expression2> `)`
<operator>   ::= <function> | <value-op>
<function>   ::= <name>
<value-op>   ::= `+` | `-` | `*` | `/` | `=` | `<` | `>`
<integer>    ::= sequence of digits, possibly preceded by minus sign
<name>   ::= any sequence of characters not an integer, and not
                 containing a blank or `(`, `)`, `;`

Note that function definitions are not expressions in this language: they are
special statements (similarly, they are not expressions in Pascal, C or Java
either).

A function definition installs a function in a global namespace for later
use. When invoked, a user defined function receives the arguments as a local
environment.

All the other statements are expressions, and can be used anywhere an
expression is expected. The statements are:

(operator expression1 ... expressionN): Evaluate all expressions and apply
    operator or function to them, returning a result.

(if expression1 expression2 expression3): Evaluate expression1, if result
    is non-zero, evaluate and return value of expression2, otherwise
    evaluate and return value of expression3.

(print expression): Evaluate expression and output its value to stdout.

(begin expression1 ... expressionN): Evaluate all expressions in sequence,
    and return the value of the last expression.

(set variable expression): Evaluate expression and assign result to variable.
    If variable is already defined in the local environment (i.e. within a
    function), assignment is made in local environment, otherwise the variable
    is assigned in the global environment. Return the value of the expression.

(while expression1 expression2): Evaluate expression1 and if it is non-zero,
    evaluate expression2 then evaluate expression1 again, until expression1
    evaluates to 0. Always returns 0.


'''

import re
import sys
import inspect

REGEX_INTEGER = re.compile(r'-?\d+$')

class InterpreterError(Exception):
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

class ReservedIdentifier(InterpreterError):
    """reserved identifier"""

class InvalidFunctionDefinition(InterpreterError):
    """invalid function definition"""

def tokenize(source_code):
    """Convert a string into a list of tokens"""
    return source_code.replace('(',' ( ').replace(')',' ) ').split()

def parse(tokens):
    """Read tokens building a syntax tree of nested lists of expressions"""
    if len(tokens) == 0:
        raise UnexpectedEndOfInput()
    token = tokens.pop(0)

    if token == '(':
        parsed = []
        if len(tokens) == 0:
            raise UnexpectedEndOfInput()
        while tokens[0] != ')':
            parsed.append(parse(tokens))
            if len(tokens) == 0:
                raise UnexpectedEndOfInput()
        tokens.pop(0) # pop off ')'
        return parsed
    elif token == ')':
        raise UnexpectedRightParen()
    else:
        return atom(token)

def parse_source(source_code):
    '''Convenience function: tokenize and parse source_code'''
    return parse(tokenize(source_code))

def atom(token):
    """Return integers as integers, everything else as symbols"""
    if REGEX_INTEGER.match(token): # -1 is an int, +1 is a symbol
        return int(token)
    else:
        return token

def check_args(function, args, skip_params=None):
    """Compare arguments with parameters expected by function"""
    fixed_params, var_params = inspect.getfullargspec(function)[:2]
    if isinstance(skip_params, (list, tuple)):
        fixed_params = [param for param in fixed_params
                          if param not in skip_params]
    num_params = len(fixed_params)
    if len(args) < num_params:
        raise MissingArguments()
    elif len(args) > num_params and var_params is None:
        raise TooManyArguments()

def sexpression_reader(linereader, prompt1='->', prompt2='>'):
    """Return tokenized expression, ignoring comments and line breaks"""
    prompt = prompt1
    open_parens = 0 # pending (, not yet closed
    tokens = []
    while True:
        lin = linereader(prompt+' ')
        raw_tokens = tokenize(lin)
        for pos, token in enumerate(raw_tokens):
            if token == ';':
                break
            elif token == '(':
                open_parens += 1
            elif token == ')':
                open_parens -= 1
                if open_parens < 0:
                    raise UnexpectedRightParen()
            tokens.append(token)
        if open_parens == 0:
            return tokens
        prompt = prompt2

def interactive_reader():
    return sexpression_reader(input)

def batch_reader(src_file):
    def linereader(dummy):
        return src_file.readline().rstrip()
    return sexpression_reader(linereader)


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
            '/': lambda a, b: a // b,
            '=': lambda a, b: 1 if a == b else 0,
            '<': lambda a, b: 1 if a < b else 0,
            '>': lambda a, b: 1 if a > b else 0,
        }
        self.global_env = operators.copy()
        self.user_functions = {}

    def install_function(self, name, params, body):
        if name in self.commands:
            msg = 'cannot create function named %r' % name
            raise ReservedIdentifier(name)
        self.user_functions[name] = UserFunction(name, params, body)

    def evaluate(self, local_env, expression):
        """Calculate the value of an expression"""
        if isinstance(expression, int):
            return expression
        elif isinstance(expression, str): # symbol
            return self.get(local_env, expression)
        elif isinstance(expression, list):
            if expression[0] in self.commands:
                # special forms evaluate (or not) their args
                command = self.commands[expression[0]]
                args = expression[1:]
                check_args(command, args, ['self', 'local_env'])
                return command(local_env, *args)
            elif expression[0] in self.user_functions:
                function = self.user_functions[expression[0]]
                args = [self.evaluate(local_env, exp) for exp in expression[1:]]
                local_env = function.bind_args(args)
                return self.evaluate(local_env, function.body)
            else:
                # evaluate operator and args
                exps = [self.evaluate(local_env, exp) for exp in expression]
                if len(exps) == 0:
                    raise NullExpression()
                operator = exps[0]
                if callable(operator):
                    args = exps[1:]
                    check_args(operator, args)
                    return operator(*args) # apply operator to args
                else:
                    raise InvalidOperator(operator)
        else:
            msg = 'Cannot evaluate {0!r}'.format(expression)
            raise InterpreterError(msg)

    def get(self, local_env, identifier):
        if identifier in local_env:
            return local_env[identifier]
        elif identifier in self.global_env:
            return self.global_env[identifier]
        else:
            raise UnknownIdentifier(identifier)

    def set(self, local_env, identifier, value):
        if identifier in local_env:
            local_env[identifier] = value
        else:
            self.global_env[identifier] = value

    def repl(self, prompt='> '):
        """A read-eval-print loop"""
        local_env = {}
        while True:
            try:
                tokens = interactive_reader()
                expression = parse(tokens)
                if expression == 'quit':
                    return
                elif isinstance(expression, list) and expression[0] == 'define':
                    if len(expression) != 4:
                        raise InvalidFunctionDefinition()
                    self.install_function(*expression[1:])
                    print(expression[1])
                else:
                    value = self.evaluate(local_env, expression)
                    print(value)
                    print()
            except (InterpreterError, ZeroDivisionError) as exc:
                print('!', str(exc))
            except (KeyboardInterrupt, EOFError):
                print()
                return

    def run(self, src_file):
        local_env = {}
        while True:
            tokens = batch_reader(src_file)
            if not tokens:
                return
            expression = parse(tokens)
            if expression == 'quit':
                return
            elif isinstance(expression, list) and expression[0] == 'define':
                if len(expression) != 4:
                    raise InvalidFunctionDefinition()
                self.install_function(*expression[1:])
            else:
                value = self.evaluate(local_env, expression)

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

    def set_cmd(self, local_env, identifier, expression):
        value = self.evaluate(local_env, expression)
        self.set(local_env, identifier, value)
        return value

    def while_cmd(self, local_env, test, body):
        while self.evaluate(local_env, test):
            self.evaluate(local_env, body)
        return 0

class UserFunction(object):
    """A user-defined function"""
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

    def bind_args(self, args):
        if len(args) < len(self.params):
            raise MissingArguments()
        elif len(args) > len(self.params):
            raise TooManyArguments()
        return dict(zip(self.params, args))

if __name__=='__main__':
    if len(sys.argv) == 1:
        Evaluator().repl()
    elif len(sys.argv) == 2 and sys.argv[1] != '-h':
        try:
            with open(sys.argv[1]) as src:
                Evaluator().run(src)
        except IOError as exc:
            print(exc)
    else:
        print('usage: {0} [src.ch1]'.format(__file__))
