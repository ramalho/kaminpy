import operator
import textwrap

from pytest import mark, raises

from .expressions import tokenize, parse, evaluate, Operator, repl
from .expressions import UnexpectedEndOfInput, UnexpectedCloseParen
from .expressions import UnknownOperator, InvalidOperator
from .expressions import MissingArgument, TooManyArguments
from .expressions import NullExpression


@mark.parametrize("source,want", [
    ("3", ["3"]),
    ("(+ 2 3)", ["(", "+", "2", "3", ")"]),
    ("(+ 2 (* 3 4))", ["(", "+", "2", "(", "*", "3", "4", ")", ")"]),
])
def test_tokenize(source, want):
    assert tokenize(source) == want


@mark.parametrize("tokens,want", [
    (["3"], 3),
    (["-3"], -3),
    (["+3"], 3),
    (["+"], "+"),
])
def test_parse_atoms(tokens, want):
    assert parse(tokens) == want


@mark.parametrize("source,want", [
    ("(2)", [2]),
    ("(+ 2 3)", ["+", 2, 3]),
    ("(+ 2 (* 3 4))",
    ["+", 2, ["*", 3, 4]]),
])
def test_parse_expressions(source, want):
    tokens = tokenize(source)
    assert parse(tokens) == want


def test_parse_empty():
    with raises(UnexpectedEndOfInput):
        parse([])


def test_parse_no_close_paren():
    with raises(UnexpectedEndOfInput):
        parse(["("])


def test_parse_right_paren_detail():
    with raises(UnexpectedCloseParen) as excinfo:
        parse([")"])
    assert str(excinfo.value) == "Unexpected ')'."


@mark.parametrize("source,want", [
    ("2", 2),
    ("-2", -2),
    ("-", Operator(symbol="-", function=operator.sub)),
])
def test_evaluate_atoms(source, want):
    expr = parse(tokenize(source))
    assert evaluate(expr) == want


def test_evaluate_unknown_operator():
    with raises(UnknownOperator) as excinfo:
        expr = parse(tokenize("@"))
        evaluate(expr)
    assert str(excinfo.value) == "Unknown operator: '@'."


@mark.parametrize("source,want", [
    ("(+ 8 2)", 10),
    ("(- 8 2)", 6),
    ("(* 8 2)", 16),
    ("(/ 8 2)", 4),
    ("(/ 2 3)", 0),
    ("(/ 20 6)", 3),
    ("(/ -20 6)", -4),
    ("(+ 2 (* 3 4))", 14),
    # (100°F − 32) * 5 / 9 = 37°C
    ("(/ (* (- 100 32) 5) 9)", 37),
    ("(/ (* (- 0 32) 5) 9)", -18),
])
def test_evaluate_expressions(source, want):
    expr = parse(tokenize(source))
    assert evaluate(expr) == want


def test_evaluate_not_operator():
    expr = parse(tokenize("(2)"))
    with raises(InvalidOperator):
        evaluate(expr)


def test_evaluate_not_operator_with_argument():
    expr = parse(tokenize("(3 4)"))
    with raises(InvalidOperator):
        evaluate(expr)


def test_evaluate_empty_expression():
    expr = parse(tokenize("()"))
    with raises(NullExpression):
        evaluate(expr)


def test_evaluate_not_operator_detail():
    expr = parse(tokenize("(5 6)"))
    with raises(InvalidOperator) as excinfo:
        evaluate(expr)

    assert str(excinfo.value) == "Invalid operator: 5."


def test_evaluate_missing_arguments():
    expr = parse(tokenize("(* 5)"))
    with raises(MissingArgument) as excinfo:
        evaluate(expr)

    assert str(excinfo.value) == "Not enough arguments for operator: '*'."


def test_evaluate_too_many_arguments():
    expr = parse(tokenize("(/ 6 7 8)"))
    with raises(TooManyArguments) as excinfo:
        evaluate(expr)

    assert str(excinfo.value) == "Too many arguments for operator: '/'."


class TextInteraction():

    def __init__(self, session):
        self.session = session
        self.prompts = set()
        self.input_line_gen = iter(self)

    def __iter__(self):
        for line in self.session.splitlines():
            line = line.strip()
            for prompt in self.prompts:
                if line.startswith(prompt):
                    yield line[len(prompt):]
                    break

    def fake_input(self, prompt):
        self.prompts.add(prompt)
        try:
            line = next(self.input_line_gen)
        except StopIteration:
            raise EOFError()
        print(prompt, end='')
        print(line)
        return line

    def __str__(self):
        return textwrap.dedent(self.session.lstrip('\n'))


@mark.parametrize("session", [
    """
    > .q
    """,
    """
    > (* 111 111)
    12321
    """,
    """
    > (* 111
    ... 111)
    12321
    """,
    """
    > (/ 6 0)
    ! Division by zero.
    > (/ 6 3)
    2
    """,
    """
    > (foo 6 0)
    ! Unknown operator: 'foo'.
    """,
    """
    > (9 8 7)
    ! Invalid operator: 9.
    """,
    """
    > (+ 6)
    ! Not enough arguments for operator: '+'.
    > (+ 6 3)
    9
    """,
    """
    > (/ 6 5 4)
    ! Too many arguments for operator: '/'.
    """,
    """
    > )
    ! Unexpected ')'.
    > (+ 2 2)
    4
    """,

])
def test_repl(monkeypatch, capsys, session):
    ti = TextInteraction(session)
    with monkeypatch.context() as m:
        m.setitem(__builtins__, "input", ti.fake_input)
        repl()
    captured = capsys.readouterr()
    assert captured.out == str(ti)
