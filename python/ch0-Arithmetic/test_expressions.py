import operator

from pytest import mark, raises

from expressions import tokenize, parse, evaluate, Operator
from expressions import UnexpectedEndOfInput, UnexpectedRightParen
from expressions import UnknownOperator, InvalidOperator
from expressions import MissingArgument, TooManyArguments


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
    with raises(UnexpectedRightParen) as excinfo:
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
