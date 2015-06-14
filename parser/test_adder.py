import pytest

from adder import evaluate


def test_literal():
    assert evaluate('42') == 42


def test_simple_addition():
    assert evaluate('1 + 2') == 3


def test_long_addition():
    assert evaluate('1 + 2 + 3 + 4 + 5') == 15


def test_error_plus():
    with pytest.raises(SyntaxError) as excinfo:
        evaluate('+')

    assert excinfo.value.args[0] == "unexpected '+' in line:\n+"


def test_error_null_src():
    assert evaluate('  ') is None


def test_error_eof():
    with pytest.raises(SyntaxError) as excinfo:
        evaluate('1 +')

    assert excinfo.value.args[0] == 'unexpected end of source'
