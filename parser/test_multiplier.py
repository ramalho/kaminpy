import pytest

from multiplier import evaluate


def test_simple_multiplication():
    assert evaluate('2 * 3') == 6


def test_multiplication_and_addition():
    assert evaluate('2 * 3 + 4') == 10


def test_addition_multiplication():
    assert evaluate('2 + 3 * 4') == 14


def test_two_multiplications():
    assert evaluate('2 * 3 + 4 * 5') == 26


def test_5_factorial():
    assert evaluate('2 * 3 * 4 * 5') == 120
