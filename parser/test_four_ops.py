import pytest

from four_ops import evaluate


def test_four_ops():
    assert evaluate('2 * 3 + 4 / 5 - 10') == -3.2

def test_four_ops2():
    assert evaluate('2 - 3 / 4 + 5 * 10') == 51.25

