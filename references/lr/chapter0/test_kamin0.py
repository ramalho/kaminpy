from pytest import raises

from kamin0 import tokenize, evaluate, parse
from kamin0 import UnexpectedEndOfInput, UnexpectedRightParen
from kamin0 import InvalidOperator, MissingArguments, TooManyArguments

def test_tokenize_atom():
    assert tokenize('3') == ['3']

def test_tokenize_call():
    assert tokenize('(+ 2 3)') == ['(','+','2','3',')']

def test_tokenize_call_with_call():
    assert tokenize('(+ 2 (* 3 4))') == ['(','+','2','(','*','3','4',')',')']

def test_parse_number():
    assert parse('3') == 3

def test_parse_negative_number():
    assert parse('-3') == -3

def test_parse_symbol():
    assert parse('+') == '+'

def test_parse_plus_number_as_symbol():
    assert parse('+1') == '+1'

def test_parse_numexp():
    assert parse('(+ 2 3)') == ['+', 2, 3]

def test_parse_numexp_inner():
    assert parse('(+ 2 (* 3 4))') == ['+', 2, ['*', 3, 4]]

def test_parse_empty_paren():
    assert parse('()') == []

def test_parse_empty():
    with raises(UnexpectedEndOfInput):
        parse('')

def test_parse_right_paren():
    with raises(UnexpectedRightParen):
        parse(')')

def test_parse_open_paren():
    with raises(UnexpectedEndOfInput):
        parse('(')

def test_parse_right_paren_detail():
    with raises(UnexpectedRightParen) as excinfo:
        parse(')')

    assert str(excinfo.value) == 'unexpected )'

def test_parse_plus_one():
    assert parse('(++ 2)') == ['++', 2]

def test_eval_int():
    assert evaluate(parse('3')) == 3

def test_eval_op():
    import operator
    assert evaluate(parse('+')) == operator.add

def test_eval_numexp():
    assert evaluate(parse('(+ 2 3)')) == 5

def test_eval_numexp_inner():
    assert evaluate(parse('(+ 2 (* 3 4))')) == 14

def test_eval_no_operator():
    with raises(InvalidOperator):
        evaluate(parse('(2)'))

def test_eval_no_operator2():
    with raises(InvalidOperator):
        evaluate(parse('(2 3)'))

def test_eval_no_operator_detail():
    with raises(InvalidOperator) as excinfo:
        evaluate(parse('(2 3)'))

    assert str(excinfo.value) == 'invalid operator: 2'

def test_eval_sub():
    assert evaluate(parse('(- 2 3)')) == -1

def test_eval_div():
    assert evaluate(parse('(/ 6 2)')) == 3

def test_eval_div_returns_int():
    assert evaluate(parse('(/ 6 4)')) == 1

def test_eval_div_by_zero():
    with raises(ZeroDivisionError):
        evaluate(parse('(/ 6 0)'))

def test_eval_eq_true():
    assert evaluate(parse('(= 2 2)')) == 1

def test_eval_eq_false():
    assert evaluate(parse('(= 2 3)')) == 0

def test_eval_lt_true():
    assert evaluate(parse('(< 2 3)')) == 1

def test_eval_lt_false():
    assert evaluate(parse('(< 2 2)')) == 0

def test_eval_lt_false_2():
    assert evaluate(parse('(< 3 2)')) == 0

def test_eval_gt_true():
    assert evaluate(parse('(> 4 3)')) == 1

def test_eval_gt_false():
    assert evaluate(parse('(> 4 4)')) == 0

def test_eval_gt_false_2():
    assert evaluate(parse('(> 3 4)')) == 0

def test_plus_number():
    with raises(InvalidOperator):
        evaluate(parse('+1'))

def test_eval_too_few_args():
    with raises(MissingArguments):
        evaluate(parse('(* 1)'))

def test_eval_too_many_args():
    with raises(TooManyArguments):
        evaluate(parse('(* 1 2 3)'))
