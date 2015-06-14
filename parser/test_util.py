from tokenize import TokenInfo

import util


def test_tokens_expr():
    src, names = expr_fix
    assert list(util.tokenize(src)) == [
        TokenInfo(type=2, string='2', start=(1, 0), end=(1, 1), line='2 + 3'),
        TokenInfo(type=14, string='+', start=(1, 2), end=(1, 3), line='2 + 3'),
        TokenInfo(type=2, string='3', start=(1, 4), end=(1, 5), line='2 + 3'),
        TokenInfo(type=0, string='', start=(2, 0), end=(2, 0), line='')]


expr_fix = ('2 + 3', ['NUMBER', 'PLUS', 'NUMBER', 'ENDMARKER'])


def test_token_names_expr():
    src, names = expr_fix
    assert util.token_names(src) == names


if_fix = ('''if x <= 1:\n'''
          '''     print(x)''',
          ['NAME', 'NAME', 'LESSEQUAL', 'NUMBER', 'COLON', 'NEWLINE',
           'INDENT', 'NAME', 'LPAR', 'NAME', 'RPAR',
           'DEDENT', 'ENDMARKER'])


def test_token_names_if():
    src, names = if_fix
    assert util.token_names(src) == names


for_fix = ('''for i in range(3):\n'''
           '''     print(i)\n'''
           '''print('end')''',
           ['NAME', 'NAME', 'NAME', 'NAME', 'LPAR', 'NUMBER', 'RPAR', 'COLON',
            'NEWLINE', 'INDENT', 'NAME', 'LPAR', 'NAME', 'RPAR', 'NEWLINE',
            'DEDENT', 'NAME', 'LPAR', 'STRING', 'RPAR', 'ENDMARKER'])


def test_token_names_for():
    src, names = for_fix
    assert util.token_names(src) == names
