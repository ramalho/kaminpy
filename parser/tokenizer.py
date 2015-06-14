r"""
    >>> dump_tokens('2 + 3')
    TokenInfo(type=2 (NUMBER), string='2', start=(1, 0), end=(1, 1), line='2 + 3')
    TokenInfo(type=14 (PLUS), string='+', start=(1, 2), end=(1, 3), line='2 + 3')
    TokenInfo(type=2 (NUMBER), string='3', start=(1, 4), end=(1, 5), line='2 + 3')
    TokenInfo(type=0 (ENDMARKER), string='', start=(2, 0), end=(2, 0), line='')
    >>> src = '''if x <= 1:
    ...     print(x)'''
    ...
    >>> dump_tokens(src)
    TokenInfo(type=1 (NAME), string='if', start=(1, 0), end=(1, 2), line='if x <= 1:\n')
    TokenInfo(type=1 (NAME), string='x', start=(1, 3), end=(1, 4), line='if x <= 1:\n')
    TokenInfo(type=29 (LESSEQUAL), string='<=', start=(1, 5), end=(1, 7), line='if x <= 1:\n')
    TokenInfo(type=2 (NUMBER), string='1', start=(1, 8), end=(1, 9), line='if x <= 1:\n')
    TokenInfo(type=11 (COLON), string=':', start=(1, 9), end=(1, 10), line='if x <= 1:\n')
    TokenInfo(type=4 (NEWLINE), string='\n', start=(1, 10), end=(1, 11), line='if x <= 1:\n')
    TokenInfo(type=5 (INDENT), string='    ', start=(2, 0), end=(2, 4), line='    print(x)')
    TokenInfo(type=1 (NAME), string='print', start=(2, 4), end=(2, 9), line='    print(x)')
    TokenInfo(type=7 (LPAR), string='(', start=(2, 9), end=(2, 10), line='    print(x)')
    TokenInfo(type=1 (NAME), string='x', start=(2, 10), end=(2, 11), line='    print(x)')
    TokenInfo(type=8 (RPAR), string=')', start=(2, 11), end=(2, 12), line='    print(x)')
    TokenInfo(type=6 (DEDENT), string='', start=(3, 0), end=(3, 0), line='')
    TokenInfo(type=0 (ENDMARKER), string='', start=(3, 0), end=(3, 0), line='')
    >>> src = '''for i in range(3):
    ...     print(i)
    ... print('end')'''
    ...
    >>> dump_tokens(src)
    TokenInfo(type=1 (NAME), string='for', start=(1, 0), end=(1, 3), line='for i in range(3):\n')
    TokenInfo(type=1 (NAME), string='i', start=(1, 4), end=(1, 5), line='for i in range(3):\n')
    TokenInfo(type=1 (NAME), string='in', start=(1, 6), end=(1, 8), line='for i in range(3):\n')
    TokenInfo(type=1 (NAME), string='range', start=(1, 9), end=(1, 14), line='for i in range(3):\n')
    TokenInfo(type=7 (LPAR), string='(', start=(1, 14), end=(1, 15), line='for i in range(3):\n')
    TokenInfo(type=2 (NUMBER), string='3', start=(1, 15), end=(1, 16), line='for i in range(3):\n')
    TokenInfo(type=8 (RPAR), string=')', start=(1, 16), end=(1, 17), line='for i in range(3):\n')
    TokenInfo(type=11 (COLON), string=':', start=(1, 17), end=(1, 18), line='for i in range(3):\n')
    TokenInfo(type=4 (NEWLINE), string='\n', start=(1, 18), end=(1, 19), line='for i in range(3):\n')
    TokenInfo(type=5 (INDENT), string='    ', start=(2, 0), end=(2, 4), line='    print(i)\n')
    TokenInfo(type=1 (NAME), string='print', start=(2, 4), end=(2, 9), line='    print(i)\n')
    TokenInfo(type=7 (LPAR), string='(', start=(2, 9), end=(2, 10), line='    print(i)\n')
    TokenInfo(type=1 (NAME), string='i', start=(2, 10), end=(2, 11), line='    print(i)\n')
    TokenInfo(type=8 (RPAR), string=')', start=(2, 11), end=(2, 12), line='    print(i)\n')
    TokenInfo(type=4 (NEWLINE), string='\n', start=(2, 12), end=(2, 13), line='    print(i)\n')
    TokenInfo(type=6 (DEDENT), string='', start=(3, 0), end=(3, 0), line="print('end')")
    TokenInfo(type=1 (NAME), string='print', start=(3, 0), end=(3, 5), line="print('end')")
    TokenInfo(type=7 (LPAR), string='(', start=(3, 5), end=(3, 6), line="print('end')")
    TokenInfo(type=3 (STRING), string="'end'", start=(3, 6), end=(3, 11), line="print('end')")
    TokenInfo(type=8 (RPAR), string=')', start=(3, 11), end=(3, 12), line="print('end')")
    TokenInfo(type=0 (ENDMARKER), string='', start=(4, 0), end=(4, 0), line='')

"""  # noqa

import io
import tokenize as pytok


def tokenize(src):
    byte_src = io.BytesIO(src.encode('utf-8'))
    for token in pytok.tokenize(byte_src.readline):
        if token.type == pytok.ENCODING:  # discard encoding token
            continue
        if token.type == pytok.OP:
            token = pytok.TokenInfo(token.exact_type, *token[1:])
        yield token


def dump_tokens(src):
    for token in tokenize(src):
        print(token)
