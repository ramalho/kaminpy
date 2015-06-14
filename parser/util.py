"""
util.py: utility functions for parsers

"""

import io
import tokenize as pytok
from token import tok_name


def tokenize(src):
    byte_src = io.BytesIO(src.encode('utf-8'))
    for token in pytok.tokenize(byte_src.readline):
        if token.type == pytok.ENCODING:  # discard encoding token
            continue
        if token.type == pytok.OP:
            token = pytok.TokenInfo(token.exact_type, *token[1:])
        yield token


def token_names(src):
    return [tok_name[token.type] for token in tokenize(src)]


def dump_tokens(src):
    for token in tokenize(src):
        print(token)
