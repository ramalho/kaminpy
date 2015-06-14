"""
util.py: utility functions for parsers

"""

import io
import tokenize as py_tokenize
import token as py_token


def tokenize(src):
    byte_src = io.BytesIO(src.encode('utf-8'))
    for token in py_tokenize.tokenize(byte_src.readline):
        if token.type == py_tokenize.ENCODING:  # discard encoding token
            continue
        if token.type == py_token.OP:
            token = py_tokenize.TokenInfo(token.exact_type, *token[1:])
        yield token


def token_names(src):
    return [py_token.tok_name[token.type] for token in tokenize(src)]


def dump_tokens(src):
    for token in tokenize(src):
        print(token)
