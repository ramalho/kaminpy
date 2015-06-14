import token as py_token

import util


class Token:

    def __init__(self, src, start, end, line):
        self.src = src
        self.start = start
        self.end = end
        self.line = line
        self.evaluate()

    def evaluate(self):
        pass

    def nud(self):
        raise SyntaxError('unexpected %r in line:\n%s' % (self.src, self.line))


class Literal(Token):

    def evaluate(self):
        self.value = int(self.src)

    def nud(self):
        return self.value


class OperatorAdd(Token):
    lbp = 10

    def led(self, left):
        right = expression(10)
        return left + right


class OperatorSub(Token):
    lbp = 10

    def led(self, left):
        right = expression(10)
        return left - right


class OperatorMul(Token):
    lbp = 20

    def led(self, left):
        right = expression(20)
        return left * right


class OperatorDiv(Token):
    lbp = 20

    def led(self, left):
        right = expression(20)
        return left / right


class End(Token):
    lbp = 0


TOKENS = {
    py_token.NUMBER: Literal,
    py_token.PLUS: OperatorAdd,
    py_token.MINUS: OperatorSub,
    py_token.STAR: OperatorMul,
    py_token.SLASH: OperatorDiv,
    py_token.ENDMARKER: End,
}


def tokenize(src):
    for token_info in util.tokenize(src):
        token_class = TOKENS[token_info.type]
        yield token_class(*token_info[1:])


token = Ellipsis


def expression(rbp=0):
    global token
    t = token
    token = next()
    left = t.nud()
    while rbp < token.lbp:
        t = token
        token = next()
        left = t.led(left)
    return left


def evaluate(src):
    """
    >>> evaluate("1 + 2")
    3
    """
    global token, next
    next = tokenize(src).__next__
    token = next()
    if isinstance(token, End):
        return None
    try:
        return expression()
    except StopIteration:
        raise SyntaxError('unexpected end of source')
