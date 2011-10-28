#!/usr/bin/env python
# coding: utf-8

from nose.tools import eq_, raises, assert_raises

from kamin1 import *

def setup():
    global eva
    eva = Evaluator()

def test_define_user_function():
    body = parse('(* n 2)')
    double = UserFunction('double', ['n'], body)

def test_install_function():
    func_def = parse('(define dobro (n) (* n 2))')
    eva.install_function(*func_def[1:])

@raises(ReservedIdentifier)
def test_install_function_reserved_name():
    func_def = parse('(define if (n) (* n 2))')
    eva.install_function(*func_def[1:])

def test_bind_function_args():
    body = parse('(* n 2)')
    double = UserFunction('double', ['n'], body)
    eq_(double.bind_args([3]), {'n':3})

def test_install_and_use_function():
    func_def = parse('(define dobro (n) (* n 2))')
    eva.install_function(*func_def[1:])
    expr = parse('(dobro 4)')
    eq_(eva.evaluate({}, expr), 8)

def test_install_and_use_function_arity_2():
    func_def = parse('(define mod (m n) (- m (* n (/ m n))))')
    eva.install_function(*func_def[1:])
    expr = parse('(mod 11 4)')
    eq_(eva.evaluate({}, expr), 3)

@raises(MissingArguments)
def test_install_and_use_function_arity_2_missing_arg():
    func_def = parse('(define mod (m n) (- m (* n (/ m n))))')
    eva.install_function(*func_def[1:])
    expr = parse('(mod 11)')
    eq_(eva.evaluate({}, expr), 3)

def test_install_and_use_recursive_function():
    func_def = parse('(define ! (n) (if (< n 2) 1 (* n (! (- n 1)))))')
    eva.install_function(*func_def[1:])
    expr = parse('(! 5)')
    eq_(eva.evaluate({}, expr), 120)



