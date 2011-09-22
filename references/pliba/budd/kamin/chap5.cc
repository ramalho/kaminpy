# include <stdio.h>
# include "lisp.h"
# include "environment.h"

extern ReaderClass * reader;
extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;
extern List emptyList;
extern Expr true;
extern Expr false;

int isTrue(Expression * cond)
{
	// the only thing false is nil
	ListNode *nval = cond->isList();
	if (nval && nval->isNil())
		return 0;
	return 1;
}

//
//	Thunks are unevaluated expressions
//

class Thunk : public Expression {
private:
	int evaluated;
	Expr value;
	Env context;
public:
	Thunk(Expression *, Environment *);

	virtual void free();
	virtual void print();
	virtual Expression * touch();
	virtual void eval(Expr &, Environment *, Environment *);

	virtual IntegerExpression * isInteger();
	virtual Symbol * isSymbol();
	virtual Function * isFunction();
	virtual ListNode * isList();
};

Thunk::Thunk(Expression * base, Environment *ctx) 
{
	evaluated = 0;
	value = base;
	context = ctx;
}

void Thunk::free()
{
	context = 0;
	value = 0;
}

void Thunk::print()
{
	if (evaluated)
		value()->print();
	else
		printf("...");
}

ListNode * Thunk::isList()
{
	// if its evaluated try it out
	if (evaluated) return value()->isList();

	// else it's not
	return 0;
}

Symbol * Thunk::isSymbol()
{
	if (evaluated) return value()->isSymbol();
	return 0;
}

Function * Thunk::isFunction()
{
	if (evaluated) return value()->isFunction();
	return 0;
}

IntegerExpression * Thunk::isInteger()
{
	if (evaluated) return value()->isInteger();
	return 0;
}

Expression * Thunk::touch()
{
	// if we haven't already evaluated, do it now
	if (! evaluated) {
		evaluated = 1;
		Expression * start = value();
		if (start)
			start->eval(value, valueOps, context);
		}
	Expression * val = value();
	if (val)
		return val->touch();
	return val;
}

void Thunk::eval(Expr & target, Environment * valusops, Environment * rho)
{
	touch();
	value()->eval(target, valusops, rho);
}

//
//	Cons is changed to that it produces a pair of thunks 
//	instead of evaluating its argument
//		

class SaslConsFunction : public Function {
public:
	virtual void apply(Expr & target, ListNode * args, Environment *);
};

void SaslConsFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	// check length
	if (args->length() != 2) {
		target = error("cons requires two arguments");
		return;
		}

	// make thunks for car and cdr
	target = new ListNode(new Thunk(args->at(0), rho), 
		new Thunk(args->at(1), rho));
}

//
//	User functions now need not evaluate their arguments
//

class LazyFunction : public UserFunction {
public:
	LazyFunction(ListNode * n, Expression * b, Environment * c)
		: UserFunction(n, b, c) {}
	virtual void apply(Expr &, ListNode *, Environment *);
};

//	convert arguments into thunks
static ListNode * makeThunks(ListNode * args, Environment * rho)
{
	if ((! args) || (args->isNil()))
		return emptyList;
	Expression * newcar = new Thunk(args->head(), rho);
	return new ListNode(newcar, makeThunks(args->tail(), rho));
}

void LazyFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	// number of args should match definition
	ListNode * anames = argNames;
	if (anames->length() != args->length()) {
		error("argument length mismatch");
		return;
		}

	// convert arguments into thunks
	ListNode * newargs = makeThunks(args, rho);

	// make new environment
	Env newrho = new Environment(anames, newargs, context);

	// evaluate body in new environment
	if (body())
		body()->eval(target, valueOps, newrho);
	else
		target = 0;

	newrho = 0;
}

//
//	Lambdas are redefined so as to produce lazy functions
//

class LambdaFunction : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

void LambdaFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 2) {
		target = error("lambda requires two arguments");
		return;
		}

	ListNode * argNames = args->head()->isList();

	if (! argNames) {
		target = error("lambda requires list of argument names");
		return;
		}

	target = new LazyFunction(argNames, args->at(1), rho);
}

initialize()
{

	// initialize global variables
	reader = new LispReader;

	// initialize the value of true
	Symbol * truesym = new Symbol("T");
	true = truesym;
	false = emptyList();

	// initialize the commands environment
	Environment * cmds = commands;
	cmds->add(new Symbol("set"), new SetStatement);

	// initialize the global environment
	Environment * ge = globalEnvironment;
	ge->add(new Symbol("if"), new IfStatement);
	ge->add(new Symbol("+"), new IntegerBinaryFunction(PlusFunction));
	ge->add(new Symbol("-"), new IntegerBinaryFunction(MinusFunction));
	ge->add(new Symbol("*"), new IntegerBinaryFunction(TimesFunction));
	ge->add(new Symbol("/"), new IntegerBinaryFunction(DivideFunction));
	ge->add(new Symbol("="), new BinaryFunction(EqualFunction));
	ge->add(new Symbol("<"), new BooleanBinaryFunction(LessThanFunction));
	ge->add(new Symbol(">"), new BooleanBinaryFunction(GreaterThanFunction));
	ge->add(new Symbol("cons"), new SaslConsFunction);
	ge->add(new Symbol("car"), new UnaryFunction(CarFunction));
	ge->add(new Symbol("cdr"), new UnaryFunction(CdrFunction));
	ge->add(new Symbol("number?"), new BooleanUnary(NumberpFunction));
	ge->add(new Symbol("symbol?"), new BooleanUnary(SymbolpFunction));
	ge->add(new Symbol("list?"), new BooleanUnary(ListpFunction));
	ge->add(new Symbol("null?"), new BooleanUnary(NullpFunction));
	ge->add(new Symbol("primop?"), new BooleanUnary(PrimoppFunction));
	ge->add(new Symbol("closure?"), new BooleanUnary(ClosurepFunction));
	ge->add(new Symbol("print"), new UnaryFunction(PrintFunction));
	ge->add(new Symbol("lambda"), new LambdaFunction);
	ge->add(truesym, truesym);
	ge->add(new Symbol("nil"), emptyList());
}

