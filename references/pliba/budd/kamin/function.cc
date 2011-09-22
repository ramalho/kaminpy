# include <stdio.h>
# include "environment.h"
# include "function.h"
# include "list.h"

extern Env valueOps;

extern Expr true;
extern Expr false;

Function * Function::isFunction()
{	return this; }

void Function::print()
{	printf("<closure>"); }

int Function::isClosure()
{	return 0; }

// default behavior for function applications is to evaluate
// arguments

static ListNode * evalArgs(ListNode * args, Environment * rho)
{
	if (args->isNil())
		return args;
	Expr newhead;
	Expression * first = args->head();
	first->eval(newhead, valueOps, rho);
	return new ListNode(newhead(), evalArgs(args->tail(), rho));
	newhead = 0; 	// force garbage collection
}

void Function::apply(Expr & target, ListNode * args, Environment * rho)
{
	List newargs = evalArgs(args, rho);
	applyWithArgs(target, newargs, rho);
	newargs = 0;	// force garbage collection
}

void Function::applyWithArgs(Expr & target, ListNode * args, Environment * rho)
{
	target = error("in function::applywithargs, should be overridden");
}

//
//	Unary functions take only one argument
//

void UnaryFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 1) {
		error("unary function given more than one argument");
		target = 0;
		return;
	}
	Function::apply(target, args, rho);
}

void UnaryFunction::applyWithArgs(Expr& target, ListNode* args, Environment*rho)
{
	if (! fun)
		target = error("unaryfunction apply and no function");
	else
		fun(target, args->at(0));
}

//
//	Binary functions take two arguments
//

void BinaryFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 2) {
		error("binary function given more than one argument");
		target = 0;
		return;
	}
	Function::apply(target, args, rho);
}

void BinaryFunction::applyWithArgs(Expr& target, ListNode* args, Environment*rho)
{
	if (! fun)
		target = error("binary function apply and no function");
	else
		fun(target, args->at(0), args->at(1));
}

//
//	Integer Binary Functions
//

void IntegerBinaryFunction::applyWithArgs(Expr & target, ListNode * args, Environment * rho)
{
	Expression * left = args->at(0);
	Expression * right = args->at(1);
	if ((! left->isInteger()) || (! right->isInteger())) {
		target = error("arithmetic function with nonint args");
		return;
		}
	
	target =  new IntegerExpression(
		fun(left->isInteger()->val(), right->isInteger()->val()));
}

//
//	Boolean Binary Functions
//

void BooleanBinaryFunction::applyWithArgs(Expr & target, ListNode * args, Environment * rho)
{
	Expression * left = args->at(0);
	Expression * right = args->at(1);
	if ((! left->isInteger()) || (! right->isInteger())) {
		error("arithmetic function with nonint args");
		return;
		}
	
	if (fun(left->isInteger()->val(), right->isInteger()->val())) 
		target = true();
	else 
		target = false();
}

//
//	user functions have argument names and body
//

UserFunction::UserFunction(ListNode * anames, Expression * bod, Environment * ctx)
{
	argNames = anames;
	body = bod;
	context = ctx;
}

void UserFunction::free()
{
	argNames = 0;
	body = 0;
	context = 0;
}

int UserFunction::isClosure()
{
	return 1;
}

void UserFunction::applyWithArgs(Expr& target, ListNode* args, Environment* rho)
{
	// number of args should match definition
	ListNode *an = argNames;
	if (an->length() != args->length()) {
		error("argument length mismatch");
		return;
		}

	// make new environment
	Env newrho; 
	newrho = new Environment(an, args, context);

	// evaluate body in new environment
	Expression * bod = body();
	if (bod)
		bod->eval(target, valueOps, newrho);
	else
		target = 0;

	newrho = 0;	// force garbage collection
}

