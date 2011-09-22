# include "expression.h"
# include <std.h>

//
//	class Expr - expression holders
//

Expr::Expr(Expression * val)
{
	value = val;
	if (val) value->referenceCount++;
}

void Expr::operator = (Expression * newvalue)
{
	// increment right hand side of assignment
	if (newvalue) {
		newvalue->referenceCount++;
		}

	// decrement left hand side of assignment 
	if (value) {
		value->referenceCount--;
		if (value->referenceCount = 0) {
			value->free();
			delete value;
			}
		}

	// then do the assignment
	value = newvalue;
}

void Expr::evalAndPrint(Environment * valueops, Environment * rho)
{
	Expr target = 0;

	// if we have a valid expression, evaluate it
	if (value)
		value->eval(target, valueops, rho);

	// Now if we have an expression, print it out
	if (target())
		target()->print();
	
	// force memory management
	target = 0;
}

# if 0
void Expr::print()
{
	if (value)
		value->print();
	printf("\n");
}
# endif

//
//	Expression - internal representation for expressions
//

Expression::Expression()
{
	referenceCount = 0;
}

void Expression::free()
{
	// do nothing
}

void Expression::eval(Expr & target, Environment * valueops, Environment * rho)
{
	// default is to do nothing
	target = this;
}

void Expression::print()
{
	fprintf(stderr,"in expression::print - should be subclassed\n");
}

// conversions

Expression * Expression::touch() { return this; }
IntegerExpression * Expression::isInteger() { return 0; }
Symbol * Expression::isSymbol() { return 0; }
ListNode * Expression::isList() { return 0; }
Environment * Expression::isEnvironment() { return 0; }
Function * Expression::isFunction() { return 0; }
APLValue * Expression::isAPLValue() { return 0; }
Method * Expression::isMethod() { return 0; }
Environment * Expression::isCluster() { return 0; }
PrologValue * Expression::isPrologValue() { return 0; }
Continuation * Expression::isContinuation() { return 0; }

//
//	basic objects - integers and symbols
//

# include "environment.h"

//
//	integers
//

void IntegerExpression::print()
{
	printf("%d", value);
}

IntegerExpression * IntegerExpression::isInteger()
{
	return this;
}

//
//	symbols
//

Symbol::Symbol(char * t)
{
	// make a new copy of text
	text = new char[strlen(t) + 1];
	if (! text) {
		error("allocation failure for symbol ", t);
		exit(1);
		}
	strcpy(text, t);
}

void Symbol::free()
{
	delete text;
}

void Symbol::eval(Expr & target, Environment * valueops, Environment * rho)
{
	Expression * result = rho->lookup(this);
	if (result)
		result = result->touch();
	else
		result = error("evaluation of unknown symbol: ", text);
	target = result;
}

void Symbol::print()
{
	printf("%s", text);
}

Symbol * Symbol::isSymbol()
{	return this; }

int Symbol::operator == (Expression *sym)
{	
	if (! sym) return 0;
	Symbol * s = sym->isSymbol();
	if (s)
		return 0 == strcmp(text, s->text); 
	return 0;
}

int Symbol::operator == (char *t)
{	
	return 0 == strcmp(text, t); 
}
