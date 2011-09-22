//
//	basic objects - integers and symbols
//

# include <std.h>
# include "environment.h"

//
//	integers
//

void IntegerExpression::free()
{
//	printf("deleting integer %d\n", value);
}

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
