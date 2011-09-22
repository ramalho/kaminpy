# include <std.h>
# include "lisp.h"

extern Expr true;
extern Expr false;
extern Env valueOps;

//
//	The bodies of the common lisp stuff
//

//
//	Quoted Constants
//

void QuotedConst::free()
{	theValue = 0; }

void QuotedConst::eval(Expr &target, Environment *, Environment *)
{
	target = theValue();
}

void QuotedConst::print()
{
	printf("'"); theValue()->print();
}

Expression * LispReader::readExpression()
{
	// if quoted constant, return it,
	if ((*p == '\'') || (*p == '`')) {
		p++;
		return new QuotedConst(readExpression());
		}
	// otherwise simply return what we had before
	return ReaderClass::readExpression();
}

//
//	the Arithmetic functions
//

int PlusFunction(int a, int b) { return a + b; }
int MinusFunction(int a, int b) { return a - b; }
int TimesFunction(int a, int b) { return a * b; }
int DivideFunction(int a, int b)
{
	if (b != 0)
		return a / b;
	error("division by zero");
	return 0;
}

//
//	Relational functions
//

void EqualFunction(Expr & target, Expression * one, Expression * two)
{

	// true if both numbers and same number
	IntegerExpression * ione = one->isInteger();
	IntegerExpression * itwo = two->isInteger();
	if (ione && itwo && (ione->val() == itwo->val())) {
		target = true();
		return;
		}

	// or both symbols and same symbol
	Symbol * sone = one->isSymbol();
	Symbol * stwo = two->isSymbol();
	if (sone && stwo && (*sone == stwo)) {
		target = true();
		return;
		}

	// or both lists and both nil
	ListNode * lone = one->isList();
	ListNode * ltwo = two->isList();
	if (lone && ltwo && lone->isNil() && ltwo->isNil()) {
		target = true();
		return;
		}

	// false otherwise
	target = false();
}

int IntEqualFunction(int a, int b) { return a == b; }
int LessThanFunction(int a, int b) { return a < b; }
int GreaterThanFunction(int a, int b) { return a > b; }

//
//	Car and Cdr
//

void CarFunction(Expr & target, Expression * arg)
{
	ListNode * thelist = arg->isList();
	if (! thelist) {
		target = error("car applied to non list");
		return;
		}
	target = thelist->head()->touch();
}

void CdrFunction(Expr & target, Expression * arg)
{
	ListNode * thelist = arg->isList();
	if (! thelist) {
		target = error("car applied to non list");
		return;
		}
	target = thelist->tail()->touch();
}

void ConsFunction(Expr & target, Expression * left, Expression * right)
{
	target = new ListNode(left, right);
}

//
//	predicates
//

void BooleanUnary::applyWithArgs(Expr & target, ListNode * args, Environment *)
{
	if (fun(args->head()))
		target = true();
	else
		target = false();
}

int NumberpFunction(Expression * arg)
{
	return 0 != arg->isInteger();
}

int SymbolpFunction(Expression * arg)
{
	return 0 != arg->isSymbol();
}

int ListpFunction(Expression * arg)
{
	ListNode * x = arg->isList();
	// list? doesn't return true on nil
	if (x && x->isNil()) return 0;
	if (x) return 1;
	return 0;
}

int NullpFunction(Expression * arg)
{
	ListNode * x = arg->isList();
	return x && x->isNil();
}

int PrimoppFunction(Expression * arg)
{
	Function * funValue = arg->isFunction();
	if (funValue)	// if not closure then primitive
		if (funValue->isClosure())
			return 0;
		return 1;
	return 0;
}

int ClosurepFunction(Expression * arg)
{
	Function * funValue = arg->isFunction();
	if (funValue)
		if (funValue->isClosure())
			return 1;
	return 0;
}

void PrintFunction(Expr & target, Expression * arg)
{	
	target = arg; 
	if (target()) target()->print(); 
	printf("\n");
}

//
//	commands
//

void DefineStatement::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 3) {
		target = error("define requires three arguments");
		return;
		}
	Symbol * name = args->at(0)->isSymbol();
	if (! name) {
		target = error("define missing name");
		return;
		}

	ListNode * argNames = args->at(1)->isList();
	if (! argNames) {
		target = error("define missing arg names");
		return;
		}

	rho->add(name, new UserFunction(argNames, args->at(2), rho));

	// yield as value the name of the function
	target = name;
};


extern int isTrue(Expression *);

void IfStatement::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 3) {
		target = error("if statement requires three arguments");
		return;
		}

	Expr cond;
	args->head()->eval(cond, valueOps, rho);
	if (isTrue(cond()))
		args->at(1)->eval(target, valueOps, rho);
	else
		args->at(2)->eval(target, valueOps, rho);
	cond = 0;
}

void WhileStatement::apply(Expr & target, ListNode * args, Environment * rho)
{	Expr stmt;

	if (args->length() != 2) {
		target = error("while statement requires two arguments");
		return;
		}

	// grab the two pieces of the statement
	Expression * condexp = args->at(0);
	Expression * stexp = args->at(1);

	// then start the execution loop
	condexp->eval(target, valueOps, rho);
	while (isTrue(target())) {
		// evaluate body
		stexp->eval(stmt, valueOps, rho);
		// but ignore it
		stmt = 0;
		// then reevaluate condition
		condexp->eval(target, valueOps, rho);
		}
}

void SetStatement::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 2) {
		target = error("set statement requires two arguments");
		return;
		}

	// get the two parts
	Symbol * sym = args->at(0)->isSymbol();
	if (! sym) {
		target = error("set commands requires symbol for first arg");
		return;
		}

	// set target to value of second argument
	args->at(1)->eval(target, valueOps, rho);

	// set it in the environment
	rho->set(sym, target());
}

void BeginStatement::applyWithArgs(Expr& target, ListNode* args, 
		Environment* rho)
{
	int len = args->length();

	// yield as value the last expression
	if (len < 1)
		target = error("begin needs at least one statement");
	else
		target = args->at(len - 1);
}


