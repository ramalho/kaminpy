# include <std.h>
# include "lisp.h"
# include "environment.h"

extern ReaderClass * reader;
extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;
extern List emptyList;

extern Expr true;
extern Expr false;

//	isTrue reverts back to the old case where 0 is false and non-0 true
int isTrue(Expression * cond)
{
	IntegerExpression *ival = cond->isInteger();
	if (ival && ival->val() == 0)
		return 0;
	return 1;
}

//
//	a Cluster is a new statement type
//		it also uses selector and modifier functions
//

class Cluster : public Expression {
private:
	Env data;
public:
	Cluster(ListNode * names, ListNode * values)
		{ data = new Environment(names, values, 0); }
	virtual void free()
		{ data = 0; }
	virtual void print()
		{ printf("<userval>"); }
	virtual Environment * isCluster()
		{ return data; }
};

class Constructor : public Function {
private:
	List names;
public:
	Constructor(ListNode * n) 
		{ names = n; }
	virtual void free()
		{ names = 0; }
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
};

void Constructor::applyWithArgs(Expr &target, ListNode *args, Environment *rho)
{
	ListNode * nmes = names;
	if (args->length() != nmes->length()) {
		target = error("wrong number of args passed to constructor");
		return;
		}
	target = new Cluster(nmes, args);
}

class Selector : public UnaryFunction {
private:
	Expr fieldName;
public:
	Selector(Symbol * name) { fieldName = name; }
	virtual void free() { fieldName = 0; }
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
};

void Selector::applyWithArgs(Expr & target, ListNode * args, Environment * rho)
{
	Environment * x = args->head()->isCluster();
	if (! x) {
		target = error("selector given non-cluster");
		return;
		}
	Symbol *s = fieldName()->isSymbol();
	if (!s)
		error("impossible case in selector, no symbol");
	target = x->lookup(s);
	if (! target())
		error("selector cannot find symbol:", s->chars());
}

class Modifier : public BinaryFunction {
private:
	Expr fieldName;
public:
	Modifier(Symbol * name) { fieldName = name; }
	virtual void free() { fieldName = 0; }
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
};

void Modifier::applyWithArgs(Expr & target, ListNode * args, Environment * rho)
{
	Environment * x = args->head()->isCluster();
	if (! x) {
		target = error("selector given non-cluster");
		return;
		}

	// set the result to the value
	target = args->at(1);
	x->set(fieldName()->isSymbol(), target());
}

class ClusterDef : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

static void catset(Environment * rho, Symbol * left, char * mid, 
		Symbol * right, Expression * val)
{	char buffer[120];

	// catenate the two symbols
	strcpy(buffer, left->chars());
	strcat(buffer, mid);
	strcat(buffer, right->chars());

	// now put the new value into rho
	rho->add(new Symbol(buffer), val);
}

void ClusterDef::apply(Expr & target, ListNode * args, Environment * rho)
{
	Expr setprefix = new Symbol("set-");

	// must have at least name, rep and one def
	if (args->length() < 3) {
		target = error("cluster ill formed");
		return;
		}

	// get name
	Symbol * name = args->head()->isSymbol();
	args = args->tail();
	if (! name) {
		target = error("cluster missing name");
		return;
		}

	// now make the environment in which cluster will execute
	Environment * inEnv = new Environment(emptyList, emptyList, rho);

	// next part should be representation
	ListNode * rep = args->head()->isList();
	args = args->tail();
	if (! rep) {
		target = error("cluster missing rep");
		return;
		}
	Symbol *s = rep->at(0)->isSymbol();
	if (! (s && (*s == "rep"))) {
		target = error("cluster missing rep");
		return;
		}
	rep = rep->tail();

	// make the name into a constructor with the representation
	inEnv->add(name, new Constructor(rep));

	// now run dow the rep list, making accessor functions
	while (! rep->isNil()) {
		s = rep->head()->isSymbol();
		if (! s) {
			target = error("ill formed rep in cluster");
			return;
			}
		inEnv->add(s, new Selector(s));
		catset(inEnv, setprefix()->isSymbol(), "",
			s, new Modifier(s));
		rep = rep->tail();
		}
	
	// remainder should be define commands
	while (! args->isNil()) {
		ListNode * body = args->head()->isList();
		if (! body) {
			target = error("ill formed cluster");
			return;
			}
		s = body->at(0)->isSymbol();
		if (! (s && (*s == "define"))) {
			target = error("missing define in cluster");
			return;
			}
		s = body->at(1)->isSymbol();
		if (! s) {
			target = error("missing name in define");
			return;
			}

		// evaluate body to define new function
		Expr temp;
		body->eval(temp, commands, inEnv);
		// make outside form
		catset(rho, name, "$", s, inEnv->lookup(s));
		temp = 0;

		// get next function
		args = args->tail();
		}

	// what do we return?
	target = 0;
	setprefix = 0;
}

initialize()
{
	// initialize global variables
	reader = new ReaderClass;
	true = new IntegerExpression(1);
	false = new IntegerExpression(0);

	// initialize the statement environment
	Environment * cmds = commands;
	cmds->add(new Symbol("define"), new DefineStatement);
	cmds->add(new Symbol("cluster"), new ClusterDef);

	// initialize the value ops environment
	Environment * vo = valueOps;
	vo->add(new Symbol("if"), new IfStatement);
	vo->add(new Symbol("while"), new WhileStatement);
	vo->add(new Symbol("set"), new SetStatement);
	vo->add(new Symbol("begin"), new BeginStatement);
	vo->add(new Symbol("+"), new IntegerBinaryFunction(PlusFunction));
	vo->add(new Symbol("-"), new IntegerBinaryFunction(MinusFunction));
	vo->add(new Symbol("*"), new IntegerBinaryFunction(TimesFunction));
	vo->add(new Symbol("/"), new IntegerBinaryFunction(DivideFunction));
	vo->add(new Symbol("="), new BinaryFunction(EqualFunction));
	vo->add(new Symbol("<"), new IntegerBinaryFunction(LessThanFunction));
	vo->add(new Symbol(">"), new IntegerBinaryFunction(GreaterThanFunction));
	vo->add(new Symbol("print"), new UnaryFunction(PrintFunction));
}
