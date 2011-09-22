# include <std.h>
# include "lisp.h"

extern ReaderClass * reader;

extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;
extern List emptyList;

//
//	need isTrue although not used
//
int isTrue(Expression *) { return 0; }

class PrologValue : public Expression {
private:
	Expr data;

public:
	PrologValue(Expression * d) { data = d; }

	virtual void print();
	virtual void free() { data = 0; }
	virtual void eval(Expr &, Environment *, Environment *);
	virtual Symbol * isSymbol();
	virtual PrologValue * isPrologValue() { return this; }

	int isUndefined() 			{ return data() == 0; }
	void setUndefined() 			{ data = 0; }
	PrologValue * indirectPtr();
	void setIndirect(PrologValue *v) 	{ data = v; }
};

void PrologValue::print()
{
	Symbol * s = isSymbol();
	if (s) 
		printf("%s", s->chars());
	else
		printf("unbound variable");
}

PrologValue * PrologValue::indirectPtr()
{
	if (! isUndefined())
		return data()->isPrologValue();
	return 0;
}

Symbol * PrologValue::isSymbol()
{
	PrologValue * iptr = indirectPtr();
	if (iptr)
		return iptr->isSymbol();
	if (! isUndefined())
		return data()->isSymbol();
	return 0;
}

void PrologValue::eval(Expr&target, Environment*valueOps, Environment*rho)
{
	Symbol * s = isSymbol();
	if (s) {
		char * p = s->chars();
		Expression * r = rho->lookup(s);
		if (r) {
			target = r;
			return;
			}
		// symbol is not known
		// if lower case, eval to itself
		if ((*p >= 'a') && (*p <= 'z')) {
			// symbols eval to themselves
			target = this;
			return;
			}
		// else make a new symbol
		target = new PrologValue(0);
		rho->add(s, target());
		return;
		}
	target = this;
	return;
}

//
//	reader reads prolog symbols (no longer recognized integers either)
//

class PrologReader : public ReaderClass {
protected:
	virtual Expression * readExpression();
};

Expression * PrologReader::readExpression()
{
	// it might be a list
	if (*p == '(') {
		p++;
		return readList();
		}
	
	// otherwise it must be a symbol
	return new PrologValue(readSymbol());
}

//
//	continuations are new types of expressions
//
class Continuation : public Expression {
public:
	virtual int withContinuation(Continuation *);
	virtual void print() { printf("<future>"); }
	virtual Continuation * isContinuation() { return this; }
};

static Continuation * nothing;	// the null continuation

int Continuation::withContinuation(Continuation * future)
{
	// default is to always work
	return 1;
}

// compose used in implementing and relation
class ComposeContinuation : public Continuation {
private:
	Expr left;
	Expr right;
public:
	ComposeContinuation(Expression * a, Expression * b)
		{ left = a; right = b; }
	virtual void free()
		{ left = 0; right = 0; }
	virtual int withContinuation(Continuation *);
};

int ComposeContinuation::withContinuation(Continuation * future)
{
	Continuation * a = left()->isContinuation();
	Continuation * b = right()->isContinuation();
	if ((! a) || (! b)) {
		error("and with non relations");
		return 0;
		}
	return a->withContinuation(b);
}

class AndContinuation : public Continuation {
private:
	List relArgs;
public:
	AndContinuation(ListNode * args)
		{ relArgs = args; }
	virtual void free()
		{ relArgs = 0; }
	virtual int withContinuation(Continuation *);
};

int AndContinuation::withContinuation(Continuation * future)
{
	ListNode * args;
	args = relArgs;
	Continuation * newrel = future;
	for (int i = args->length()-1; i >= 0; i--) 
		newrel = new ComposeContinuation(args->at(i), newrel);

	Expr p = newrel;	// for gc purposes
	int result = newrel->withContinuation(nothing);
	p = 0;
	return result;
}

class OrContinuation : public Continuation {
private:
	List relArgs;
public:
	OrContinuation(ListNode * args) { relArgs = args; }
	virtual void free() { relArgs = 0; }
	virtual int withContinuation(Continuation *);
};

int OrContinuation::withContinuation(Continuation * future)
{
	ListNode * args;
	// try each alternative in turn
	for (args = relArgs; ! args->isNil(); args = args->tail()) {
		Continuation * r = args->head()->isContinuation();
		if (! r) {
			error("or argument is non-relation");
			return 0;
			}
		if (r->withContinuation(future)) return 1;
		}
	// nothing worked
	return 0;
}

class UnifyContinuation : public Continuation {
private:
	Expr left;
	Expr right;
public:
	UnifyContinuation(Expression * a, Expression * b)
		{ left = a; right = b; }
	virtual void free()
		{ left = 0; right = 0; }
	virtual int withContinuation(Continuation *);
};

static int unify(PrologValue *& c, PrologValue * a, PrologValue * b)
{

	// if either one is undefined, set it to the other
	if (a->isUndefined()) {
		c = a;
		a->setIndirect(b);
		return 1;
		}
	if (b->isUndefined()) {
		c = b;
		b->setIndirect(a);
		return 1;
		}

	// if either one are indirect, run down chain
	PrologValue * indirval;
	indirval = a->indirectPtr();
	if (indirval)
		return unify(c, indirval, b);
	indirval = b->indirectPtr();
	if (indirval)
		return unify(c, a, indirval);

	// both must now be symbolic, work if the same
	c = 0;
	Symbol * as = a->isSymbol();
	Symbol * bs = b->isSymbol();
	if ((! as) || (! bs)) 
		error("impossible", "unification of non-symbols");
	else if (strcmp(as->chars(), bs->chars()) == 0)
		return 1;
	return 0;
}

int UnifyContinuation::withContinuation(Continuation * future)
{
	PrologValue * a = left()->isPrologValue();
	PrologValue * b = right()->isPrologValue();

	// the following shouldn't ever happen, but check anyway
	if ((!a) || (!b)) {
		error("impossible", "missing prolog values in unification");
		return 0;
		}

	// now try unification
	PrologValue * c = 0;
	if (unify(c, a, b) && future->withContinuation(nothing))
		return 1;

	// didn't work, undo assignment and fail
	if (c) 
		c->setUndefined();
	return 0;
}

//
//	the printing relation
//

class PrintContinuation : public Continuation {
private:
	Expr val;

public:
	PrintContinuation(Expression * x) { val = x; }
	virtual void free() { val = 0; }
	virtual int withContinuation(Continuation *);
};

int PrintContinuation::withContinuation(Continuation * future)
{
	// see if we are a symbol, if so print it out
	Symbol * s = val()->isSymbol();
	if (s) {
		printf("%s\n", s->chars());
		return future->withContinuation(nothing);
		}
	return 0;
}

//
//	the operations used when reading rules
//

class UnifyOperation : public BinaryFunction {
public:
	virtual void applyWithArgs(Expr & target, ListNode * args, 
		Environment *)
	{ target = new UnifyContinuation(args->at(0), args->at(1)); }

};

class PrintOperation : public UnaryFunction {
public:
	virtual void applyWithArgs(Expr & target, ListNode * args, 
		Environment *)
		{ target = new PrintContinuation(args->at(0)); }
};

class AndOperation : public Function {
public:
	virtual void applyWithArgs(Expr & target, ListNode * args, 
		Environment *)
		{ target = new AndContinuation(args); }
};

class OrOperation : public Function {
public:
	virtual void applyWithArgs(Expr & target, ListNode * args, 
		Environment *)
		{ target = new OrContinuation(args); }
};

//
//	the query statement is used to ask questions
//

class QueryStatement : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

void QueryStatement::apply(Expr&target, ListNode*args, Environment*rho)
{
	if (args->length() != 1) {
		target = error("wrong number of args to query");
		return;
		}

	// we make a new environment to isolate any new variables defined
	Env newrho = new Environment(emptyList, emptyList, rho);

	args->at(0)->eval(target, valueOps, newrho);

	Continuation * f = 0;
	if (target())
		f = target()->isContinuation();
	if (! f) {
		target = error("query given non-relation");
		return;
		}
	if (f->withContinuation(nothing))
		target = new Symbol("ok");
	else
		target = new Symbol("not ok");

	newrho = 0;	// force memory management
}

initialize()
{
	// create the reader/parser 
	reader = new PrologReader;

	// make the empty relation
	nothing = new Continuation;

	// make the operators that are legal inside of relations
	Environment * rops = valueOps;
	rops->add(new Symbol("print"), new PrintOperation);
	rops->add(new Symbol(":=:"), new UnifyOperation);
	rops->add(new Symbol("and"), new AndOperation);
	rops->add(new Symbol("or"), new OrOperation);

	// initialize the commands environment
	Environment * cmds = commands;
	cmds->add(new Symbol("define"), new DefineStatement);
	cmds->add(new Symbol("query"), new QueryStatement);
}
