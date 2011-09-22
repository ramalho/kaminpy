//
//	The core classes for the basic lisp functions
//
# include "reader.h"
# include "function.h"

//
//	the Lisp reader adds quoted constants
//

class QuotedConst : public Expression {
private:
	Expr theValue;
public:
	QuotedConst(Expression * val)
		{ theValue = val; }

	virtual void free();
	virtual void eval(Expr &, Environment *, Environment *);
	virtual void print();
};

class LispReader : public ReaderClass {
protected:
	virtual Expression * readExpression();
};

//
//	The arithmetic functions
//

int PlusFunction(int, int);
int MinusFunction(int, int);
int TimesFunction(int, int);
int DivideFunction(int, int);

//
//	Relational functions
//

void EqualFunction(Expr &, Expression *, Expression *);
int IntEqualFunction(int, int);
int LessThanFunction(int, int);
int GreaterThanFunction(int, int);

//
//	We can do Car and Cdr because they all evaluate their arguments
//	But we can't include cons because in chap5 is ceases to evaluate
//	its arguments
//


void CarFunction(Expr &, Expression *);
void CdrFunction(Expr &, Expression *);
void ConsFunction(Expr &, Expression *, Expression *);

//
//	predicates
//

class BooleanUnary : public UnaryFunction {
private:
	int (*fun)(Expression *);
public:
	BooleanUnary(int (*thefun)(Expression *))
		{fun = thefun; }
	virtual void applyWithArgs(Expr& target, ListNode* args, Environment*);
};

int NumberpFunction(Expression *);
int SymbolpFunction(Expression *);
int ListpFunction(Expression *);
int NullpFunction(Expression *);
int PrimoppFunction(Expression *);
int ClosurepFunction(Expression *);

void PrintFunction(Expr &, Expression *);

//
//	commands
//

class DefineStatement : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

class IfStatement : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

class WhileStatement : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

class SetStatement : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

class BeginStatement : public Function {
public:
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
};


