# ifndef intsymh
# define intsymh

# include "expression.h"

class IntegerExpression : public Expression {
private:
	int value;
public:
	IntegerExpression(int v) 
		{ value = v; }

	virtual void free();
	virtual void print();
	virtual IntegerExpression * isInteger();

	int val()
		{ return value; }
};

class Symbol : public Expression {
private:
	char * text;

public:
	Symbol(char *);

	virtual void free();
	virtual void eval(Expr &, Environment *, Environment *);
	virtual void print();
	virtual Symbol * isSymbol();

	int operator == (Symbol *);
	int operator == (char *);
	char * chars() { return text; }
};

#endif

