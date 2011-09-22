# ifndef listh
# define listh

# include "expression.h"

class ListNode : public Expression {
protected:
	Expr h;		// the head field
	Expr t;		// the tail field
public:
	ListNode(Expression *, Expression *);

	// overridden methods
	virtual void free();
	virtual void eval(Expr &, Environment *, Environment *);
	virtual void print();
	virtual ListNode * isList();

	// list specific methods
	int isNil();
	int length();
	Expression * at(int);
	virtual Expression * head() { return h(); }
	void head(Expression * x) { h = x; }
	ListNode * tail();
};

//
//	class List is used to gc list structures
//

class List : public Expr {
public:
	operator ListNode *()
		{ return val()?val()->isList():0; }
	void operator = (ListNode * r)
		{ Expr::operator = (r); }
};
# endif

