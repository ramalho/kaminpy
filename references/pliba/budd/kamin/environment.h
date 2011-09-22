# ifndef environment
# define environment

# include "list.h"

//
//	Env is a shadow to Expr to perform memory management
//

class Env : public Expr {
public:
	operator Environment * ();
	void operator = (Environment * r);
};

//	forward declarations
class Symbol;

class Environment : public Expression {
private:
	List theNames;
	List theValues;
	Env theLink;

public:
	Environment(ListNode *, ListNode *, Environment *);

	// overridden methods
	virtual void free();
	virtual Environment * isEnvironment();

	// new methods
	Expression * lookup(Symbol *);
	void add(Symbol *, Expression *);
	void set(Symbol *, Expression *);
};

//
//	now define the methods for class Env
//

inline Env::operator Environment * ()
{ return val()?val()->isEnvironment():0; }

inline void Env::operator = (Environment * r)
{ Expr::operator = (r); }

# endif

