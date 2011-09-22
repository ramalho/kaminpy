# ifndef expressionh
# define expressionh

//	forward references
class Environment;
class Expression;

class Expr {
private:
	Expression * value;

protected:
	Expression * val()
		{ return value; }

public:
	Expr(Expression * = 0);

	Expression * operator ()()
		{ return val(); }

	void operator = (Expression *);

	void evalAndPrint(Environment *, Environment *);
};

// more forward declarations
class IntegerExpression;
class Symbol;
class ListNode;
class Function;
class Environment;
class APLValue;
class Method;
class PrologValue;
class Continuation;

class Expression {
private:
	friend class Expr;
	int referenceCount;
public:
	Expression();

	virtual void free();

	// basic object protocol
	virtual void eval(Expr &, Environment *, Environment *);
	virtual void print();

	// conversion tests
	virtual Expression * touch();
	virtual IntegerExpression * isInteger();
	virtual Symbol * isSymbol();
	virtual Function * isFunction();
	virtual ListNode * isList();
	virtual Environment * isEnvironment();
	virtual APLValue * isAPLValue();
	virtual Method * isMethod();
	virtual Environment * isCluster();
	virtual PrologValue * isPrologValue();
	virtual Continuation * isContinuation();
};

class IntegerExpression : public Expression {
private:
	int value;
public:
	IntegerExpression(int v) 
		{ value = v; }

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

	int operator == (Expression *);
	int operator == (char *);
	char * chars() { return text; }
};

Expression * error(char *, char * x = 0);

# endif
