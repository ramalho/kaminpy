# include "environment.h"
# include "lisp.h"

extern ReaderClass * reader;
extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;
extern List emptyList;

extern Expr true;
extern Expr false;

int isTrue(Expression * cond)
{
	// the only thing false is nil
	ListNode *nval = cond->isList();
	if (nval && nval->isNil())
		return 0;
	return 1;
}


//
//	Lambda functions - 
//

class LambdaFunction : public Function {
public:
	virtual void apply(Expr &, ListNode *, Environment *);
};

void LambdaFunction::apply(Expr & target, ListNode * args, Environment * rho)
{
	if (args->length() != 2) {
		target = error("lambda requires two arguments");
		return;
		}

	ListNode * argNames = args->head()->isList();
	if (! argNames) {
		target = error("lambda requires list of argument names");
		return;
		}

	target = new UserFunction(argNames, args->at(1), rho);
}

initialize()
{

	// initialize global variables
	reader = new LispReader;

	// initialize the value of true
	Symbol * truesym = new Symbol("T");
	true = truesym;
	false = emptyList();

	// initialize the command environment
	// there are no command or value-ops as such in scheme

	// initialize the global environment
	Environment * ge = globalEnvironment;
	ge->add(new Symbol("if"), new IfStatement);
	ge->add(new Symbol("while"), new WhileStatement);
	ge->add(new Symbol("set"), new SetStatement);
	ge->add(new Symbol("begin"), new BeginStatement);
	ge->add(new Symbol("+"), new IntegerBinaryFunction(PlusFunction));
	ge->add(new Symbol("-"), new IntegerBinaryFunction(MinusFunction));
	ge->add(new Symbol("*"), new IntegerBinaryFunction(TimesFunction));
	ge->add(new Symbol("/"), new IntegerBinaryFunction(DivideFunction));
	ge->add(new Symbol("="), new BinaryFunction(EqualFunction));
	ge->add(new Symbol("<"), new BooleanBinaryFunction(LessThanFunction));
	ge->add(new Symbol(">"), new BooleanBinaryFunction(GreaterThanFunction));
	ge->add(new Symbol("cons"), new BinaryFunction(ConsFunction));
	ge->add(new Symbol("car"), new UnaryFunction(CarFunction));
	ge->add(new Symbol("cdr"), new UnaryFunction(CdrFunction));
	ge->add(new Symbol("number?"), new BooleanUnary(NumberpFunction));
	ge->add(new Symbol("symbol?"), new BooleanUnary(SymbolpFunction));
	ge->add(new Symbol("list?"), new BooleanUnary(ListpFunction));
	ge->add(new Symbol("null?"), new BooleanUnary(NullpFunction));
	ge->add(new Symbol("primop?"), new BooleanUnary(PrimoppFunction));
	ge->add(new Symbol("closure?"), new BooleanUnary(ClosurepFunction));
	ge->add(new Symbol("print"), new UnaryFunction(PrintFunction));
	ge->add(new Symbol("lambda"), new LambdaFunction);
	ge->add(truesym, truesym);
	ge->add(new Symbol("nil"), emptyList());
}

