# include <std.h>

# include "list.h"
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

initialize()
{

	// create the reader/parser 
	reader = new LispReader;

	// initialize the global environment
	Symbol * truesym = new Symbol("T");
	true = truesym;
	false = emptyList();
	Environment * genv = globalEnvironment;
	// make T evaluate to T always
	genv->add(truesym, truesym);
	genv->add(new Symbol("nil"), emptyList());

	// initialize the commands environment
	Environment * cmds = commands;
	cmds->add(new Symbol("define"), new DefineStatement);

	// initialize the value-ops environment
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
	vo->add(new Symbol("<"), new BooleanBinaryFunction(LessThanFunction));
	vo->add(new Symbol(">"), new BooleanBinaryFunction(GreaterThanFunction));
	vo->add(new Symbol("cons"), new BinaryFunction(ConsFunction));
	vo->add(new Symbol("car"), new UnaryFunction(CarFunction));
	vo->add(new Symbol("cdr"), new UnaryFunction(CdrFunction));
	vo->add(new Symbol("number?"), new BooleanUnary(NumberpFunction));
	vo->add(new Symbol("symbol?"), new BooleanUnary(SymbolpFunction));
	vo->add(new Symbol("list?"), new BooleanUnary(ListpFunction));
	vo->add(new Symbol("null?"), new BooleanUnary(NullpFunction));
	vo->add(new Symbol("print"), new UnaryFunction(PrintFunction));
}
