# include <std.h>

# include "list.h"
# include "function.h"
# include "environment.h"
# include "lisp.h"

extern ReaderClass * reader;
extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;

extern Expr true;
extern Expr false;

int isTrue(Expression * cond)
{
	IntegerExpression *ival = cond->isInteger();
	if (ival && ival->val() == 0)
		return 0;
	return 1;
}

initialize()
{
	// initialize global variables
	reader = new ReaderClass;

	// initialize the statement environment
	Environment * cmds = commands;
	cmds->add(new Symbol("define"), new DefineStatement);

	// initialize the global environment
	Environment * vo = valueOps;
	vo->add(new Symbol("if"), new IfStatement);
	vo->add(new Symbol("while"), new WhileStatement);
	vo->add(new Symbol("set"), new SetStatement);
	vo->add(new Symbol("begin"), new BeginStatement);
	vo->add(new Symbol("+"), new IntegerBinaryFunction(PlusFunction));
	vo->add(new Symbol("-"), new IntegerBinaryFunction(MinusFunction));
	vo->add(new Symbol("*"), new IntegerBinaryFunction(TimesFunction));
	vo->add(new Symbol("/"), new IntegerBinaryFunction(DivideFunction));
	vo->add(new Symbol("="), new IntegerBinaryFunction(IntEqualFunction));
	vo->add(new Symbol("<"), new IntegerBinaryFunction(LessThanFunction));
	vo->add(new Symbol(">"), new IntegerBinaryFunction(GreaterThanFunction));
	vo->add(new Symbol("print"), new UnaryFunction(PrintFunction));
}
