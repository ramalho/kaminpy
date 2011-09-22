# include <std.h>
# include "list.h"
# include "function.h"
# include "environment.h"

ListNode::ListNode(Expression * car, Expression * cdr)
{
	h = car;
	t = cdr;
}

void ListNode::free()
{
	h = 0;
	t = 0;
}

int ListNode::isNil()
{
	// we are nil if we have no value and no rest of list
	return h() == 0 && t() == 0;
}

int ListNode::length()
{
	if (isNil())
		return 0;
	ListNode * cd = tail();
	if (cd)
		return 1 + cd->length();
	return 0;
}

Expression * ListNode::at(int index)
{
	if (index <= 0)
		return head();
	ListNode * cd = tail();
	if (cd)
		return cd->at(index - 1);
	return error("impossible case", "index to list at: illegal");
}

ListNode * ListNode::tail()
{
	if (!t()) {
error("impossible case", "tail on empty list??");
exit(1);
}
	ListNode * x = t()->isList();
	if (!x) error("impossible case", "tail can't find list");
	return x;
}

void ListNode::eval(Expr & target, Environment * valueops, Environment * rho)
{

	// an empty list evaluates to nil
	if (isNil()) {
		target = this;
		return;
		}

	// if first argument is a symbol, see if it is a command
	Expression * firstarg = head();
	Expression * fun = 0;
	Symbol * name = firstarg->isSymbol();
	if (name) 
		fun = valueops->lookup(name);

	// otherwise evaluate it in the given environment
	if (! fun)  {
		firstarg->eval(target, valueops, rho);
		fun = target();
		}

	// now see if it is a function
	Function * theFun = 0;
	if (fun)
		theFun = fun->isFunction();
	if (theFun) { 
		theFun->apply(target, tail(), rho);
		}
	else {
		target = error("evaluation of unknown function");
		// print();	// print out what we know
		return;
		}
}

void ListNode::print()
{
	printf("(");
	if (! isNil()) {
		// not a nil list, print elements
		h()->print();
		Expression *cd = t();
		while (cd) {
			ListNode *cdl = cd->isList();
			if (! cdl) {
				printf(" "); cd->print();
				break;
				}
			if (cdl->isNil())
				break;
			printf(" "); cdl->head()->print();
			cd = cdl->tail();	
			}
		}
	printf(")");
}

ListNode * ListNode::isList()
{	return this; }

