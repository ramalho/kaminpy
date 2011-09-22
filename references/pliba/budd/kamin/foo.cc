# include <std.h>
# include "environment.h"
# include "intsym.h"
# include "list.h"

Environment * Environment::isEnvironment()
{	return this; }

void Environment::add(Symbol *, Expression *)
{
	error("Environment::add should be subclassed");
}

Expression * Environment::lookup(Symbol *)
{
	return error("Environment::lookup should be subclassed");
}

void Environment::set(Symbol *, Expression *)
{
	error("Environment::set should be subclassed");
}

void Environment::print()
{
	error("Environment::print should be subclassed");
}

//
//	Envlist - an environment built out of two parallel lists
//

Environment::Environment(ListNode * names, ListNode * values, Environment * link)
{
	theNames = names;
	theValues = values;
	theLink = link;
}

void Environment::free()
{
	theNames = 0;
	theValues = 0;
	theLink = 0;
}

void Environment::print()
{
	if (theNames()) theNames()->print(); printf("\n");
	if (theValues()) theValues()->print(); printf("\n");
	if (theLink()) theLink()->print();
}

void Environment::set(Symbol * sym, Expression * value)
{
	ListNode * nameit = theNames();
	ListNode * valueit = theValues();

	while (! nameit->isNil()) {
		if (sym->match(nameit->car()->isSymbol())) {
			valueit->car(value);
			return;
			}
		nameit = nameit->tail();
		valueit = valueit->tail();
		}

	// otherwise see if we can find it on somebody elses list
	if (theLink()) {
		theLink()->set(sym, value);
		return;
		}

	// not found and we're the end of the line, just add
	add(sym, value);
}

Expression * Environment::lookup(Symbol * sym)
{
	ListNode * nameit = theNames();
	ListNode * valueit = theValues();

	while (! nameit->isNil()) {
		if (sym->match(nameit->car()->isSymbol()))
			return valueit->car();
		nameit = nameit->tail();
		valueit = valueit->tail();
		}

	// otherwise see if we can find it on somebody elses list
	if (theLink()) return theLink()->lookup(sym);

	// not found, return nil value
	return 0;
	
}

void Environment::add(Symbol * s, Expression * v)
{
	theNames = new ListNode(s, theNames());
	theValues = new ListNode(v, theValues());
}

