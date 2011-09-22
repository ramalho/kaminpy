# include <std.h>

# include "environment.h"

//
//	Environment - an environment is built out of two parallel lists
//

Environment::Environment(ListNode* names, ListNode* values, Environment* link)
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

Environment * Environment::isEnvironment()
{	
	return this; 
}

void Environment::set(Symbol * sym, Expression * value)
{
	ListNode * nameit = theNames;
	ListNode * valueit = theValues;

	while (! nameit->isNil()) {
		if (*sym == nameit->head()) {
			valueit->head(value);
			return;
			}
		nameit = nameit->tail();
		valueit = valueit->tail();
		}

	// otherwise see if we can find it on somebody elses list
	Environment * link = theLink;
	if (link) {
		link->set(sym, value);
		return;
		}

	// not found and we're the end of the line, just add
	add(sym, value);
}

Expression * Environment::lookup(Symbol * sym)
{
	ListNode * nameit = theNames;
	ListNode * valueit = theValues;

	while (! nameit->isNil()) {
		if (*sym == nameit->head())
			return valueit->head();
		nameit = nameit->tail();
		valueit = valueit->tail();
		}

	// otherwise see if we can find it on somebody elses list
	Environment * link = theLink;
	if (link) return link->lookup(sym);

	// not found, return nil value
	return 0;
}

void Environment::add(Symbol * s, Expression * v)
{
	theNames = new ListNode(s, (ListNode *) theNames);
	theValues = new ListNode(v, (ListNode *) theValues);
}
