//
//	APL Interpreter
//

# include "lisp.h"
//	uses printf from stdio package
# include <stdio.h>
//	uses isdigit from ctypes
# include <ctype.h>

extern ReaderClass * reader;
extern Env globalEnvironment;
extern Env commands;
extern Env valueOps;
extern List emptyList;


//
//	isTrue is not used, but must be defined
//
int isTrue(Expression * x)
{
	error("isTrue invoked??");
}

//
//	the datatype APLValue represents an apl type value
//

class APLValue : public Expression {
private:
	List shapedata;
	int * data;
public:
	APLValue(ListNode *, int);
	APLValue(int);	// for vectors

	// the overridden methods
	virtual APLValue * isAPLValue();
	virtual void free();
	virtual void print();

	// methods unique to apl values
	int size();
	ListNode * shape()
		{ return shapedata; }
	int shapeAt(int);
	int at(int pos)
		{ return data[pos]; }
	void atPut(int pos, int val)
		{ data[pos] = val; }
	
};

APLValue::APLValue(ListNode * s, int size)
{
	shapedata = s;
	data = new int[size];
}

APLValue::APLValue(int size)
{
	shapedata = new ListNode(new IntegerExpression(size), emptyList());
	data = new int[size];
}

APLValue * APLValue::isAPLValue()
{
	return this;
}

void APLValue::free()
{
	shapedata = 0;
	// ?? delete data;
}

void APLValue::print()
{	int i, j;

	switch(shape()->length()) {
		case 0:	// scalar values
			printf("%d", at(0));
			break;

		case 1: // vector values
			int len = size();
			for (i = 0; i < len; i++)
				printf("%d ", at(i));
			break;

		case 2:	// matrix values
			int len1 = shapeAt(0);
			int len2 = shapeAt(1);
			for (i = 0; i < len1; i++) {
				for (j = 0; j < len2; j++)
					printf("%d ", at(i*len2 + j));
				printf("\n");
				}
			break;
		default:
			printf("rank is %d\n", shape()->length());
			error("unknown rank in apl value printing");
	}
}

int APLValue::size()
{
	int sz = 1;
	for (ListNode * n = shapedata; ! n->isNil(); n = n->tail()) {
		IntegerExpression * extent = n->head()->isInteger();
		if (extent)
			sz *= extent->val();
		}
	return sz;
}

int APLValue::shapeAt(int pos)
{
	IntegerExpression * ie = shape()->at(pos)->isInteger();
	if (ie)
		return ie->val();
	error("impossible case in shapeAt");
	return 0;
}

//
//	the apl reader catches scalar values and vector values
//

class APLreader : public LispReader {
protected:
	virtual Expression * readExpression();
private:
	APLValue * readAPLscalar(int);
	APLValue * readAPLvector(int);
};

Expression * APLreader::readExpression()
{
	// see if it is a scalar value
	if ((*p == '-') && isdigit(*(p+1))) {
		p++;
		return readAPLscalar( - readInteger());
		}

	if (isdigit(*p))
		return readAPLscalar(readInteger());

	// see if it is a vector constant
	if (*p == '(') {
		p++; 
		skipNewlines();
		if (isdigit(*p))
			return readAPLvector(0);
		return readList();
		}

	// else default
	return LispReader::readExpression();
}

APLValue * APLreader::readAPLscalar(int d)
{
	// read a scalar value, but make it an apl value
	APLValue * newval = new APLValue(emptyList, 1);
	newval->atPut(0, d);
	return newval;
}

APLValue * APLreader::readAPLvector(int size)
{
	skipNewlines();

	// if at end of list, make new vector
	if (*p == ')') {
		p++;
		return new APLValue(size);
		}

	// else we better have a digit, save it and get the rest
	int sign = 1;
	if (*p == '-') { sign = -1; p++; }
	if (! isdigit(*p))
		error("ill formed apl vector constant");
	int val = sign * readInteger();
	APLValue * newval = readAPLvector(size + 1);
	newval->atPut(size, val);
	return newval;
}

//
//	the scalar functions
//
int scalarMax(int a, int b) { if (a > b) return a; else return b; }
int scalarOr(int a, int b) { return a || b; }
int scalarAnd(int a, int b) { return a && b; }
int scalarEq(int a, int b) { return a == b; }

//
//	the APL functions
//
class APLUnaryFunction : public UnaryFunction {
public:
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
	virtual void applyOp(Expr &, APLValue *);
};

void APLUnaryFunction::applyWithArgs(Expr & target, ListNode * args,
	Environment * rho)
{
	APLValue * arg1 = args->at(0)->isAPLValue();
	if (! arg1) {
		target = error("non-apl value given to unary function");
		return;
		}
	applyOp(target, arg1);
}

void APLUnaryFunction::applyOp(Expr & target, APLValue * arg)
{
	target = error("subclass should override APLUnary::applyOp");
}

class APLBinaryFunction : public BinaryFunction {
public:
	virtual void applyWithArgs(Expr &, ListNode *, Environment *);
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

void APLBinaryFunction::applyWithArgs(Expr & target, ListNode * args,
	Environment * rho)
{
	APLValue * arg1 = args->at(0)->isAPLValue();
	APLValue * arg2 = args->at(1)->isAPLValue();
	if ((! arg1) || (!arg2)) {
		target = error("non-apl value given to binary function");
		return;
		}
	applyOp(target, arg1, arg2);
}

void APLBinaryFunction::applyOp(Expr & target, APLValue * arg1, APLValue * arg2)
{
	target = error("subclass should override APLBinaryFunction");
}

//
//	the APL definitions of the scalar functions
//

class APLScalarFunction : public APLBinaryFunction {
private:
	int (*fun)(int, int);
public:
	APLScalarFunction(int (*f)(int, int))
		{ fun = f; }
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

void APLScalarFunction::applyOp(Expr& target, APLValue* left, APLValue* right)
{
	if (left->size() == 1) {	// scalar extension of left
		int extent = right->size();
		APLValue * newval = new APLValue(right->shape(), extent);
		int lvalue = left->at(0);
		while (--extent >= 0)
			newval->atPut(extent, fun(lvalue, right->at(extent)));
		target = newval;
		}
	else if (right->size() == 1) {	// scalar extension of right
		int extent = left->size();
		APLValue * newval = new APLValue(left->shape(), extent);
		int rvalue = right->at(0);
		while (--extent >= 0)
			newval->atPut(extent, fun(left->at(extent), rvalue));
		target = newval;
		}
	else {				// conforming arrays
		int extent = left->size();
		if (extent != right->size()) {
			target = error("conformance error on scalar function");
			return;
			}
		for (int i = left->shape()->length(); --i >= 0; ) 
			if (left->shapeAt(i) != right->shapeAt(i)) {
				target = 
				error("conformance error on scalar function");
				return;
				}

		APLValue * newval = new APLValue(left->shape(), extent);
		while (--extent >= 0)
			newval->atPut(extent, 
				fun(left->at(extent), right->at(extent)));
		target = newval;
		}
}

//
//	Reductions
//

class APLReduction : public APLUnaryFunction {
private:
	int (*fun)(int, int);
public:
	APLReduction(int (*f)(int, int))
		{ fun = f; }
	virtual void applyOp(Expr &, APLValue *);
};

static int lastSize(ListNode * sz)
{
	int i = sz->length();
	if (i > 0) {
		IntegerExpression * ie = sz->at(i-1)->isInteger();
		if (ie)
			return ie->val();
		}
	return 1;
}

static ListNode * removeLast(ListNode * sz)
{
	ListNode * newsz = emptyList;
	int i = sz->length()-1;
	while (--i >= 0)
		newsz = new ListNode(sz->at(i), newsz);
	return newsz;
}

void APLReduction::applyOp(Expr & target, APLValue * arg)
{
	// compute the size of the new expression
	int rowextent = lastSize(arg->shape());
	int extent = arg->size() / rowextent;
	APLValue * newval = new APLValue(removeLast(arg->shape()), extent);

	while (--extent >= 0) {
		int start = (extent + 1) * rowextent - 1;
		int newint = arg->at(start);
		for (int i = rowextent - 2; i >= 0; i--)
			newint = fun(arg->at(--start), newint);
		newval->atPut(extent, newint);
		}

	target = newval;
}

//
//	Compression function
//

class CompressionFunction : public APLBinaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

static ListNode * replaceLast(ListNode * sz, int i)
{
	ListNode *nz = new ListNode(new IntegerExpression(i), emptyList());
	for (i = sz->length() - 1; --i >= 0; )
		nz = new ListNode(sz->at(i), nz);
	return nz;
}

void CompressionFunction::applyOp(Expr& target, APLValue* left, APLValue* right)
{
	if (left->shape()->length() >= 2) {
		target = error("compression requires vector left arg");
		return;
		}
	int lsize = left->size();	// works for both scalar and vec
	int rsize = lastSize(right->shape());
	if (lsize != rsize) {
		target = error("compression conformability error");
		return;
		}
	// compute the number of non-zero values
	int i, nsize;
	nsize = 0;
	for (i = 0; i < lsize; i++)
		if (left->at(i)) nsize++;
	
	// now compute the new size
	int rextent = right->size();
	int extent = (rextent / lsize) * nsize;

	APLValue * newval = new APLValue(replaceLast(right->shape(), nsize),
				extent);

	// now fill in the values
	int index = 0;
	for (i = 0; i <= rextent; i++)
		if (left->at(i % lsize))
			newval->atPut(index++, right->at(i));
	target = newval;
}

//
//	shape function
//

class ShapeFunction : public APLUnaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *);
};

void ShapeFunction::applyOp(Expr & target, APLValue * arg)
{
	int extent = arg->shape()->length();
	ListNode * newshape = new ListNode(new IntegerExpression(extent),
			emptyList());
	APLValue * newval = new APLValue(newshape, extent);
	while (--extent >= 0) {
		IntegerExpression * ie = arg->shape()->at(extent)->isInteger();
		if (ie)
			newval->atPut(extent, ie->val());
		else
			target = error("impossible case in Shapefunction");
		}
	target = newval;
};

//
//	ravel function
//

class RavelFunction : public APLUnaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *);
};

void RavelFunction::applyOp(Expr & target, APLValue * arg)
{
	int extent = arg->size();
	APLValue * newval = new APLValue(extent);
	while (--extent >= 0) 
		newval->atPut(extent, arg->at(extent));
	target = newval;
}

//
//	the restruct function
//

class RestructFunction : public APLBinaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

void RestructFunction::applyOp(Expr & target, APLValue * left, APLValue * right)
{
	int llen = left->shape()->length();
	if (llen >= 2) {
		target = error("restruct requires vector left arg");
		return;
		}
	llen = left->size();	// works for either scalar or vector
	int extent = 1;
	ListNode * newShape = emptyList;
	while (--llen >= 0) {
		newShape = new ListNode(new IntegerExpression(left->at(llen)),
			newShape);
		extent *= left->at(llen);
		}
	APLValue * newval = new APLValue(newShape, extent);
	int rsize = right->size();
	while (--extent >= 0)
		newval->atPut(extent, right->at(extent % rsize));
	target = newval;
}

//
//	the iota (index) function
//

class IndexFunction : public APLUnaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *);
};

void IndexFunction::applyOp(Expr & target, APLValue * arg)
{
	if (arg->size() != 1) {
		target = error("index function requires scalar argument");
		return;
		}
	int extent = arg->at(0);
	APLValue * newval = new APLValue(extent);
	while (--extent >= 0)
		newval->atPut(extent, extent + 1);
	target = newval;
}

//
//	Catenation
//

class CatenationFunction : public APLBinaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

void CatenationFunction::applyOp(Expr& target, APLValue* left, APLValue* right)
{
	ListNode * lshape = left->shape();
	ListNode * rshape = right->shape();
	int llen = lshape->length();
	int rlen = rshape->length();
	if (llen <= 0 || (llen != rlen)) {
		target = error("catenation conformability error");
		return;
		}

	// get the size of the last row in each structure
	int lrow, rrow;
	IntegerExpression * ie = lshape->at(llen-1)->isInteger();
	if (ie)
		lrow = ie->val();
	else
		lrow = 1;
	ie = rshape->at(rlen-1)->isInteger();
	if (ie)
		rrow = ie->val();
	else
		rrow = 1;

	// build up the new size
	int extent = lrow + rrow;
	ListNode * newShape = new ListNode(
		new IntegerExpression(extent), emptyList());
	llen = llen - 1;
	while (--llen >= 0) {
		newShape = new ListNode(lshape->at(llen), newShape);
		ie = lshape->at(llen)->isInteger();
		if (ie)
			extent *= ie->val();
		}

	APLValue * newval = new APLValue(newShape, extent);

	// now build the new values
	int i, index, lindex, rindex;
	index = lindex = rindex = 0;
	while (index < extent) {
		for (i = 0; i < lrow; i++)
			newval->atPut(index++, left->at(lindex++));
		for (i = 0; i < rrow; i++)
			newval->atPut(index++, right->at(rindex++));
		}

	target = newval;
}

//
//	transpose
//

class TransposeFunction : public APLUnaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *);
};

void TransposeFunction::applyOp(Expr& target, APLValue * arg)
{
	// transpose of vectors or scalars does nothings
	if (arg->shape()->length() != 2) {
		target = arg;
		return;
		}

	// get the two extents
	int lim1 = arg->shapeAt(0);
	int lim2 = arg->shapeAt(1);

	// build new shapes
	ListNode * newShape =
		new ListNode(arg->shape()->at(1),
		new ListNode(arg->shape()->at(0), emptyList()));
	APLValue * newval = new APLValue(newShape, lim1 * lim2);

	// now compute the values
	for (int i = 0; i < lim2; i++)
		for (int j = 0; j < lim2; j++)
			newval->atPut(i * lim1 + j,
				arg->at(j * lim2 + i));

	target = newval;
}

//
//	subscripting
//

class SubscriptFunction : public APLBinaryFunction {
public:
	virtual void applyOp(Expr &, APLValue *, APLValue *);
};

void SubscriptFunction::applyOp(Expr& target, APLValue *left, APLValue *right)
{
	if (right->shape()->length() >= 2) {
		target = error("subscript requires vector second arg");
		return;
		}
	int rsize = right->size();
	int lsize = lastSize(left->shape());
	int extent = (left->size() / lsize) * rsize;

	APLValue * newval = new APLValue(replaceLast(left->shape(), rsize),
		extent);

	for (int i = 0; i < extent; i++)
		newval->atPut(i, left->at(
			(i / rsize) * lsize + (right->at(i % rsize)-1)));
	target = newval;
}

initialize()
{

	// initialize global variables
	reader = new APLreader;

	// initialize the statement environment
	Environment * cmds = commands;
	cmds->add(new Symbol("define"), new DefineStatement);

	// initialize the value ops environment
	Environment * vo = valueOps;
	vo->add(new Symbol("set"), new SetStatement);
	vo->add(new Symbol("+"), new APLScalarFunction(PlusFunction));
	vo->add(new Symbol("-"), new APLScalarFunction(MinusFunction));
	vo->add(new Symbol("*"), new APLScalarFunction(TimesFunction));
	vo->add(new Symbol("/"), new APLScalarFunction(DivideFunction));
	vo->add(new Symbol("max"), new APLScalarFunction(scalarMax));
	vo->add(new Symbol("or"), new APLScalarFunction(scalarOr));
	vo->add(new Symbol("and"), new APLScalarFunction(scalarAnd));
	vo->add(new Symbol("="), new APLScalarFunction(scalarEq));
	vo->add(new Symbol("<"), new APLScalarFunction(LessThanFunction));
	vo->add(new Symbol(">"), new APLScalarFunction(GreaterThanFunction));
	vo->add(new Symbol("+/"), new APLReduction(PlusFunction));
	vo->add(new Symbol("-/"), new APLReduction(MinusFunction));
	vo->add(new Symbol("*/"), new APLReduction(TimesFunction));
	vo->add(new Symbol("//"), new APLReduction(DivideFunction));
	vo->add(new Symbol("max/"), new APLReduction(scalarMax));
	vo->add(new Symbol("or/"), new APLReduction(scalarOr));
	vo->add(new Symbol("and/"), new APLReduction(scalarAnd));
	vo->add(new Symbol("compress"), new CompressionFunction);
	vo->add(new Symbol("shape"), new ShapeFunction);
	vo->add(new Symbol("ravel"), new RavelFunction);
	vo->add(new Symbol("restruct"), new RestructFunction);
	vo->add(new Symbol("cat"), new CatenationFunction);
	vo->add(new Symbol("indx"), new IndexFunction);
	vo->add(new Symbol("trans"), new TransposeFunction);
	vo->add(new Symbol("[]"), new SubscriptFunction);
	vo->add(new Symbol("print"), new UnaryFunction(PrintFunction));
}

