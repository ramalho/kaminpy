# ifndef readerh
# define readerh

// forward declarations
class Expression;
class IntegerExpression;
class Symbol;
class ListNode;

class ReaderClass {
public:
	Expression * promptAndRead();

protected:
	char buffer[1000];	// the input buffer
	char * p;		// current location in buffer

	// general functions
	void printPrimaryPrompt();
	void printSecondaryPrompt();
	void fillInputBuffer();
	int  isSeparator(int);
	void skipSpaces();
	void skipNewlines();

	// functions that can be overridden
	virtual Expression * readExpression();

	// specific types of expressions
	int readInteger();
	Symbol * readSymbol();
	ListNode * readList();
};

# endif
