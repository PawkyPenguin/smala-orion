grammar SimpleGrammar;

eval
	: prog
	;

prog
	: 'def' IDENTIFIER '()' '{' body '}'
	;

body
	: line*
	;

line
	: 'val' IDENTIFIER '=' expression # assignment
	| expression # expressionLine
	;

expression
	: mathexpr (BOOLOP mathexpr)? # boolexpr
	| 'if' '(' expression ')' '{' body '}' 'else' '{' body '}' # ifExpr
	;

mathexpr
	: term (GROUPOP term)?
	;

term
	: factor (RINGOP factor)*
	;

factor
	: ('-')? nsFact
	;

nsFact
	: NUMBER # numberNs
	| BOOLEAN # booleanNs
	| IDENTIFIER # value
	| '(' expression ')' # bracketNs
	;

GROUPOP
	: [+-]
	;

RINGOP
	: [/*]
	;

BOOLOP
	: ('<'|'>'|'==')
	;	

BOOLEAN
	: 'true'
	| 'false'
	;

NUMBER
	: [0-9] +
	;

VAL
	: 'val'
	;

IDENTIFIER
	: [a-zA-Z] [a-zA-Z0-9_]*
	;

EOL
	: '\r'? '\n'
	;

WS
	: [ \t\r\n] -> skip
	;
