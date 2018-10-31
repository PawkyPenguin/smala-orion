grammar SimpleGrammar;

eval
	: prog EOF
	;

prog
	: 'def ' IDENTIFIER '(' (argument (',' argument)*)? ')' body
	;

argument
	: name=IDENTIFIER '=' (case1=NUMBER|case2=BOOLEAN)
	;

body
	: '{' EOL? (line EOL)* EOL? '}'
	;

line
	: 'val' ident=IDENTIFIER '=' expression # valAssignment
	| 'var' ident=IDENTIFIER '=' expression # varAssignment
	| ident=IDENTIFIER '=' expression # assignment
	| expression # expressionLine
	;

expression
	: boolexpr1=mathexpr (boolexprOp=BOOLOP boolexpr2=mathexpr)? # boolexpr
	| 'if' '(' ifCond=expression ')' ifBody=body ('else' elseBody=body)? # ifExpr
	;

mathexpr
	: mathexpr1=term (mathexprOp=GROUPOP mathexpr2=term)?
	;

term
	: term1=factor (termOp=RINGOP term2=factor)*
	;

factor
	: NUMBER # numberNs
	| BOOLEAN # booleanNs
	| IDENTIFIER # variable
	| '(' expression ')' # bracketNs
	;

GROUPOP
	: ('+'|'-'|'||')
	;

RINGOP
	: ('/'|'*'|'&&')
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
	: [a-zA-Z]+
	;

EOL
	: '\r'? '\n'
	;

WS
	: (' ' | '\t' | '\f')+ -> skip
	;

COMMENT
	: ('//' ~ [\n\r]*) -> skip
	;
