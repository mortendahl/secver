%{

open Structure
open Lexer

%}



%token <string> STR
%token IN OUT NEW BEGIN END EX PAR NIL IF THEN ELSE
%token SND FST OK
%token LPAREN RPAREN SEMICOLON COMMA EQ EXCL
%token EOF

%left ELSE
%left SEMICOLON
%left PAR

%start main
%type <Structure.pr> main

%%

main:
	EOF							{ raise Eof }
	| process EOF				{ $1 }
;

process:
	IN message name SEMICOLON process					{ PrIn($2, $3, $5) }
	| EXCL IN message name SEMICOLON process			{ PrReIn($3, $4, $6) }
	| OUT message message								{ PrOut($2, $3) }
	| NEW name SEMICOLON process						{ PrNew($2, $4) }
	| EX message SEMICOLON process						{ PrEx($2, $4) }
	| BEGIN label LPAREN message RPAREN					{ PrBegin($2, $4) }
	| END label LPAREN message RPAREN					{ PrEnd($2, $4) }
	| process PAR process								{ PrPar($1, $3) }
	| LPAREN process RPAREN								{ $2 }
	| NIL												{ PrNil }
	| IF message EQ message THEN process ELSE process	{ PrIf($2, $4, $6, $8) }
;
	
message:
	name				{ MsgName($1) }
	| FST message		{ MsgFst($2) }
	| SND message		{ MsgSnd($2) }
	| OK				{ MsgOk }
	| LPAREN message COMMA message RPAREN	{ MsgPair($2, $4) }
;

name:
	STR				{ $1 }
;

label:
	STR				{ $1 }
;