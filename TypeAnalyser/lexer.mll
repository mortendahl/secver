{

open Parser
exception Eof

}

rule token = parse
	[' ' '\t']				{ token lexbuf }    (* skip blanks *)
	| ('\r')? '\n'			{ token lexbuf }	(* skip line breaks *)
	| "(*"					{ comments 0 lexbuf }
	
	| "in"					{ IN }
	| "out"					{ OUT }
	| "new"					{ NEW }
	| "begin"				{ BEGIN }
	| "end"					{ END }
	| "ex"					{ EX }
	| '|'					{ PAR }
	| "nil"					{ NIL }
	| "if"					{ IF }
	| "then"				{ THEN }
	| "else"				{ ELSE }
	
	| "snd"					{ SND }
	| "fst"					{ FST }
	| "ok"					{ OK }
	
	| '('					{ LPAREN }
	| ')'					{ RPAREN }
	| ';'					{ SEMICOLON }
	| ','					{ COMMA }
	| '='					{ EQ }
	| '!'					{ EXCL }
	
	| (['a'-'z' 'A'-'Z']+ as s1) (['a'-'z' 'A'-'Z' '0'-'9']* as s2)		{ STR(s1 ^ s2) }
	
	| '@'					{ EOF }
	| eof					{ EOF }
	
	
and comments level = parse
	| "*)"			{ if level = 0 then token lexbuf else comments (level-1) lexbuf }
	| "(*"			{ comments (level+1) lexbuf }
	| _				{ comments level lexbuf }
	| eof			{ raise Eof }