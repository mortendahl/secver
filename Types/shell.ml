open Structure


let _ =
	try
		let lexbuf = Lexing.from_channel stdin in		
		let p = Parser.main Lexer.token lexbuf in
		
		print_string "\n*** Parsed process ***\n\n";
		formatprintpr p;
		flush stdout;
		
		let ctx = emptycontext in
		let ctx = bindname ctx "n" (TyVar("N")) in
		let ctx = bindname ctx "m" (TyVar("M")) in
		let ctx = bindname ctx "p" (TyPair(TyVar("P'"), TyVar("P'"))) in
		
		print_string "\n\n\n*** Constraint generation ***\n\n";
		let (ntv, nev, c) = genPrCnst tyvargen efvargen ctx p in
		flush stdout;

		print_string "\n\n\n*** Type constraint solving ***\n\n";
		let (ntv, nev, tysubst, c') = solveCnsts ntv nev c in
		flush stdout;
		
		print_string "\n\n\n*** Type substitution ***\n\n";
		printtysubsts tysubst;
		flush stdout;
		
		print_string "\n\n\n*** Remaining constraints ***\n\n";
		printlist printcnst c' "\n";
		flush stdout;
		
		print_string "\n\n\n*** Effect constraint solving ***\n\n";
		let efassign = solveEffects c' [] in
		flush stdout;
		
		print_string "\n\n\n*** Effect substitution ***\n\n";
		printeffectassignment efassign;
		flush stdout;
		
		print_string "\n\n\n";
		flush stdout
	with Lexer.Eof ->
		exit 0