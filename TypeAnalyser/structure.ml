exception Error of string


(* ****** Constructs ****** *)

type name = string
type label = string
type hole = int

type msg =		(* Message *)
	MsgPair of msg * msg
	| MsgFst of msg
	| MsgSnd of msg
	| MsgName of name
	| MsgOk
	| MsgHole of hole
	
type subst =	(* Substitution *)
	msg * hole

type aef =		(* Atomic Effect *)
	label * msg
	
type ef =		(* Effect *)
	EfVar of string
	| EfAtoms of aef list
	| EfSubst of ef * subst
	
type ty =		(* Type *)
	TyVar of string
	| TyBase of string
	| TyCh of ty
	| TyPair of ty * ty
	| TyOk of ef
	| TySubst of ty * subst
	
type pr =		(* Process *)
	PrIn of msg * name * pr
	| PrReIn of msg * name * pr
	| PrOut of msg * msg
	| PrNew of name * pr
	| PrPar of pr * pr
	| PrNil
	| PrIf of msg * msg * pr * pr
	| PrEx of msg * pr
	| PrBegin of label * msg
	| PrEnd of label * msg

	
type tysubst =	(* Type substitution *)
	string * ty
	

	
	
(* ****** Constraints ****** *)

type cnst =
	CnstTypeEq of ty * ty
	| CnstEffectEq of ef * ef
	| CnstEffectIn of aef * (ef list)		(* inclusion: "begin l(m) in E,F" *)
	| CnstEffectSub of ef * (ef list)		(* subset: E subset G,H  *)


	

(* ****** Printing ****** *)

let printlist printfun eles seperator =
	let rec helper printfun eles prefix =
		match eles with
			[] -> ()
			| ele::rest ->
				print_string prefix;
				printfun ele;
				helper printfun rest seperator
	in
	helper printfun eles ""

let rec printname n =
	print_string n
	
let rec printlabel l =
	print_string l

let rec printmsg msg =
	match msg with
		MsgPair(m1, m2) ->
			print_string "(";
			printmsg m1;
			print_string ", ";
			printmsg m2;
			print_string ")"
		| MsgFst(m1) ->
			print_string "fst ";
			printmsg m1
		| MsgSnd(m2) ->
			print_string "snd ";
			printmsg m2
		| MsgName(n) ->
			printname n
		| MsgOk ->
			print_string "ok"
		| MsgHole(h) ->
			print_int h
			
let rec printsubst (m, h) =
	print_string "{";
	printmsg m;
	print_string "/";
	print_int h;
	print_string "}"
	
let printatomiceffect (l,m) =
	print_string l; 
	print_string "("; 
	printmsg m;
	print_string ")"
	
let rec printeffect ef =
	match ef with
		EfVar(e) -> 
			print_string e
		| EfAtoms(aefs) -> 
			print_string "{"; 
			printlist printatomiceffect aefs ",";
			print_string "}"
		| EfSubst(ef1, subst1) -> 
			printeffect ef1;
			printsubst subst1

let rec printtype ty =
	match ty with
		TyVar(v) ->
			print_string v
		| TyBase(b) ->
			print_string b
		| TyCh(ty1) ->
			print_string "Ch(";
			printtype ty1;
			print_string ")"
		| TyPair(ty1, ty2) ->
			print_string "Pair(";
			printtype ty1;
			print_string ", ";
			printtype ty2;
			print_string ")"
		| TyOk(ef) ->
			print_string "Ok(";
			printeffect ef;
			print_string ")"
		| TySubst(ty1, subst1) ->
			printtype ty1;
			printsubst subst1
			
let rec printpr pr = 
	match pr with
		PrIn(n, x, p) ->
			print_string "in ";
			printmsg n;
			print_string " ";
			print_string x;
			print_string "; ";
			printpr p
		| PrReIn(n, x, p) ->
			print_string "!in ";
			printmsg n;
			print_string " ";
			print_string x;
			print_string "; ";
			printpr p
		| PrOut(n, m) ->
			print_string "out ";
			printmsg n;
			print_string " ";
			printmsg m
		| PrNew(n, p) ->
			print_string "new ";
			print_string n;
			print_string "; ";
			printpr p
		| PrPar(p1, p2) ->
			printpr p1;
			print_string " | ";
			printpr p2
		| PrNil ->
			print_string "nil"
		| PrIf(m1, m2, p, q) ->
			print_string "if ";
			printmsg m1;
			print_string " = ";
			printmsg m2;
			print_string " then ";
			printpr p;
			print_string " else ";
			printpr q
		| PrEx(m, p) ->
			print_string "ex ";
			printmsg m;
			print_string "; ";
			printpr p
		| PrBegin(l, m) ->
			print_string "begin ";
			print_string l;
			print_string "(";
			printmsg m;
			print_string ")"
		| PrEnd(l, m) ->
			print_string "end ";
			print_string l;
			print_string "(";
			printmsg m;
			print_string ")"
						
let rec printcnst cnst =
	match cnst with
		CnstTypeEq(ty1, ty2) ->
			printtype ty1;
			print_string " = ";
			printtype ty2
		| CnstEffectEq(ef1, ef2) ->
			printeffect ef1;
			print_string " = ";
			printeffect ef2
		| CnstEffectIn(aef, efs) ->
			let (l, m) = aef in
			print_string l; 
			print_string "("; 
			printmsg m;
			print_string ") in ";
			printlist printeffect efs ", "
		| CnstEffectSub(ef, efs) ->
			printeffect ef;
			print_string " <= ";
			printlist printeffect efs ", "
		

let rec printtysubsts tysubsts =
	printlist (fun (s, ty) -> 
				print_string s; 
				print_string " -> "; 
				printtype ty) tysubsts "\n"
	
	
	
	
(* ****** Format Printing ****** *)

let formatprintpr pr = 
	let rec helper indent pr = 
		match pr with
			PrIn(n, x, p) ->
				print_string indent;
				print_string "in ";
				printmsg n;
				print_string " ";
				printname x;
				print_string "; \n";
				helper (indent^"  ") p
			| PrReIn(n, x, p) ->
				print_string indent;
				print_string "!in ";
				printmsg n;
				print_string " ";
				printname x;
				print_string "; \n";
				helper (indent^"  ") p
			| PrOut(n, m) ->
				print_string indent;
				print_string "out ";
				printmsg n;
				print_string " ";
				printmsg m
			| PrNew(n, p) ->
				print_string indent;
				print_string "new ";
				print_string n;
				print_string "; \n";
				helper (indent^"  ") p
			| PrPar(p1, p2) ->
				helper indent p1;
				print_string " | \n";
				helper indent p2
			| PrNil ->
				print_string indent;
				print_string "nil"
			| PrIf(m1, m2, p, q) ->
				print_string indent;
				print_string "if ";
				printmsg m1;
				print_string " = ";
				printmsg m2;
				print_string " then \n";
				helper (indent^"  ") p;
				print_string ("\n" ^ indent);
				print_string "else \n";
				helper (indent^"  ") q
			| PrEx(m, p) ->
				print_string indent;
				print_string "ex ";
				printmsg m;
				print_string "; \n";
				helper indent p
			| PrBegin(l, m) ->
				print_string indent;
				print_string "begin ";
				printlabel l;
				print_string "(";
				printmsg m;
				print_string ")"
			| PrEnd(l, m) ->
				print_string indent;
				print_string "end ";
				printlabel l;
				print_string "(";
				printmsg m;
				print_string ")"
	in
	helper "" pr 

	
	
	
(* ****** Helpers ****** *)

type binding =
	BindName of name * ty
	| BindEffect of ef

let emptycontext = []

let bindname ctx n ty = 
	(BindName(n, ty))::ctx
	
let bindeffect ctx ef =
	(BindEffect(ef))::ctx

let rec getTypeFromContext ctx n =
	match ctx with
		[] -> raise (Error("getTypeFromContext: could not find name '"^ n ^ "' in context"))
		| BindName(n1, ty)::xs ->
			if (=) (String.uppercase n1) (String.uppercase n) then ty
			else getTypeFromContext xs n
		| BindEffect(_)::xs ->
			getTypeFromContext xs n

let rec nameOccursIn x m =
	match m with
		MsgName(n) ->
			(=) x n
		| MsgOk ->
			false
		| MsgFst(m1) ->
			nameOccursIn x m1
		| MsgSnd(m2) ->
			nameOccursIn x m2
		| MsgPair(m1, m2) ->
			nameOccursIn x m1 || nameOccursIn x m2
		| MsgHole(h) ->
			raise (Error("nameOccursIn: should not occur"))
			
let rec effects ctx =
	match ctx with
		[] -> []
		| BindName(_)::rest -> effects rest
		| BindEffect(ef)::rest ->
			ef::(effects rest)
						
let rec begins p =
	match p with
		PrBegin(l, m) ->
			[(l, m)]
		| PrEnd(l, m) ->
			[]
		| PrNew(a, p1) ->
			let unfiltered = begins p1 in
			let filtered = List.filter 
				(fun (_, m) -> not (nameOccursIn a m)) 
				unfiltered in		(* enforces 'a \not \in fv(S)' *)
			filtered
		| PrPar(p1, p2) ->
			(begins p1) @ (begins p2)
		| _ -> []
		
let rec msgeq m1 m2 =
	(* TODO: are closures captured? (fst fst ..) = .. ? *)
	if (=) m1 m2 then true
	else
		match (m1, m2) with
			(MsgFst(MsgPair(m11, _)), _) ->
				msgeq m11 m2
			| (_, MsgFst(MsgPair(m21, _))) ->
				msgeq m1 m21
			| (MsgSnd(MsgPair(_, m12)), _) ->
				msgeq m12 m2
			| (_, MsgSnd(MsgPair(_, m22))) ->
				msgeq m1 m22
			| _ -> false



type nextuvar = NextUVar of string * uvargenerator
and	uvargenerator = unit -> nextuvar
let tyvargen = let rec f n () = NextUVar("T_" ^ string_of_int n, f (n+1)) in f 0
let efvargen = let rec f n () = NextUVar("E_" ^ string_of_int n, f (n+1)) in f 0



(* Analysis *)

let shiftSubst (m, h) = (m, h+1)

let rec genMsgCnst ntv nev ctx msg =
	match msg with
		MsgName(n) ->
			let ty = getTypeFromContext ctx n in
			
			printmsg msg;
			print_string " : ";
			printtype ty;
			print_string "\n";
			
			(ntv, nev, ty, [])
			
		| MsgFst(m) ->
			let (ntv, nev, x, c1) = genMsgCnst ntv nev ctx m in
			
			let NextUVar(n1, ntv) = ntv() in let x1 = TyVar(n1) in
			let NextUVar(n2, ntv) = ntv() in let x2 = TyVar(n2) in
			
			let c = (CnstTypeEq(x, TyPair(x1, x2)))::c1 in
			
			printmsg msg;
			print_string " : ";
			printtype x1;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, x1, c)
			
		| MsgSnd(m) ->
			let (ntv, nev, x, c1) = genMsgCnst ntv nev ctx m in
			
			let NextUVar(n1, ntv) = ntv() in let x1 = TyVar(n1) in
			let NextUVar(n2, ntv) = ntv() in let x2 = TyVar(n2) in
			let NextUVar(n2', ntv) = ntv() in let x2' = TyVar(n2') in
			
			let c = (CnstTypeEq(x, TyPair(x1, x2)))::c1 in
			let c = (CnstTypeEq(x2', TySubst(x2, (MsgFst(m), 1))))::c in
			
			printmsg msg;
			print_string " : ";
			printtype x2';
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, x2', c)
			
		| MsgPair(m1, m2) ->
			let (ntv, nev, x1, c1) = genMsgCnst ntv nev ctx m1 in
			let (ntv, nev, x2, c2) = genMsgCnst ntv nev ctx m2 in
			
			let NextUVar(n, ntv) = ntv() in let x = TyVar(n) in
			let NextUVar(n2', ntv) = ntv() in let x2' = TyVar(n2') in
			
			let c = (CnstTypeEq(x, TyPair(x1, x2')))::(c1 @ c2) in
			let c = (CnstTypeEq(x2, TySubst(x2', (m1, 1))))::c in
			
			printmsg msg;
			print_string " : ";
			printtype x;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, x, c)
			
		| MsgOk ->
			let NextUVar(n, ntv) = ntv() in let x = TyVar(n) in
			let NextUVar(ne, nev) = nev() in let e = EfVar(ne) in
			
			let c = [CnstEffectSub(e, effects ctx); CnstTypeEq(x, TyOk(e))] in
			
			printmsg msg;
			print_string " : ";
			printtype x;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, x, c)
		
		| MsgHole(_) ->
			raise (Error("genMsgCnst: holes should not occur in these messages"))

let rec genPrCnst ntv nev ctx pr =
	match pr with
		PrIn(n, x, p) ->
			let (ntv, nev, tyn, c1) = genMsgCnst ntv nev ctx n in
			let NextUVar(tyxn, ntv) = ntv() in let tyx = TyVar(tyxn) in
			let (ntv, nev, c2) = genPrCnst ntv nev (bindname ctx x tyx) p in
			
			let c = (CnstTypeEq(tyn, TyCh(tyx)))::(c1 @ c2) in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrReIn(n, x, p) ->
			let (ntv, nev, tyn, c1) = genMsgCnst ntv nev ctx n in
			let NextUVar(tyxn, ntv) = ntv() in let tyx = TyVar(tyxn) in
			let (ntv, nev, c2) = genPrCnst ntv nev (bindname ctx x tyx) p in
			
			let c = (CnstTypeEq(tyn, TyCh(tyx)))::(c1 @ c2) in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrOut(n, m) ->
			let (ntv, nev, tyn, c1) = genMsgCnst ntv nev ctx n in
			let (ntv, nev, tym, c2) = genMsgCnst ntv nev ctx m in
			
			let c = (CnstTypeEq(tyn, TyCh(tym)))::(c1 @ c2) in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrPar(p1, p2) ->
			let p1begins = begins p1 in
			let p2begins = begins p2 in
			let p1ctx = if (!=) [] p2begins then bindeffect ctx (EfAtoms(p2begins)) else ctx in
			let p2ctx = if (!=) [] p1begins then bindeffect ctx (EfAtoms(p1begins)) else ctx in
		
			let (ntv, nev, c1) = genPrCnst ntv nev p1ctx p1 in
			let (ntv, nev, c2) = genPrCnst ntv nev p2ctx p2 in
			
			let c = c1 @ c2 in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrNil ->
			let c = [] in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, [])
			
		| PrEx(m, p) ->
			let (ntv, nev, tym, c1) = genMsgCnst ntv nev ctx m in
			let NextUVar(en, nev) = nev() in let e = EfVar(en) in
			let (ntv, nev, c2) = genPrCnst ntv nev (bindeffect ctx e) p in
			
			let c = (CnstTypeEq(tym, TyOk(e)))::(c1 @ c2) in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrBegin(_, m) ->
			let (ntv, nev, tym, c) = genMsgCnst ntv nev ctx m in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrEnd(l, m) ->
			let (ntv, nev, tym, c1) = genMsgCnst ntv nev ctx m in
			
			let c = (CnstEffectIn((l, m), effects ctx))::c1 in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
		| PrNew(n, p) ->			
			let NextUVar(tynn, ntv) = ntv() in let tyn = TyVar(tynn) in
			let NextUVar(tychvarn, ntv) = ntv() in let tych = TyCh(TyVar(tychvarn)) in		(* generative type *)
			let (ntv, nev, c1) = genPrCnst ntv nev (bindname ctx n tyn) p in
			
			let c = (CnstTypeEq(tyn, tych))::c1 in
			
			printpr pr;
			print_string "   - ";
			printlist printcnst c ", ";
			print_string "\n";
			
			(ntv, nev, c)
			
			
			
let rec reduceMessage m subst =
	match m with
		MsgPair(m1, m2) -> MsgPair(reduceMessage m1 subst, reduceMessage m2 subst)
		| MsgFst(m1) -> MsgFst(reduceMessage m1 subst)
		| MsgSnd(m2) -> MsgSnd(reduceMessage m2 subst)
		| MsgName(_) -> m
		| MsgOk -> m
		| MsgHole(h) -> 
			let (m', h') = subst in
			if (=) h h' then m' else m
			
let rec reduceEffect ef =
	let rec helper substs ef =
		match ef with
			EfVar(_) ->
				List.fold_left (fun ef subst -> EfSubst(ef, subst)) ef substs
			| EfAtoms(aefs) ->
				EfAtoms(List.map 
					(fun (l, m) -> 
						let reduced = List.fold_left (fun m' subst -> reduceMessage m' subst) m substs in
						(l, reduced))
					aefs)
						
			| EfSubst(ef', subst) ->
				helper (subst::substs) ef'
	in
	helper [] ef
		
let rec reduceType ty =
	let rec helper substs ty =
		match ty with 
			TyVar(_) -> 
				List.fold_left (fun ty subst -> TySubst(ty, subst)) ty substs
			| TyBase(_) -> ty
			| TyCh(ty1) -> TyCh(helper substs ty1)
			| TyPair(ty1, ty2) ->
				let ty2substs = List.map (fun (m, h) -> (m, h+1)) substs in
				TyPair(helper substs ty1, helper ty2substs ty2)
			| TyOk(ef1) -> 
				let ef1' = List.fold_left (fun ef subst -> EfSubst(ef, subst)) ef1 substs in
				TyOk(reduceEffect ef1')
			| TySubst(ty', subst) ->
				helper (subst::substs) ty'
	in
	helper [] ty

(*	
let rec reduceType ty =
	match ty with
		TySubst(ty', subst) ->
			(match ty' with
				TyVar(_) -> ty
				| TyBase(_) -> ty'
				| TyCh(ty1) -> TyCh(reduceType (TySubst(ty1, subst)))
				| TyPair(ty1, ty2) -> TyPair(reduceType (TySubst(ty1, subst)), reduceType (TySubst(ty2, shiftSubst subst)))
				| TyOk(ef1) -> TyOk(reduceEffect (EfSubst(ef1, subst)))
				| TySubst(_) -> reduceType (TySubst(reduceType ty', subst))
			)
		| _ -> ty
*)
	
let rec cloneEffect nev ef =
	match ef with
		EfVar(_) ->
			let NextUVar(en, nev) = nev() in (nev, EfVar(en))
		| EfAtoms(_) -> (nev, ef)
		| EfSubst(ef1, subst) ->
			let (nev, clone1) = cloneEffect nev ef1 in
			(nev, clone1)	(* remove subst from clone - old: (nev, EfSubst(clone1, subst)) *)
	
let rec cloneType ntv nev ty =
	match ty with
		TyVar(_) ->
			let NextUVar(tyn, ntv) = ntv() in (ntv, nev, TyVar(tyn))
		| TyBase(_) -> (ntv, nev, ty)
		| TyCh(ty1) -> 
			let (ntv, nev, clone) = cloneType ntv nev ty1 in
			(ntv, nev, TyCh(clone))
		| TyPair(ty1, ty2) ->
			let (ntv, nev, clone1) = cloneType ntv nev ty1 in
			let (ntv, nev, clone2) = cloneType ntv nev ty2 in
			(ntv, nev, TyPair(clone1, clone2))
		| TyOk(ef1) ->
			let (nev, clone1) = cloneEffect nev ef1 in 
			(ntv, nev, TyOk(clone1))
		| TySubst(ty1, subst) ->
			let (ntv, nev, clone1) = cloneType ntv nev ty1 in
			(ntv, nev, clone1)	(* remove subst from clone - old: (ntv, nev, TySubst(clone1, subst)) *)
			
			
			
			
let rec substtype tysubst ty =
	let rec helper varname replacetype ty =
		match ty with
			TyVar(t) ->
				if (=) t varname then replacetype else ty
			| TyCh(ty1) ->
				TyCh(helper varname replacetype ty1)
			| TyPair(ty1, ty2) ->
				TyPair(helper varname replacetype ty1, helper varname replacetype ty2)
			| TySubst(ty1, msgsubst1) ->
				TySubst(helper varname replacetype ty1, msgsubst1)
			| _ -> ty
	in 
	List.fold_left 
		(fun ty (varname, replacetype) -> helper varname replacetype ty)
		ty
		tysubst

let rec substtypecnst tysubst cnst =
	match cnst with
		CnstTypeEq(ty1, ty2) ->
			let ty1' = substtype tysubst ty1 in
			let ty2' = substtype tysubst ty2 in
			CnstTypeEq(ty1', ty2')
		| _ -> cnst	

let rec underlyingtyvar ty =
	match ty with 
		TyVar(x) -> x
		| TySubst(ty', _) -> underlyingtyvar ty'
		| _ -> raise (Error("underlyingtyvar: should not occur"))
		
let rec istyvar ty =
	match ty with
		TyVar(_) -> true
		| _ -> false
		
let rec istysubst ty =
	(* NOTE: if ty is reduced it is a tysubst if and only if there's a TyVar at the bottom *)
	match ty with
		TySubst(_) -> true
		| _ -> false
		
let rec occursin x ty =
	match ty with
		TyVar(x') ->
			(=) x x'
		| TyBase(_) ->
			false
		| TyCh(ty1) ->
			occursin x ty1
		| TyPair(ty1, ty2) ->
			occursin x ty1 || occursin x ty2
		| TyOk(_) ->
			false
		| TySubst(ty1, _) ->
			occursin x ty1


let rec solveCnsts ntv nev cnsts =
	let rec pickTyCnstToSolve cnsts =
		match cnsts with
			[] -> (None, [])	
			| cnst::rest ->
				(match cnst with
					| CnstTypeEq(ty1, ty2) ->
						let (ty1, ty2) = (reduceType ty1, reduceType ty2) in
						(* ignore if tysubsts on both sides or circular tyvar = tysubst *)
						if (istysubst ty1 && istysubst ty2)
							|| (istysubst ty1 && istyvar ty2 && (=) (underlyingtyvar ty1) (underlyingtyvar ty2))
							|| (istyvar ty1 && istysubst ty2 && (=) (underlyingtyvar ty1) (underlyingtyvar ty2)) 
						then
							let (picked, rest) = pickTyCnstToSolve rest in
							(picked, cnst::rest)
						else
							(Some (ty1, ty2), rest)
					| _ ->
						let (picked, rest) = pickTyCnstToSolve rest in
						(picked, cnst::rest)
				)
	in
	let (cnst, rest) = pickTyCnstToSolve cnsts in
	match cnst with
		None -> (ntv, nev, [], rest)
		| Some (ty1, ty2) ->
			(* NOTE: pickTyCnst guarantees (if != None)
				* ty1 and ty2 are reduced
				* one of them is not a TySubst and no circular under subst *)
			printtype ty1;
			print_string "  =  ";
			printtype ty2;
			print_string "\n";
			flush stdout;
			(match (ty1, ty2) with
				(TyVar(x), TyVar(y)) ->
					if (=) x y then solveCnsts ntv nev rest
					else 
						let rest = List.map (substtypecnst [(x, ty2)]) rest in
						let (ntv, nev, tysubst, rest) = solveCnsts ntv nev rest in
						(ntv, nev, (x, ty2)::tysubst, rest)
						
				| (TyVar(x), _) ->
					if occursin x ty2 then raise (Error("circular"))
					else
						let rest = List.map (substtypecnst [(x, ty2)]) rest in
						let (ntv, nev, tysubst, rest) = solveCnsts ntv nev rest in
						(ntv, nev, (x, ty2)::tysubst, rest)
						
				| (_, TyVar(x)) ->
					if occursin x ty1 then raise (Error("circular"))
					else
						let rest = List.map (substtypecnst [(x, ty1)]) rest in
						let (ntv, nev, tysubst, rest) = solveCnsts ntv nev rest in
						(ntv, nev, (x, ty1)::tysubst, rest)
						
				| (TyBase(a1), TyBase(a2)) ->
					if (!=) a1 a2 then raise (Error("unifyTypes: not same base type"))
					else
						solveCnsts ntv nev rest
						
				| (TyCh(ty1'), TyCh(ty2')) ->
					let newcnsts = [CnstTypeEq(ty1', ty2')] in
					solveCnsts ntv nev (newcnsts @ rest)
					
				| (TyPair(ty1', ty1''), TyPair(ty2', ty2'')) ->
					let newcnsts = [CnstTypeEq(ty1', ty2'); CnstTypeEq(ty1'', ty2'')] in
					solveCnsts ntv nev (newcnsts @ rest)
					
				| (TyOk(ef1'), TyOk(ef2')) ->
					let newcnsts = [CnstEffectEq(ef1', ef2')] in
					solveCnsts ntv nev (newcnsts @ rest)
					
				| (TySubst(_), TySubst(_)) ->
					raise (Error("should not occur"))
					
				| (TySubst(_), _) ->
					let x = underlyingtyvar ty1 in
					if occursin x ty2 then raise (Error("circular"))
					else
						let (ntv, nev, ty2clone) = cloneType ntv nev ty2 in
						let newcnsts = [CnstTypeEq(TyVar(x), ty2clone); CnstTypeEq(ty1, ty2)] in
						solveCnsts ntv nev (newcnsts @ rest)
					
				| (_, TySubst(_)) ->
					let x = underlyingtyvar ty2 in
					if occursin x ty1 then raise (Error("circular"))
					else
						let (ntv, nev, ty1clone) = cloneType ntv nev ty1 in
						let newcnsts = [CnstTypeEq(TyVar(x), ty1clone); CnstTypeEq(ty1, ty2)] in
						solveCnsts ntv nev (newcnsts @ rest)
				
				| _ -> raise (Error("incompatible types"))
			)









exception Unsatisfiable of string

(* TODO: change impl to always use sets for effects? *)

module OrderedAef = 
	struct
		type t = label * msg
		let compare (l1, m1) (l2, m2) =
			if (=) l1 l2 then
				(* note: m1 !eq m2 => m1 != m2 *)
				if msgeq m1 m2 then 0
				else compare m1 m2
			else compare l1 l2
	end

module SetAef = Set.Make (OrderedAef)

type efname =
	string

type efsubst = 
	efname * SetAef.t
	
type efassign =
	efsubst list

	
let rec printeffectassignment efassign =
	printlist (fun (efname, aefset) ->
					print_string efname;
					print_string ": ";
					printlist (fun aef -> printatomiceffect aef) 
								(SetAef.elements aefset) 
								", ")
				efassign
				"\n"
				
(*
let effectUnion efassign efs =
	let sets = List.map (fun ef -> evalEffect ef efassign) efs in
	let union = List.fold_left (fun set efset -> SetAef.union ef efset) SetAef.empty sets in
	union
*)


let rec ensureInEffect aef efname efassign =
	match efassign with
		[] -> 
			[(efname, SetAef.singleton aef)]
		| (efname', aefset')::rest ->
			if (=) efname efname' then
				(efname', SetAef.add aef aefset')::rest
			else
				let rest' = ensureInEffect aef efname rest in
				(efname', aefset')::rest'

let rec evalEffect ef efassign =
	match ef with
		EfVar(n) ->
			(try
				List.assoc n efassign
			with Not_found ->
				SetAef.empty)
		| EfAtoms(aefs) ->
			List.fold_left (fun set aef -> SetAef.add aef set) SetAef.empty aefs
		| EfSubst(ef', subst) ->
			let eval' = SetAef.elements (evalEffect ef' efassign) in
			let eval'' = List.map (fun (l', m') -> (l', reduceMessage m' subst)) eval' in
			List.fold_left (fun set aef -> SetAef.add aef set) SetAef.empty eval''	

let rec genAefsToTry aef ef =
	(* TODO: generate list of elements that could satisfy 'element \in ef' *)
	let (l, m) = aef in
	match ef with
		EfVar(_) -> [aef]
		| EfAtoms(_) -> [aef]
		| EfSubst(ef1, (m1, h1)) ->
			(* TODO: impl correctly: do recursion in message construction *)
			if msgeq m m1 then [(l, MsgHole(h1)); aef]
			else [aef]
	
let rec genEfsToTry aef efs =
	(* TODO: generate list of effects that could satisfy 'element \in ef' *)
	efs
	
let rec expandable ef =
	match ef with
		EfVar(_) -> true
		| EfAtoms(_) -> false
		| EfSubst(ef1, _) -> expandable ef1

let rec someexpandable efs =
	match efs with
		[] -> false
		| ef::rest -> (expandable ef) || (someexpandable rest)
		
let rec cnstSatisfied cnst efassign =
	match cnst with
		CnstEffectEq(ef1, ef2) ->
			let set1 = evalEffect ef1 efassign in
			let set2 = evalEffect ef2 efassign in
			
			if SetAef.equal set1 set2 then
				true
			else
				if SetAef.subset set1 set2 then
					if expandable ef1 then
						false
					else 
						raise (Unsatisfiable("cnstSatisfied"))
				else
					if expandable ef2 then
						false
					else
						raise (Unsatisfiable("cnstSatisfied"))
		
		| CnstEffectIn(aef, efs) ->
			let efssets = List.map (fun ef -> evalEffect ef efassign) efs in
			let efsunion = List.fold_left (fun set efset -> SetAef.union set efset) SetAef.empty efssets in
			
			if SetAef.mem aef efsunion then
				true
			else
				if someexpandable efs then
					false
				else
					raise (Unsatisfiable("cnstSatisfied"))
				
		| CnstEffectSub(ef, efs) ->
			let efset = evalEffect ef efassign in
			let efssets = List.map (fun ef -> evalEffect ef efassign) efs in
			let efsunion = List.fold_left (fun set efset -> SetAef.union set efset) SetAef.empty efssets in
			
			if SetAef.subset efset efsunion then
				true
			else
				if someexpandable efs then
					false
				else
					raise (Unsatisfiable("cnstSatisfied"))
			
		| _ -> 
			raise (Error("should not occur"))
		
let rec cnstsSatisfied cnsts efassign =
	match cnsts with
		[] -> true
		| cnst::rest ->
			(* warning: avoid short-circuiting! *)
			let satisfied = cnstSatisfied cnst efassign in
			if cnstsSatisfied rest efassign then satisfied
			else false
			
let rec contradictionfree cnsts efassign =
	try
		let _ = cnstsSatisfied cnsts efassign in
		true
	with Unsatisfiable(_) ->
		false

let rec checkandpick indent cnsts efassign =
	match cnsts with
		[] -> None
		| cnst::rest ->
			(match cnst with
				CnstEffectEq(ef1, ef2) ->
					let set1 = evalEffect ef1 efassign in
					let set2 = evalEffect ef2 efassign in
					
					if SetAef.equal set1 set2 then
						checkandpick indent rest efassign
					else
						if SetAef.subset set1 set2 then
							if expandable ef1 && contradictionfree rest efassign then
								let diff = SetAef.diff set2 set1 in
								let element = SetAef.choose diff in
								Some (element, [ef1])
							else
								(print_string indent;
								print_string "unsatisfiable: eq, non-expandable ef1\n";
								raise (Unsatisfiable("eq, non-expandable ef1")))
						else
							if expandable ef2 && contradictionfree rest efassign then
								let diff = SetAef.diff set1 set2 in
								let element = SetAef.choose diff in
								Some (element, [ef2])
							else
								(print_string indent;
								print_string "unsatisfiable: eq, non-expandable ef2\n";
								raise (Unsatisfiable("eq, non-expandable ef2")))
				
				| CnstEffectIn(aef, efs) ->
					raise (Error("checkandpick - should not occur"))
					(*
					let efssets = List.map (fun ef -> evalEffect ef efassign) efs in
					let efsunion = List.fold_left (fun set efset -> SetAef.union set efset) SetAef.empty efssets in
					
					if SetAef.mem aef efsunion then
						checkandpick rest efassign
					else
						Some (aef, efs)
					*)
						
				| CnstEffectSub(ef, efs) ->
					let efset = evalEffect ef efassign in
					let efssets = List.map (fun ef -> evalEffect ef efassign) efs in
					let efsunion = List.fold_left (fun set efset -> SetAef.union set efset) SetAef.empty efssets in
					
					if SetAef.subset efset efsunion then
						checkandpick indent rest efassign
					else
						if someexpandable efs && contradictionfree rest efassign then
							let inter = SetAef.inter efset efsunion in
							let domain = SetAef.diff efset inter in
							let element = SetAef.choose domain in
							Some (element, efs)
						else 
							(print_string indent;
							print_string "unsatisfiable: sub, non-expandable\n";
							raise (Unsatisfiable("sub, non-expandable")))
					
				| _ -> 
					(* ignore *) 
					checkandpick indent rest efassign
			)
			
let rec underlyingefvarname ef =
	match ef with
		EfVar(efname) -> efname
		| EfAtoms(_) -> raise (Error("should not occur - underlyingefvarname"))
		| EfSubst(ef1, _) -> underlyingefvarname ef1
				
let rec satisfyReq indent cnsts efassign aef efs =
	let rec aefChoiceHelper indent aefsToTry efname =
		match aefsToTry with
			[] -> 
				print_string indent;
				print_string "unsatifiable: no more aef choices\n";
				raise (Unsatisfiable("no more aef choices"))
				
			| aef::rest ->
			
				print_string indent;
				print_string efname;
				print_string " += ";
				printatomiceffect aef;
				print_string "\n";
			
				let efassign' = ensureInEffect aef efname efassign in
				try
					let next = checkandpick (indent^"  ") cnsts efassign' in
					(match next with
						None -> efassign'
						| Some (nextaef, nextefs) ->
							satisfyReq (indent^"  ") cnsts efassign' nextaef nextefs)
				with Unsatisfiable(s) ->
					(*print_string (indent^"  ");
					print_string "unsatisfiable: ";
					print_string s;
					print_string "\n";*)
					aefChoiceHelper indent rest efname
	in
	let rec unionChoiceHelper indent aef efsToTry =
		match efsToTry with
			[] -> 
				print_string indent;
				print_string "unsatifiable: no more union choices\n";
				raise (Unsatisfiable("no more union choices"))
				
			| ef::rest ->
			
				print_string indent;
				printeffect ef;
				print_string "\n";
				
				(match ef with
					EfAtoms(aefs) -> 
						let set = List.fold_left (fun set aef -> SetAef.add aef set) SetAef.empty aefs in
						if SetAef.mem aef set then
							efassign
						else
							unionChoiceHelper indent aef rest
						
					| _ ->
						let aefsToTry = genAefsToTry aef ef in
						try
							aefChoiceHelper (indent^"  ") aefsToTry (underlyingefvarname ef)
						with Unsatisfiable(s) ->
							(*print_string (indent^"  ");
							print_string "unsatisfiable: ";
							print_string s;
							print_string "\n";*)
							unionChoiceHelper indent aef rest)
	in
	
	print_string indent;
	printatomiceffect aef;
	print_string " in ";
	printlist printeffect efs ", ";
	print_string "\n";
	
	let efsToTry = genEfsToTry aef efs in
	unionChoiceHelper (indent^"  ") aef efsToTry

let solveEffects mixedcnsts efassign =
	let rec helper cnsts efassign initreqs =
		match initreqs with
			[] -> efassign
			| CnstEffectIn(aef, efs)::rest ->
				let efassign' = satisfyReq "" cnsts efassign aef efs in
				print_string "\n";				
				helper cnsts efassign' rest
				
			| _ -> raise (Error("should not occur"))
	in
	let efcnsts = List.filter (fun cnst -> match cnst with CnstTypeEq(_) -> false | _ -> true) mixedcnsts in
	let (cnsts, initreqs) = List.partition (fun cnst -> match cnst with CnstEffectIn(_) -> false | _ -> true) efcnsts in
	helper cnsts [] initreqs
	




(*

#use "D:/Desktop/Speciale/analyser/structure.ml";;

*)
