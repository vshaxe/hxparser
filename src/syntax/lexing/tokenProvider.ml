open Sedlex_menhir
open Tokens
open AstPre
open Ops
open Token
open WorkList.LinkedNode
open WorkList

type t = {
	lexbuf : lexbuf;
	skipped_list : int WorkList.t;
	mutable inserted_tokens : work_token list;
	mutable token_cache : (placed_token * trivia_flag) WorkList.t;
	mutable branches : ((expr * bool) * bool * Lexing.position) list;
	mutable skipped : int;
	mutable in_dead_branch : bool;
	mutable blocks : range list;
}

let create lexbuf = {
	lexbuf = lexbuf;
	skipped_list = WorkList.create 0;
	inserted_tokens = [];
	token_cache = begin
		let neutral = ((NONSENSE "",Lexing.dummy_pos,Lexing.dummy_pos),TFNormal) in
		WorkList.create neutral;
	end;
	branches = [];
	skipped = 0;
	in_dead_branch = false;
	blocks = [];
}

let insert_token tp wtoken =
	tp.inserted_tokens <- wtoken :: tp.inserted_tokens

let on_shift tp (token,flag,skipped) =
	let trail = ref [] in
	let cache = tp.token_cache in
	let skip () =
		let skipped = tp.skipped + skipped in
		tp.skipped <- 0;
		WorkList.push tp.skipped_list skipped;
		WorkList.advance_by cache (fun node -> fst node.next.data) skipped
	in
	let leading = match flag with
		| TFNormal ->
			let leading = skip() in
			ignore(advance cache);
			leading;
		| TFSkipped ->
			assert false;
		| TFInserted | TFImplicit ->
			let leading = skip() in
			insert cache (token,flag);
			ignore(advance cache);
			leading;
		| TFSplit(original,token2) ->
			let leading = skip() in
			(WorkList.current cache).data <- (token,snd (WorkList.current cache).data);
			ignore(advance cache);
			insert cache (token2,TFNormal);
			leading;
	in
	{tleading = leading; ttrailing = trail; tflags = [flag]}

let skip tp (_,_,skipped) =
	tp.skipped <- tp.skipped + skipped + 1

let lexer_token tp =
	let p1 = tp.lexbuf.pos in
	let tk = (if tp.in_dead_branch then Lexer.preprocessor else Lexer.token) tp.lexbuf in
	let p2 = tp.lexbuf.pos in
	push tp.token_cache ((tk,p1,p2),TFNormal);
	(tk,p1,p2)

let rec fetch_token tp skipped =
	let token = lexer_token tp in
	process_token tp token skipped

and process_token tp (tk,p1,p2) skipped =
	let not_expr e = EUnop(Not,Prefix,e),snd e in
	let preprocess checkpoint =
		let skipped = ref 0 in
		let cond = ParserPre.MenhirInterpreter.loop (fun () ->
			let token,skipped2 = fetch_token tp 0 in
			skipped := !skipped + skipped2 + 1;
			token
		) checkpoint in
		cond,!skipped
	in
	match tk with
	| (WHITESPACE _ | COMMENT _ | NONSENSE _) ->
		fetch_token tp (skipped + 1)
	| NEWLINE _ | COMMENTLINE _ ->
		fetch_token tp (skipped + 1)
	| SHARPERROR ->
		let message_checkpoint = ParserPre.Incremental.sharp_error_message tp.lexbuf.pos in
		let _,skipped2 = preprocess message_checkpoint in
		(* TODO: this loses tokens if there is no message *)
		fetch_token tp (skipped + skipped2 + 1)
	| SHARPLINE ->
		let line_number_checkpoint = ParserPre.Incremental.sharp_line_number tp.lexbuf.pos in
		let line,skipped2 = preprocess line_number_checkpoint in
		set_line tp.lexbuf (try int_of_string line with _ -> assert false);
		fetch_token tp (skipped + skipped2 + 1)
	| SHARPIF when tp.in_dead_branch ->
		(* We have to push _something_ on the stack so the #end can consume it. Parsing the
			condition could arrive at the #end immediately. *)
		tp.branches <- (((EConst (Ident "false"),Pos.Range.null),true),true,p1) :: tp.branches;
		fetch_token tp (skipped + 1)
	| SHARPIF ->
		let cond_checkpoint = ParserPre.Incremental.sharp_condition tp.lexbuf.pos in
		let cond,skipped2 = preprocess cond_checkpoint in
		tp.branches <- ((cond,tp.in_dead_branch),tp.in_dead_branch,p1) :: tp.branches;
		fetch_token tp (skipped + skipped2 + 1)
	| SHARPELSEIF ->
		begin match tp.branches with
		| ((_,b1),b2,_) :: _ when not b1 || b2 ->
			tp.in_dead_branch <- true;
			fetch_token tp (skipped + 1)
		| ((cond2,dead_branch),dead,p1) :: branches ->
			let cond_checkpoint = ParserPre.Incremental.sharp_condition tp.lexbuf.pos in
			let cond,skipped2 = preprocess cond_checkpoint in
			tp.branches <- ((cond,false),dead,p1) :: branches;
			if dead || not dead_branch then tp.in_dead_branch <- true;
			fetch_token tp (skipped + skipped2 + 1)
		| [] -> assert false
		end
	| SHARPELSE ->
		begin match tp.branches with
		| ((cond,dead_branch),dead,p1) :: branches ->
			tp.branches <- ((not_expr cond,not dead_branch),dead,p1) :: branches;
			if dead || not dead_branch then tp.in_dead_branch <- true;
			fetch_token tp (skipped + 1)
		| [] -> assert false
		end
	| SHARPEND ->
		begin match tp.branches with
		| (_,dead,p1) :: branches ->
			tp.blocks <- (p1,p2) :: tp.blocks;
			tp.branches <- branches;
			tp.in_dead_branch <- dead;
			fetch_token tp (skipped + 1)
		| [] -> assert false
		end
	| _ ->
		(tk,p1,p2),skipped

let next_token tp = match tp.inserted_tokens with
	| [] ->
		let token,skipped = fetch_token tp 0 in
		begin match token with
		| (SEMICOLON,_,_) ->
			begin match fetch_token tp 0 with
			| ((ELSE,_,_) as token2),skipped2 ->
				token2,TFNormal,skipped + skipped2 + 1
			| token2,skipped2 ->
				insert_token tp (token2,TFNormal,skipped2);
				token,TFNormal,skipped
			end
		| _ -> token,TFNormal,skipped
		end
	| (token,flag,skipped) :: tokens ->
		tp.inserted_tokens <- tokens;
		token,flag,skipped