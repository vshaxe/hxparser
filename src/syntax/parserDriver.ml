open Ast
open Sedlex_menhir
open Printf
open Parser
open Token
open Json

module I = MenhirInterpreter
module M = MenhirLib.General

type debug_kind =
	| DStart
	| DShift
	| DReduce
	| DOffer
	| DInsert
	| DAccept
	| DReject

type parser_driver_config = {
	mutable debug_flags : debug_kind list;
	mutable build_parse_tree : bool;
	mutable output_json : bool;
}

type placed_token = (token * Lexing.position * Lexing.position)

type token_info = placed_token * placed_token list

type tree =
	| Node of string * tree list
	| Leaf of token_info

type lookahead_state =
	| LANone
	| LAToken of token_info
	| LAActive

type 'a state = {
	tree : tree list;
	in_dead_branch : bool;
	recover_state : 'a state;
	checkpoint : 'a I.checkpoint;
	last_offer : token_info;
	last_shift : token_info;
	inserted_tokens : placed_token list;
	lookahead_state : lookahead_state;
}

type 'a context = {
	lexbuf : lexbuf;
	config : parser_driver_config;
	mutable branches : (expr * 'a state * ('a state * expr) list) list;
}

let default_config () = {
	debug_flags = [];
	build_parse_tree = false;
	output_json = false;
}

let create_context config lexbuf = {
	lexbuf = lexbuf;
	config = config;
	branches = [];
}

let create_state checkpoint =
	let dummy_token = ((EOF,Lexing.dummy_pos,Lexing.dummy_pos),[]) in
	let rec state = {
		tree = [];
		in_dead_branch = false;
		checkpoint = checkpoint;
		last_offer = dummy_token;
		last_shift = dummy_token;
		recover_state = state;
		inserted_tokens = [];
		lookahead_state = LANone;
	} in
	state

let has_debug ctx flag = List.mem flag ctx.config.debug_flags

let print_position = Pos.Position.print

let print_token (token,p1,p2) =
	 Printf.sprintf "%s (%s - %s)" (s_token token) (print_position p1) (print_position p2)

let rec print_tree tabs t = match t with
	| Leaf(token,trivia) -> print_token token
	| Node(_,[]) -> ""
	| Node(name,[t1]) -> (match name with "" -> "" | _ -> name ^ ": ") ^ (print_tree tabs t1)
	| Node(name,tl) ->
		begin match List.rev tl with
			| Node("#list", tl2) :: tl -> print_tree tabs (Node(name,(List.rev tl) @ tl2))
			| _ -> Printf.sprintf "%s%s" (match name with "" -> "" | _ -> name ^ ":") (String.concat "" (List.map (fun t -> match print_tree (tabs ^ "  ") t with "" -> "" | s -> "\n" ^ tabs ^ s) tl))
		end

let print_tree_list tree =
	String.concat "\n" (List.map (fun t -> print_tree "" t) tree)

let pos_to_json p =
	let open Lexing in
	JInt p.pos_cnum

let rec to_json = function
	| Leaf((token,p1,p2),trivia) ->
		let trivia = List.map (fun t -> Leaf(t,[])) trivia in
		let l = ("token",JString (s_token token)) :: ("start",pos_to_json p1) :: ("end",pos_to_json p2) ::
			(match trivia with [] -> [] | _ -> ["trivia",JArray (List.map to_json trivia)]) in
		JObject l
	| Node(_,[]) -> JNull
	| Node(name,[t1]) -> (match name with "" | "#list" -> to_json t1 | _ -> JObject[name,to_json t1])
	| Node(name,tl) ->
		begin match List.rev tl with
			| Node("#list", tl2) :: tl -> to_json (Node(name,(List.rev tl) @ tl2))
			| _ ->
				let l = List.map to_json tl in
				let l = List.filter (fun j -> j <> JNull) l in
				match l with
				| [] -> JNull
				| _ ->
					let j = JArray l in
					(match name with "" | "#list" -> j | _ -> JObject [name,j])
		end

let offer ctx state token trivia =
	if has_debug ctx DOffer then begin
		print_endline (Printf.sprintf "[OFFER ] %s" (print_token token));
	end;
	let checkpoint = I.offer state.checkpoint token in
	let state = {state with last_offer = (token,trivia); checkpoint = checkpoint; recover_state = state} in
	state

let rec input_needed : 'a . 'a context -> 'a state -> 'a = fun ctx state ->
	let token_from_lexer state = match state.inserted_tokens with
		| token :: tokens when not state.in_dead_branch ->
			{state with inserted_tokens = tokens},token
		| _ ->
			let p1 = ctx.lexbuf.pos in
			let tk = (if state.in_dead_branch then Lexer.preprocessor else Lexer.token) ctx.lexbuf in
			let p2 = ctx.lexbuf.pos in
			state,(tk,p1,p2)
	in
	let not_expr e = EUnop(Not,Prefix,e),snd e in
	let rec next_token state trivia =
		let state,token = token_from_lexer state in
		process_token state (token,trivia)
	and process_token state ((tk,p1,p2) as token,trivia) = match tk with
		| (WHITESPACE _ | COMMENTLINE _) -> next_token state (token :: trivia)
		| COMMENT _ -> next_token state (token :: trivia)
		| SHARPERROR ->
			let message_checkpoint = (Parser.Incremental.sharp_error_message ctx.lexbuf.pos) in
			let _ = run ctx.config ctx.lexbuf message_checkpoint in
			next_token state trivia
		| SHARPIF ->
			let cond_checkpoint = (Parser.Incremental.sharp_condition ctx.lexbuf.pos) in
			let cond = run ctx.config ctx.lexbuf cond_checkpoint in
			let state2 = match state.lookahead_state with
				| LAToken(token,_) -> {state with inserted_tokens = token :: state.inserted_tokens; lookahead_state = LAActive}
				| _ -> state
			in
			ctx.branches <- (cond,state2,[]) :: ctx.branches;
			next_token state trivia
		| SHARPELSEIF ->
			let cond_checkpoint = (Parser.Incremental.sharp_condition ctx.lexbuf.pos) in
			let cond = run ctx.config ctx.lexbuf cond_checkpoint in
			begin match ctx.branches with
			| (expr,state0,states) :: branches ->
				ctx.branches <- (cond,state0,((state,expr) :: states)) :: branches;
				next_token {state0 with in_dead_branch = true} trivia
			| [] -> assert false
			end
		| SHARPELSE ->
			begin match ctx.branches with
			| (expr,state0,states) :: branches ->
				ctx.branches <- (not_expr expr,state0,((state,expr) :: states)) :: branches;
				next_token {state0 with in_dead_branch = true} trivia
			| [] -> assert false
			end
		| SHARPEND ->
			begin match ctx.branches with
			| (expr,_,states) :: branches ->
				ctx.branches <- branches;
				begin match List.filter (fun (state,_) -> not state.in_dead_branch) ((state,expr) :: states) with
				| [state,_] -> next_token state trivia
				| _ -> next_token state trivia
				end
			| [] -> assert false
			end
		| SEMICOLON when state.lookahead_state = LANone ->
			let clear_lookahead state = {state with lookahead_state = LANone} in
			begin match next_token {state with lookahead_state = LAToken(token,trivia)} trivia with
				| (state,((ELSE,p1,p2) as token),trivia) ->
					clear_lookahead state,token,trivia
				| (state2,token2,trivia2) ->
					if state2.lookahead_state = LAActive then
						clear_lookahead state,token2,trivia2
					else begin
						let state = {state with inserted_tokens = token2 :: state.inserted_tokens; lookahead_state = LANone} in
						state,token,trivia
					end
			end
		| _ ->
			state,token,trivia
	in
	let state,token,trivia = next_token state [] in
	let state = offer ctx state token trivia in
	loop ctx state

and loop : 'a . 'a context -> 'a state -> 'a =
	fun ctx state -> match state.checkpoint with
	| I.Accepted v ->
		if ctx.config.output_json then begin
			let buffer = Buffer.create 0 in
			Json.write_json (Buffer.add_string buffer) (JArray (List.map to_json state.tree));
			print_endline (Buffer.contents buffer);
		end;
		if has_debug ctx DAccept then begin
			if ctx.config.build_parse_tree then
				print_endline (Printf.sprintf "[ACCEPT] %s" (print_tree_list state.tree))
			else
				print_endline "[ACCEPT]"
		end;
		v
	| I.InputNeeded _ ->
		input_needed ctx state
	| I.Shifting _ ->
		let token = state.last_offer in
		if has_debug ctx DShift then begin
			let ((tk,_,_),_) = token in print_endline (Printf.sprintf "[SHIFT ] %s" (s_token tk));
		end;
		let state = {state with checkpoint = I.resume state.checkpoint; last_shift = token} in
		let state =
			if ctx.config.build_parse_tree then {state with tree = Leaf token :: state.tree}
			else state
		in
		loop ctx state
	| I.AboutToReduce(_,production) ->
		if has_debug ctx DReduce then begin match I.rhs production with
			| [] -> ()
			| rhs -> print_endline (Printf.sprintf "[REDUCE] %s <- %s" (s_xsymbol (I.lhs production)) (String.concat " " (List.map s_xsymbol rhs)));
		end;
		let state = if ctx.config.build_parse_tree then begin
			let l = List.length (I.rhs production) in
			let _,nodes1,nodes2 = List.fold_left (fun (i,l1,l2) x -> if i < l then (i + 1,x :: l1,l2) else (i + 1,l1,x :: l2)) (0,[],[]) state.tree in
			let nodes2 = List.rev nodes2 in
			{state with tree = ((Node(s_xsymbol (I.lhs production),nodes1)) :: nodes2)}
		end else
			state
		in
		let state = {state with checkpoint = I.resume state.checkpoint} in
		loop ctx state
	| I.HandlingError env ->
		(*print_endline (Printf.sprintf "[ERROR ] Token: %s, Last shift: %s" (print_token (fst state.last_offer)) (print_token (fst state.last_shift)));*)
		let insert token =
			if has_debug ctx DInsert then begin
				print_endline (Printf.sprintf "[INSERT] %s" (s_token token));
			end;
			let last_offer = state.last_offer in
			let state = offer ctx state.recover_state (token,Lexing.dummy_pos,Lexing.dummy_pos) [] in
			let state = {state with inserted_tokens = fst last_offer :: state.inserted_tokens } in
			loop ctx state
		in
		begin match state.last_shift with
			| ((BRCLOSE,p1,_),_) when I.acceptable state.recover_state.checkpoint SEMICOLON p1 -> insert SEMICOLON
			| _ ->
				let so = match Lazy.force (I.stack env) with
					| M.Cons(I.Element(lrstate,_,_,_),_) -> (try Some (SyntaxErrors.message (I.number lrstate)) with Not_found -> None)
					| _ -> None
				in
				begin match so with
					| Some s -> print_endline s;
					| None -> ()
				end;
				loop ctx {state with checkpoint = I.resume state.checkpoint};
		end;
	| I.Rejected ->
		if has_debug ctx DReject then begin
			if ctx.config.build_parse_tree then
				print_endline (Printf.sprintf "[REJECT] %s" (print_tree_list state.tree));
			print_endline (Printf.sprintf "[REJECT] %s" (print_token (fst state.last_offer)));
		end;
		assert false

and start : 'a . 'a context -> 'a I.checkpoint -> 'a = fun ctx checkpoint ->
	if has_debug ctx DStart then begin
		print_endline "[START ]"
	end;
	let state = create_state checkpoint in
	loop ctx state

and run : 'a . parser_driver_config -> lexbuf -> 'a I.checkpoint -> 'a = fun config lexbuf checkpoint ->
	let ctx = create_context config lexbuf in
	start ctx checkpoint