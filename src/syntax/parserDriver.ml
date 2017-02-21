open Ast
open Sedlex_menhir
open Printf
open Parser
open Token

module I = MenhirInterpreter
module M = MenhirLib.General

module Config = struct
	type debug_kind =
		| DStart
		| DShift
		| DReduce
		| DOffer
		| DInsert
		| DAccept
		| DReject

	type t = {
		mutable debug_flags : debug_kind list;
		mutable build_parse_tree : bool;
	}

	let default_config () = {
		debug_flags = [];
		build_parse_tree = false;
	}
end

type trivia_flag =
	| TFImplicit
	| TFInserted
	| TFSkipped

type placed_token = (token * Lexing.position * Lexing.position)

type token_info = placed_token * trivia

and tree =
	| Node of string * tree list
	| Leaf of token_info
	| Flag of string

and trivia = {
	tleading: tree list;
	ttrailing: tree list;
	tflags : trivia_flag list;
}

let create_trivia flags = {
	tleading = [];
	ttrailing = [];
	tflags = flags;
}

module State = struct
	type 'a t = {
		tree : tree list;
		recover_state : 'a t;
		checkpoint : 'a I.checkpoint;
		last_offer : token_info;
		last_shift : token_info;
	}

	let create checkpoint =
		let dummy_token = ((EOF,Lexing.dummy_pos,Lexing.dummy_pos),create_trivia []) in
		let rec state = {
			tree = [];
			checkpoint = checkpoint;
			last_offer = dummy_token;
			last_shift = dummy_token;
			recover_state = state;
		} in
		state

end

type 'a result =
	| Accept of 'a * tree list
	| Reject of string list * tree list

let print_position = Pos.Position.print

let print_token (token,p1,p2) =
	 Printf.sprintf "%s (%s - %s)" (s_token token) (print_position p1) (print_position p2)

let rec print_trivia trivia =
	let l = (match trivia.tleading with [] -> [] | _ -> ["leading",String.concat ", " (List.map (fun t -> print_tree "" t) trivia.tleading)]) @
	(match trivia.ttrailing with [] -> [] | _ -> ["trailing",String.concat ", " (List.map (fun t -> print_tree "" t) trivia.ttrailing)]) in
	String.concat ", " (List.map (fun (name,s) -> Printf.sprintf "%s: %s" name s) l)

and print_tree tabs t = match t with
	| Leaf(token,trivia) -> Printf.sprintf "%s [%s]" (print_token token) (print_trivia trivia)
	| Node(_,[]) -> ""
	| Node(name,[t1]) -> (match name with "" -> "" | _ -> name ^ ": ") ^ (print_tree tabs t1)
	| Node(name,tl) ->
		begin match List.rev tl with
			| Node(name2, tl2) :: tl when name = name2 -> print_tree tabs (Node(name,(List.rev tl) @ tl2))
			| _ -> Printf.sprintf "%s%s" (match name with "" -> "" | _ -> name ^ ":") (String.concat "" (List.map (fun t -> match print_tree (tabs ^ "  ") t with "" -> "" | s -> "\n" ^ tabs ^ s) tl))
		end
	| Flag name -> name

let print_tree_list tree =
	String.concat "\n" (List.map (fun t -> print_tree "" t) tree)

module TokenProvider = struct
	type t = {
		lexbuf: lexbuf;
		mutable parse_expr: expr I.checkpoint -> expr result;
		mutable parse_number: string I.checkpoint -> string result;
		mutable parse_string: string I.checkpoint -> string result;
		token_cache: placed_token Queue.t;
		mutable inserted_token : token_info option;
		mutable leading: tree list;
		mutable branches : ((expr * bool) * bool) list;
		mutable in_dead_branch : bool;
	}

	let create lexbuf = {
		lexbuf = lexbuf;
		parse_expr = Obj.magic;
		parse_number = Obj.magic;
		parse_string = Obj.magic;
		token_cache = Queue.create();
		inserted_token = None;
		leading = [];
		branches = [];
		in_dead_branch = false;
	}

	let insert_token tp token =
		assert (tp.inserted_token = None);
		tp.inserted_token <- Some token

	let consume_token tp =
		let _ = Queue.pop tp.token_cache in
		()

	let lexer_token tp =
		let p1 = tp.lexbuf.pos in
		let tk = (if tp.in_dead_branch then Lexer.preprocessor else Lexer.token) tp.lexbuf in
		let p2 = tp.lexbuf.pos in
		(tk,p1,p2)

	let peek_token tp =
		let token = if Queue.is_empty tp.token_cache then begin
			let token = lexer_token tp in
			Queue.push token tp.token_cache;
			token
		end else
			Queue.peek tp.token_cache
		in
		token

	let consume_trailing tp trivia =
		let rec loop acc = match peek_token tp with
			| ((WHITESPACE _ | COMMENT _ | COMMENTLINE _),_,_) as token ->
				consume_token tp;
				loop ((Leaf(token,create_trivia [])) :: acc)
			| (NEWLINE _,_,_) as token ->
				consume_token tp;
				List.rev (Leaf(token,create_trivia []) :: acc)
			| _ ->
				List.rev acc
		in
		{trivia with ttrailing = loop []}

	let rec fetch_token tp =
		let token = if Queue.is_empty tp.token_cache then lexer_token tp
		else Queue.pop tp.token_cache in
		let token,trivia = process_token tp token in
		token,trivia

	and process_token tp (tk,p1,p2) : token_info =
		let add_leading trivia =
			tp.leading <- (Leaf (trivia,create_trivia [])) :: tp.leading
		in
		let merge_leading trivia =
			tp.leading <- trivia.tleading @ tp.leading
		in
		let parse f checkpoint = match f checkpoint with
			| Accept(x,tree) ->
				merge_leading {tleading = tree; ttrailing = []; tflags = [] };
				x
			| Reject _ -> raise Exit
		in
		let not_expr e = EUnop(Not,Prefix,e),snd e in
		match tk with
		| (WHITESPACE _ | NEWLINE _ | COMMENTLINE _ | COMMENT _) ->
			add_leading (tk,p1,p2);
			fetch_token tp
		| SHARPERROR ->
			add_leading (tk,p1,p2);
			let message_checkpoint = Parser.Incremental.sharp_error_message tp.lexbuf.pos in
			(* TODO: this loses tokens if there is no message *)
			let _ = (try parse tp.parse_string message_checkpoint with Exit -> "") in
			fetch_token tp;
		| SHARPLINE ->
			add_leading (tk,p1,p2);
			let line_number_checkpoint = Parser.Incremental.sharp_line_number tp.lexbuf.pos in
			let line = parse tp.parse_number line_number_checkpoint in
			set_line tp.lexbuf (try int_of_string line with _ -> assert false);
			fetch_token tp;
		| SHARPIF when tp.in_dead_branch ->
			add_leading (tk,p1,p2);
			(* We have to push _something_ on the stack so the #end can consume it. Parsing the
			   condition could arrive at the #end immediately. *)
			tp.branches <- (((EConst (Ident "false"),Pos.Range.null),true),true) :: tp.branches;
			fetch_token tp;
		| SHARPIF ->
			add_leading (tk,p1,p2);
			let cond_checkpoint = Parser.Incremental.sharp_condition tp.lexbuf.pos in
			let cond = parse tp.parse_expr cond_checkpoint in
			tp.branches <- ((cond,tp.in_dead_branch),tp.in_dead_branch) :: tp.branches;
			fetch_token tp;
		| SHARPELSEIF ->
			add_leading (tk,p1,p2);
			begin match tp.branches with
			| ((_,b1),b2) :: _ when not b1 || b2 ->
				tp.in_dead_branch <- true;
				fetch_token tp
			| ((cond2,dead_branch),dead) :: branches ->
				let cond_checkpoint = Parser.Incremental.sharp_condition tp.lexbuf.pos in
				let cond = parse tp.parse_expr cond_checkpoint in
				tp.branches <- ((cond,false),dead) :: branches;
				if dead || not dead_branch then tp.in_dead_branch <- true;
				fetch_token tp
			| [] -> assert false
			end
		| SHARPELSE ->
			add_leading (tk,p1,p2);
			begin match tp.branches with
			| ((cond,dead_branch),dead) :: branches ->
				tp.branches <- ((not_expr cond,not dead_branch),dead) :: branches;
				if dead || not dead_branch then tp.in_dead_branch <- true;
				fetch_token tp
			| [] -> assert false
			end
		| SHARPEND ->
			add_leading (tk,p1,p2);
			begin match tp.branches with
			| (_,dead) :: branches ->
				tp.branches <- branches;
				tp.in_dead_branch <- dead;
				fetch_token tp
			| [] -> assert false
			end
		| _ ->
			let leading = List.rev tp.leading in
			tp.leading <- [];
			let trivia = { tleading = leading; ttrailing = []; tflags = [] } in
			(tk,p1,p2),trivia

	let next_token tp = match tp.inserted_token with
		| None ->
			let token,trivia = fetch_token tp in
			let trivia = consume_trailing tp trivia in
			begin match token with
			| (SEMICOLON,_,_) ->
				begin match fetch_token tp with
				| ((ELSE,_,_) as token2,trivia2) ->
					let trivia2 = consume_trailing tp trivia2 in
					let trivia2 = {trivia2 with tleading = (Leaf(token,{trivia with tflags = TFSkipped :: trivia.tflags}) :: trivia2.tleading)} in
					token2,trivia2
				| token2 ->
					insert_token tp token2;
					token,trivia
				end
			| _ -> token,trivia
			end
		| Some(token,trivia) ->
			let trivia = consume_trailing tp trivia in
			tp.inserted_token <- None;
			token,trivia
end

open State
open Config

let has_debug config flag = List.mem flag config.debug_flags

let rec print_tree tabs t = match t with
	| Leaf(token,trivia) -> print_token token
	| Node(_,[]) -> ""
	| Node(name,[t1]) -> (match name with "" -> "" | _ -> name ^ ": ") ^ (print_tree tabs t1)
	| Node(name,tl) ->
		begin match List.rev tl with
			| Node(name2, tl2) :: tl when name = name2 -> print_tree tabs (Node(name,(List.rev tl) @ tl2))
			| _ -> Printf.sprintf "%s%s" (match name with "" -> "" | _ -> name ^ ":") (String.concat "" (List.map (fun t -> match print_tree (tabs ^ "  ") t with "" -> "" | s -> "\n" ^ tabs ^ s) tl))
		end
	| Flag name -> name

let print_tree_list tree =
	String.concat "\n" (List.map (fun t -> print_tree "" t) tree)

let offer config state token trivia =
	if has_debug config DOffer then begin
		print_endline (Printf.sprintf "[OFFER ] %s" (print_token token));
	end;
	let checkpoint = I.offer state.checkpoint token in
	let state = {state with last_offer = (token,trivia); checkpoint = checkpoint; recover_state = state} in
	state

let rec input_needed : 'a . (Config.t * TokenProvider.t) -> 'a State.t -> 'a result = fun (config,tp) state ->
	let token,trivia = TokenProvider.next_token tp in
	let state = offer config state token trivia in
	loop (config,tp) state

and loop : 'a . (Config.t * TokenProvider.t) -> 'a State.t -> 'a result =
	fun (config,tp) state -> match state.checkpoint with
	| I.Accepted v ->
		if has_debug config DAccept then begin
			if config.build_parse_tree then
				print_endline (Printf.sprintf "[ACCEPT] %s" (print_tree_list state.tree))
			else
				print_endline "[ACCEPT]"
		end;
		Accept(v,state.tree)
	| I.InputNeeded _ ->
		input_needed (config,tp) state
	| I.Shifting _ ->
		let token = state.last_offer in
		if has_debug config DShift then begin
			let ((tk,_,_),_) = token in print_endline (Printf.sprintf "[SHIFT ] %s" (s_token tk));
		end;
		let state = {state with checkpoint = I.resume state.checkpoint; last_shift = token} in
		let state =
			if config.build_parse_tree then {state with tree = Leaf token :: state.tree}
			else state
		in
		loop (config,tp) state
	| I.AboutToReduce(_,production) ->
		if has_debug config DReduce then begin match I.rhs production with
			| [] -> ()
			| rhs -> print_endline (Printf.sprintf "[REDUCE] %s <- %s" (s_xsymbol (I.lhs production)) (String.concat " " (List.map s_xsymbol rhs)));
		end;
		let state = if config.build_parse_tree then begin
			let l = List.length (I.rhs production) in
			let _,nodes1,nodes2 = List.fold_left (fun (i,l1,l2) x -> if i < l then (i + 1,x :: l1,l2) else (i + 1,l1,x :: l2)) (0,[],[]) state.tree in
			let nodes2 = List.rev nodes2 in
			{state with tree = ((Node(s_xsymbol (I.lhs production),nodes1)) :: nodes2)}
		end else
			state
		in
		let state = {state with checkpoint = I.resume state.checkpoint} in
		loop (config,tp) state
	| I.HandlingError env ->
		(*print_endline (Printf.sprintf "[ERROR ] Token: %s, Last shift: %s" (print_token (fst state.last_offer)) (print_token (fst state.last_shift)));*)
		let insert token allowed p =
			if has_debug config DInsert then begin
				print_endline (Printf.sprintf "[INSERT] %s" (s_token token));
			end;
			let last_offer = state.last_offer in
			let state = offer config state.recover_state (token,p,p) (create_trivia [if allowed then TFImplicit else TFInserted]) in
			TokenProvider.insert_token tp last_offer;
			loop (config,tp) state
		in
		let acceptable = I.acceptable state.recover_state.checkpoint in
		let was_inserted trivia = List.mem TFInserted trivia.tflags in
		begin match state.last_shift with
			| ((BRCLOSE,p1,_),trivia) when not (was_inserted trivia) && acceptable SEMICOLON p1 -> insert SEMICOLON true p1
			| _ ->
				let p = tp.TokenProvider.lexbuf.pos in
				if acceptable SEMICOLON p then insert SEMICOLON false p
				else if acceptable PCLOSE p then insert PCLOSE false p
				else if acceptable BRCLOSE p then insert BRCLOSE false p
				else if acceptable BKCLOSE p then insert BKCLOSE false p
				else if acceptable (IDENT "_") p then insert (IDENT "_") false p
				else loop (config,tp) {state with checkpoint = I.resume state.checkpoint};
				(*let so = match Lazy.force (I.stack env) with
					| M.Cons(I.Element(lrstate,_,_,_),_) -> (try Some (SyntaxErrors.message (I.number lrstate)) with Not_found -> None)
					| _ -> None
				in
				begin match so with
					| Some s -> print_endline s;
					| None -> ()
				end;*)
		end;
	| I.Rejected ->
		let messages = ref [] in
		if has_debug config DReject then begin
			if config.build_parse_tree then begin
				messages := (Printf.sprintf "[REJECT] %s" (print_tree_list state.tree)) :: !messages;
		end;
			messages := (Printf.sprintf "[REJECT] %s" (print_token (fst state.last_offer))) :: !messages;
		end else
			messages := "[REJECT]" :: !messages;
		Reject(!messages,state.tree)

and start : 'a . (Config.t * TokenProvider.t) -> 'a I.checkpoint -> 'a result = fun (config,tp) checkpoint ->
	if has_debug config DStart then begin
		print_endline "[START ]"
	end;
	let state = State.create checkpoint in
	loop (config,tp) state

let run : 'a . Config.t -> lexbuf -> 'a I.checkpoint -> 'a result = fun config lexbuf checkpoint ->
	let open TokenProvider in
	let tp = TokenProvider.create lexbuf in
	tp.parse_expr <- start (config,tp);
	tp.parse_number <- start (config,tp);
	tp.parse_string <- start (config,tp);
	start (config,tp) checkpoint