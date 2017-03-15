open Tokens
open Token
open TokenProvider

module type Engine = sig
	module I : MenhirLib.IncrementalEngine.EVERYTHING with type token = token
	val s_xsymbol : I.xsymbol -> string

	type tree =
		| Node of I.xsymbol * tree list
		| Leaf of token_info
end

module Make (E : Engine) = struct

	open E
	module M = MenhirLib.General

	module State = struct
		type 'a t = {
			recover_state : 'a t;
			checkpoint : 'a I.checkpoint;
			last_offer : work_token;
			last_shift : placed_token;
			tree : tree list;
		}

		let create checkpoint =
			let dummy_token = (EOF,Lexing.dummy_pos,Lexing.dummy_pos) in
			let rec state = {
				checkpoint = checkpoint;
				last_offer = (dummy_token,TFNormal,0);
				last_shift = dummy_token;
				recover_state = state;
				tree = [];
			} in
			state
	end

	type 'a result =
		| Accept of 'a * tree list * range list
		| Reject of string list * tree list * range list

	let print_position = Pos.Position.print

	let print_token (token,p1,p2) =
		Printf.sprintf "%s (%s - %s)" (s_token token) (print_position p1) (print_position p2)

	open State
	open Config

	let has_debug config flag = List.mem flag config.debug_flags

	let offer config state flag skipped token =
		if has_debug config DOffer then begin
			prerr_endline (Printf.sprintf "[OFFER ] %s" (print_token token));
		end;
		let checkpoint = I.offer state.checkpoint token in
		let state = {state with last_offer = (token,flag,skipped); checkpoint = checkpoint; recover_state = state} in
		state

	let shift (config,tp) state =
		let token,_,_ = state.last_offer in
		if has_debug config DShift then begin
			let ((tk,_,_)) = token in prerr_endline (Printf.sprintf "[SHIFT ] %s" (s_token tk));
		end;
		let state =
			if config.build_parse_tree then {state with tree = Leaf(token,TokenProvider.on_shift tp state.last_offer) :: state.tree}
			else state
		in
		state,token

	let rec loop (config,tp) state = match state.checkpoint with
		| I.Accepted v ->
			if has_debug config DAccept then begin
				prerr_endline "[ACCEPT]"
			end;
			Accept(v,state.tree,tp.TokenProvider.blocks)
		| I.InputNeeded _ ->
			let token,flag,skipped = TokenProvider.next_token tp in
			let state = offer config state flag skipped token in
			loop (config,tp) state
		| I.Shifting _ ->
			let state,token = shift (config,tp) state in
			let state = {state with checkpoint = I.resume state.checkpoint; last_shift = token} in
			loop (config,tp) state
		| I.AboutToReduce(_,production) ->
			if has_debug config DReduce then begin match I.rhs production with
				| [] -> ()
				| rhs -> prerr_endline (Printf.sprintf "[REDUCE] %s <- %s" (E.s_xsymbol (I.lhs production)) (String.concat " " (List.map E.s_xsymbol rhs)));
			end;
			let state = {state with checkpoint = I.resume state.checkpoint} in
			let state = if config.build_parse_tree then begin
				let l = List.length (I.rhs production) in
				let rec loop i acc nodes =
					if i >= l then acc,nodes
					else match nodes with
						| hd :: tl -> loop (i + 1) (hd :: acc) tl
						| [] -> assert false
				in
				let nodes1,nodes2 = loop 0 [] state.tree in
				{state with tree = ((Node((I.lhs production),nodes1)) :: nodes2)}
			end else
				state
			in
			loop (config,tp) state
		| I.HandlingError env ->
			let insert token allowed p =
				if has_debug config DInsert then begin
					prerr_endline (Printf.sprintf "[INSERT] %s" (s_token token));
				end;
				let last_offer = state.last_offer in
				let token = (token,p,p) in
				let state = offer config state.recover_state (if allowed then TFImplicit else TFInserted) 0 token in
				TokenProvider.insert_token tp last_offer;
				(config,tp),state
			in
			let token,_,skipped = state.last_offer in
			let split (p1,p2) new1 new2 =
				let open Lexing in
				let p1' = {p1 with pos_cnum = p1.pos_cnum + 1} in
				let token1 = (new1,p1,p1') in
				let token2 = (new2,p1',p2) in
				TokenProvider.insert_token tp (token2,TFNormal,0);
				TokenProvider.insert_token tp (token1,(TFSplit(token,token2)),skipped);
				(config,tp),state.recover_state
			in
			let acceptable token = I.acceptable state.recover_state.checkpoint token in
			let fail () =
				loop (config,tp) {state with checkpoint = I.resume state.checkpoint}
			in
			begin match state.last_shift,token with
				| ((BRCLOSE,_,p)),_ when acceptable SEMICOLON p ->
					let fst,snd = insert SEMICOLON true p in
					loop fst snd
				| _,((SHR,p1,p2)) when acceptable GT p1 -> (let (fst,snd) = split (p1,p2) GT GT in loop fst snd)
				| _,((USHR,p1,p2)) when acceptable GT p1 -> (let (fst,snd) = split (p1,p2) GT SHR in loop fst snd)
				| _,((GTE,p1,p2)) when acceptable GT p1 -> (let (fst,snd) = split (p1,p2) GT ASSIGN in loop fst snd)
				| _,((ASSIGNSHR,p1,p2)) when acceptable GT p1 -> (let (fst,snd) = split (p1,p2) GT GTE in loop fst snd)
				| _,((ASSIGNUSHR,p1,p2)) when acceptable GT p1 -> (let (fst,snd) = split (p1,p2) GT ASSIGNSHR in loop fst snd)
				| _ when not config.recover -> fail()
				| ((tk,_,p)),_ ->
					if acceptable SEMICOLON p then (let (fst,snd) = insert SEMICOLON false p in loop fst snd)
					else if acceptable PCLOSE p then (let (fst,snd) = insert PCLOSE false p in loop fst snd)
					else if acceptable BKCLOSE p then (let (fst,snd) = insert BKCLOSE false p in loop fst snd)
					else if tk <> SEMICOLON && acceptable (IDENT "_") p then (let (fst,snd) = insert (IDENT "_") false p in loop fst snd)
					else begin match token with
						| ((EOF,_,_)) ->
							if acceptable BRCLOSE p then (let (fst,snd) = insert BRCLOSE false p in loop fst snd)
							else fail()
						| token ->
							TokenProvider.skip tp state.last_offer;
							loop (config,tp) state.recover_state
					end
					(*let so = match Lazy.force (I.stack env) with
						| M.Cons(I.Element(lrstate,_,_,_),_) -> (try Some (SyntaxErrors.message (I.number lrstate)) with Not_found -> None)
						| _ -> None
					in
					begin match so with
						| Some s -> prerr_endline s;
						| None -> ()
					end;*)
			end;
		| I.Rejected ->
			let messages = ref [] in
			if has_debug config DReject then begin
				messages := (Printf.sprintf "[REJECT] %s" (print_token (let token,_,_ = state.last_offer in token))) :: !messages;
			end else
				messages := "[REJECT]" :: !messages;
			let state =
				(* In recover mode we only fail if the last offer was EOF. Since that wasn't shifted,
				let's append it to the rejected tree. *)
				if config.recover then fst (shift (config,tp) state)
				else state
			in
			Reject(!messages,List.rev state.tree,tp.TokenProvider.blocks)

	let start (config,tp) checkpoint =
		if has_debug config DStart then begin
			prerr_endline "[START ]"
		end;
		let state = State.create checkpoint in
		loop (config,tp) state

	let run config tp checkpoint =
		start (config,tp) checkpoint
end