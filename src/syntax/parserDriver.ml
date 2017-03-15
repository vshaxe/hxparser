open Tokens
open Token
open TokenProvider

module Make (I : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE with type token = token) = struct

	module M = MenhirLib.General

	module State = struct
		type 'a t = {
			recover_state : 'a t;
			checkpoint : 'a I.checkpoint;
			last_offer : work_token;
			last_shift : placed_token;
		}

		let create checkpoint =
			let dummy_token = (EOF,Lexing.dummy_pos,Lexing.dummy_pos) in
			let rec state = {
				checkpoint = checkpoint;
				last_offer = (dummy_token,TFNormal,0);
				last_shift = dummy_token;
				recover_state = state;
			} in
			state
	end

	type 'a result =
		| Accept of 'a
		| Reject of string list

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
		TokenProvider.on_shift tp state.last_offer;
		state,token

	let rec loop (config,tp) state = match state.checkpoint with
		| I.Accepted v ->
			if has_debug config DAccept then begin
				prerr_endline "[ACCEPT]"
			end;
			Accept v
		| I.InputNeeded _ ->
			let token,flag,skipped = TokenProvider.next_token tp in
			let state = offer config state flag skipped token in
			loop (config,tp) state
		| I.Shifting _ ->
			let state,token = shift (config,tp) state in
			let state = {state with checkpoint = I.resume state.checkpoint; last_shift = token} in
			loop (config,tp) state
		| I.AboutToReduce(_,production) ->
			let state = {state with checkpoint = I.resume state.checkpoint} in
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
				loop (config,tp) state
			in
			let token,_,skipped = state.last_offer in
			let split (p1,p2) new1 new2 =
				let open Lexing in
				let p1' = {p1 with pos_cnum = p1.pos_cnum + 1} in
				let token1 = (new1,p1,p1') in
				let token2 = (new2,p1',p2) in
				TokenProvider.insert_token tp (token2,TFNormal,0);
				TokenProvider.insert_token tp (token1,(TFSplit(token,token2)),skipped);
				loop (config,tp) state.recover_state
			in
			let acceptable token = I.acceptable state.recover_state.checkpoint token in
			let fail () =
				loop (config,tp) {state with checkpoint = I.resume state.checkpoint}
			in
			begin match state.last_shift,token with
				| ((BRCLOSE,_,p)),_ when acceptable SEMICOLON p -> insert SEMICOLON true p
				| _,((SHR,p1,p2)) when acceptable GT p1 -> split (p1,p2) GT GT
				| _,((USHR,p1,p2)) when acceptable GT p1 -> split (p1,p2) GT SHR
				| _,((GTE,p1,p2)) when acceptable GT p1 -> split (p1,p2) GT ASSIGN
				| _,((ASSIGNSHR,p1,p2)) when acceptable GT p1 -> split (p1,p2) GT GTE
				| _,((ASSIGNUSHR,p1,p2)) when acceptable GT p1 -> split (p1,p2) GT ASSIGNSHR
				| _ when not config.recover -> fail()
				| ((tk,_,p)),_ ->
					if acceptable SEMICOLON p then insert SEMICOLON false p
					else if acceptable PCLOSE p then insert PCLOSE false p
					else if acceptable BKCLOSE p then insert BKCLOSE false p
					else if tk <> SEMICOLON && acceptable (IDENT "_") p then insert (IDENT "_") false p
					else begin match token with
						| ((EOF,_,_)) ->
							if acceptable BRCLOSE p then insert BRCLOSE false p
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
			(* In recover mode we only fail if the last offer was EOF. Since that wasn't shifted,
			let's append it to the rejected tree. *)
			if config.recover then ignore(shift (config,tp) state);
			Reject !messages

	let start (config,tp) checkpoint =
		if has_debug config DStart then begin
			prerr_endline "[START ]"
		end;
		let state = State.create checkpoint in
		loop (config,tp) state

	let run config tp checkpoint =
		start (config,tp) checkpoint
end