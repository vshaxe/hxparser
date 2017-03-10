open Token

type t = {
	lexbuf : Sedlex_menhir.lexbuf;
	skipped_list : int WorkList.t;
	mutable inserted_tokens : work_token list;
	mutable token_cache : (placed_token * trivia_flag) WorkList.t;
	mutable branches : ((AstPre.expr * bool) * bool * Lexing.position) list;
	mutable skipped : int;
	mutable in_dead_branch : bool;
	mutable blocks : range list;
}

val next_token : t -> work_token

val insert_token : t -> work_token -> unit

val on_shift : t -> work_token -> trivia

val skip : t -> work_token -> unit

val create : Sedlex_menhir.lexbuf -> t