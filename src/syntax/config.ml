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
	mutable recover : bool;
}

let default_config () = {
	debug_flags = [];
	build_parse_tree = false;
	recover = false;
}