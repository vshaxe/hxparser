module type JsonApi = sig
	type t
	val (* Ben & *) jarray : t list -> t
	val jobject : (string * t) list -> t
	val jbool : bool -> t
	val jstring : string -> t
	val jint : int -> t
	val jnull : t
end

open ParserDriver

module TreeToJson (Api : JsonApi) = struct
	open Token

	let pos_to_json p =
		let open Lexing in
		Api.jint p.pos_cnum

	let range_to_json p1 p2 l =
		("start",pos_to_json p1) :: ("end",pos_to_json p2) :: l

	let convert data tp blocks errors =
		let open WorkList in
		let open LinkedNode in
		let tokens = to_list (fun node ->
			let tk,_,_ = fst node.data in
			Api.jstring (s_token tk)
		) tp.TokenProvider.token_cache.first.next in
		let skipped = to_list (fun node -> Api.jint node.data) tp.TokenProvider.skipped_list.first.next in
		let blocks = List.map (fun (p1,p2) -> Api.jobject (range_to_json p1 p2 [])) blocks in
		let errors = List.map (fun s -> Api.jstring s) errors in
		let errors = Api.jobject ["name",Api.jstring "errors";"sub",Api.jarray errors] in
		let js = Api.jobject ["document",Api.jobject [
			"tree",data;
			"tokens",Api.jarray tokens;
			"skipped",Api.jarray skipped;
			"blocks",Api.jarray blocks;
			"errors",errors
		]] in
		js
end