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

module TreeToJson (Api : JsonApi) (E : Engine) = struct

	open E
	open Token

	let pos_to_json p =
		let open Lexing in
		Api.jint p.pos_cnum

	let range_to_json p1 p2 l =
		("start",pos_to_json p1) :: ("end",pos_to_json p2) :: l

	let rec to_json = function
		| Leaf(token,trivia) ->
			let acc = ref [] in
			let jtoken (tk,p1,p2) l =
				("token",Api.jstring (Token.s_token tk)) :: range_to_json p1 p2 l
			in
			begin match trivia.tleading with
				| [] -> ()
				| _ -> acc := ("leading",Api.jarray (List.map (fun token -> Api.jobject (jtoken token [])) trivia.tleading)) :: !acc
			end;
			begin match !(trivia.ttrailing) with
				| [] -> ()
				| l -> acc := ("trailing",Api.jarray (List.map (fun token -> Api.jobject (jtoken token [])) l)) :: !acc
			end;
			List.iter (function
				| TFSkipped -> acc := ("skipped",Api.jbool true) :: !acc
				| TFImplicit -> acc := ("implicit",Api.jbool true) :: !acc
				| TFInserted -> acc := ("inserted",Api.jbool true) :: !acc
				| TFNormal | TFSplit _ -> ()
			) trivia.tflags;
			let l = ("name",Api.jstring "token") :: jtoken token (match !acc with | [] -> [] | trivia -> ["trivia",Api.jobject trivia ]) in
			Api.jobject l
		| Node(sym,[]) -> Api.jnull
		| Node(sym,[t1]) when (match s_xsymbol sym with "" | "?" -> true | _ -> false) -> to_json t1
		| Node(sym,tl) ->
			let name = s_xsymbol sym in
			begin match List.rev tl with
				| Node(sym2, tl2) :: tl when name = (s_xsymbol sym2) -> to_json (Node(sym,(List.rev tl) @ tl2))
				| _ ->
					let l = List.map to_json tl in
					begin match List.for_all (fun t -> t = Api.jnull) l with
					| true -> Api.jnull
					| false ->
						let j = Api.jarray l in
						Api.jobject ["name",Api.jstring name;"sub",j]
					end
			end

	let convert tree tp blocks errors =
		let tree = List.map to_json tree in
		let tree = Api.jobject ["name",Api.jstring "tree";"sub",Api.jarray tree] in
		let open WorkList in
		let open LinkedNode in
		let tokens = to_list (fun node ->
			let tk,_,_ = fst node.data in
			let flag = match snd node.data with
				| TFImplicit -> ["flag",Api.jstring "implicit"]
				| TFInserted -> ["flag",Api.jstring "inserted"]
				| _ -> []
			in
			Api.jobject (("token",Api.jstring (s_token tk)) :: flag)
		) tp.TokenProvider.token_cache.first.next in
		let skipped = to_list (fun node -> Api.jint node.data) tp.TokenProvider.skipped_list.first.next in
		let blocks = List.map (fun (p1,p2) -> Api.jobject (range_to_json p1 p2 [])) blocks in
		let errors = List.map (fun s -> Api.jstring s) errors in
		let errors = Api.jobject ["name",Api.jstring "errors";"sub",Api.jarray errors] in
		let js = Api.jobject ["document",Api.jobject [
			"tree",tree;
			"tokens",Api.jarray tokens;
			"skipped",Api.jarray skipped;
			"blocks",Api.jarray blocks;
			"errors",errors
		]] in
		js
end