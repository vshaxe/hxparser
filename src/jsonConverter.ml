module type JsonApi = sig
	type t
	val (* Ben & *) jarray : t list -> t
	val jobject : (string * t) list -> t
	val jbool : bool -> t
	val jstring : string -> t
	val jint : int -> t
	val jnull : t
end

module TreeToJson (Api : JsonApi) = struct
	open ParserDriver
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
			begin match trivia.ttrailing with
				| [] -> ()
				| _ -> acc := ("trailing",Api.jarray (List.map (fun token -> Api.jobject (jtoken token [])) trivia.ttrailing)) :: !acc
			end;
			List.iter (function
				| TFSkipped -> acc := ("skipped",Api.jbool true) :: !acc
				| TFImplicit -> acc := ("implicit",Api.jbool true) :: !acc
				| TFInserted -> acc := ("inserted",Api.jbool true) :: !acc
			) trivia.tflags;
			let l = ("name",Api.jstring "token") :: jtoken token (match !acc with | [] -> [] | trivia -> ["trivia",Api.jobject trivia ]) in
			Api.jobject l
		| Node(_,[]) -> Api.jnull
		| Node(sym,[t1]) ->
			let name = s_xsymbol sym in
			let j = to_json t1 in
			if j = Api.jnull then Api.jnull
			else (match name with "" -> j | _ -> Api.jobject["name",Api.jstring name;"sub",Api.jarray [j]])
		| Node(sym,tl) ->
			let name = s_xsymbol sym in
			begin match List.rev tl with
				| Node(sym2, tl2) :: tl when name = (s_xsymbol sym2) -> to_json (Node(sym,(List.rev tl) @ tl2))
				| _ ->
					let l = List.map to_json tl in
					let l = List.filter (fun j -> j <> Api.jnull) l in
					match l with
					| [] -> Api.jnull
					| _ ->
						let j = Api.jarray l in
						(match name with "" -> j | _ -> Api.jobject ["name",Api.jstring name;"sub",j])
			end

	let convert tree blocks =
		let tree = List.map to_json tree in
		let tree = Api.jobject ["name",Api.jstring "tree";"sub",Api.jarray tree] in
		let blocks = List.map (fun (p1,p2) -> Api.jobject (range_to_json p1 p2 [])) blocks in
		let blocks = Api.jobject ["name",Api.jstring "blocks";"sub",Api.jarray blocks] in
		let js = Api.jobject ["document",Api.jobject ["tree",tree;"blocks",blocks]] in
		js
end