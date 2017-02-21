module type JsonApi = sig
	type t
	val jarray : t list -> t
	val jobject : (string * t) list -> t
	val jbool : bool -> t
	val jstring : string -> t
	val jint : int -> t
	val jnull : t
end

module TreeToJson (Api : JsonApi) = struct
	open ParserDriver

	let pos_to_json p =
		let open Lexing in
		Api.jint p.pos_cnum

	let rec to_json = function
		| Leaf((token,p1,p2),trivia) ->
			let acc = ref [] in
			begin match trivia.tleading with
				| [] -> ()
				| _ -> acc := ("leading",Api.jarray (List.map to_json trivia.tleading)) :: !acc
			end;
			begin match trivia.ttrailing with
				| [] -> ()
				| _ -> acc := ("trailing",Api.jarray (List.map to_json trivia.ttrailing)) :: !acc
			end;
			List.iter (function
				| TFSkipped -> acc := ("skipped",Api.jbool true) :: !acc
				| TFImplicit -> acc := ("implicit",Api.jbool true) :: !acc
				| TFInserted -> acc := ("inserted",Api.jbool true) :: !acc
			) trivia.tflags;
			let l = ("name",Api.jstring "token") :: ("token",Api.jstring (Token.s_token token)) :: ("start",pos_to_json p1) :: ("end",pos_to_json p2) ::
				(match !acc with | [] -> [] | trivia -> ["trivia",Api.jobject trivia ]) in
			Api.jobject l
		| Node(_,[]) -> Api.jnull
		| Node(name1,[Node(name2,sub)]) -> to_json (Node((if name1 = "" then name2 else name1 ^ " " ^ name2),sub))
		| Node(name,[t1]) ->
			let j = to_json t1 in
			if j = Api.jnull then Api.jnull
			else (match name with "" -> j | _ -> Api.jobject["name",Api.jstring name;"sub",Api.jarray [j]])
		| Node(name,tl) ->
			begin match List.rev tl with
				| Node(name2, tl2) :: tl when name = name2 -> to_json (Node(name,(List.rev tl) @ tl2))
				| _ ->
					let l = List.map to_json tl in
					let l = List.filter (fun j -> j <> Api.jnull) l in
					match l with
					| [] -> Api.jnull
					| _ ->
						let j = Api.jarray l in
						(match name with "" -> j | _ -> Api.jobject ["name",Api.jstring name;"sub",j])
			end
end