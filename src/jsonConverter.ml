open ParserDriver

module TreeToJson (Api : JsonApi.JsonApi) = struct
	open Token

	let pos_to_json p =
		let open Lexing in
		Api.jint p.pos_cnum

	let range_to_json p1 p2 l =
		("start",pos_to_json p1) :: ("end",pos_to_json p2) :: l

	let convert data tp errors =
		let open WorkList in
		let open LinkedNode in
		let open TokenProvider in
		let tokens = to_list (fun node ->
			let tk,_,_ = fst node.data in
			let flag = match snd node.data with
				| TFImplicit -> ["flag",Api.jstring "implicit"]
				| TFInserted -> ["flag",Api.jstring "inserted"]
				| _ -> []
			in
			Api.jobject (("token",Api.jstring (s_token tk)) :: flag)
		) tp.token_cache.first.next in
		let skipped = to_list (fun node -> Api.jint node.data) tp.skipped_list.first.next in
		let blocks = List.map (fun (p1,p2) -> Api.jobject (range_to_json p1 p2 [])) tp.blocks in
		let errors = List.map Api.jstring errors in
		let js = Api.jobject ["document",Api.jobject [
			"tree",data;
			"tokens",Api.jarray tokens;
			"skipped",Api.jarray skipped;
			"blocks",Api.jarray blocks;
			"errors",Api.jarray errors
		]] in
		js
end