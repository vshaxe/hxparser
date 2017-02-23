open ParserDriver
open ParserDriver.Config

module JsOfOcamlConverter = JsonConverter.TreeToJson(struct
	type t = Js.Unsafe.any
	let jobject l = Js.Unsafe.inject (Js.Unsafe.obj (Array.of_list l))
	let jarray l = Js.Unsafe.inject (Js.array (Array.of_list l))
	let jfloat f = Js.Unsafe.inject (Js.number_of_float f)
	let jint i = Js.Unsafe.inject (Js.number_of_float (float_of_int i))
	let jnull = Js.Unsafe.inject Js.null
	let jbool b = Js.Unsafe.inject (Js.bool b)
	let jstring s = Js.Unsafe.inject (Js.string s)
end)

let config = {
	debug_flags = [];
	build_parse_tree = true;
	recover = true;
}

let parse filename entrypoint s =
	let open Sedlex_menhir in
	let report_error sl =
		List.iter print_endline sl;
		print_endline ("while parsing " ^ filename ^ "\n\n");
		failwith "Failed"
	in
	let s = Js.to_string s in
	let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_string s) in
	begin try
		let _ = Lexer.skip_header lexbuf in
		begin match entrypoint with
			| "file" ->
				begin match run config lexbuf (Parser.Incremental.file lexbuf.pos) with
				| Reject(sl,_) -> report_error sl
				| Accept(_,tree,blocks) -> JsOfOcamlConverter.convert tree blocks
				end;
			| "class_field" ->
				begin match run config lexbuf (Parser.Incremental.class_field_only lexbuf.pos) with
				| Reject(sl,_) -> report_error sl
				| Accept(_,tree,blocks) -> JsOfOcamlConverter.convert tree blocks
				end;
			| _ -> failwith ("Unknown entry point: " ^ entrypoint)
		end
	with exc ->
		report_error [Printexc.to_string exc];
	end;
;;

Js.export "parse" parse
