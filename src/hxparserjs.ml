open Config

module JSON = struct
	type t = Js.Unsafe.any
	let jobject l = Js.Unsafe.inject (Js.Unsafe.obj (Array.of_list l))
	let jarray l = Js.Unsafe.inject (Js.array (Array.of_list l))
	let jfloat f = Js.Unsafe.inject (Js.number_of_float f)
	let jint i = Js.Unsafe.inject (Js.number_of_float (float_of_int i))
	let jnull = Js.Unsafe.inject Js.null
	let jbool b = Js.Unsafe.inject (Js.bool b)
	let jstring s = Js.Unsafe.inject (Js.string s)
end

module Emitter = JsonEmitter.JsonEmitter(JSON)

module Parser = Parser.Make(Emitter)

module ParserDriver = ParserDriver.Make(Parser.MenhirInterpreter)

open ParserDriver

module JsOfOcamlConverter = JsonConverter.TreeToJson (JSON)

let config = {
	debug_flags = [];
	build_parse_tree = true;
	recover = true;
}

let parse filename entrypoint s =
	let open Sedlex_menhir in
	let s = Js.to_string s in
	let report_error sl =
		Js.Unsafe.inject (Js.string "something went very wrong")
	in
	let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_string s) in
	begin try
		let _ = Lexer.skip_header lexbuf in
		let tp = TokenProvider.create lexbuf in
		let run entrypoint emit =
			begin match run config tp (entrypoint lexbuf.pos) with
			| Reject sl -> report_error sl
			| Accept result -> JsOfOcamlConverter.convert (emit result) tp []
			end;
		in
		begin match Js.to_string entrypoint with
			| "file" ->
				run Parser.Incremental.file (fun (pack,decls) -> Emitter.emit_file pack decls)
			| "decls" ->
				run Parser.Incremental.decls_only JSON.jarray
			| "class_fields" ->
				run Parser.Incremental.class_fields_only JSON.jarray
			| "block_elements" ->
				run Parser.Incremental.block_elements_only JSON.jarray
			| entrypoint ->
				failwith ("Unknown entry point: " ^ entrypoint)
		end
	with exc ->
		report_error [Printexc.to_string exc];
	end;
;;

Js.export "parse" parse
