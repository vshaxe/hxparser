open ParserDriver
open Json

let config = default_config()
let quit_early = ref true
let num_files = ref 0
let num_errors = ref 0

let stdin_filename = "<stdin>"

let parse filename =
	let open Sedlex_menhir in
	let ch =
		if filename = stdin_filename then (
			set_binary_mode_in stdin true;
			stdin
		) else
			open_in_bin filename
		in
	let report_error sl =
		List.iter print_endline sl;
		print_endline ("while parsing " ^ filename ^ "\n\n");
		incr num_errors;
		if !quit_early then begin
			close_in ch;
			failwith "Failed"
		end;
	in
	let lexbuf = create_lexbuf ~file:filename (Sedlexing.Utf8.from_channel ch) in
	begin try
		let _ = Lexer.skip_header lexbuf in
		let com = create_common config lexbuf in
		let print_json () = match com.json with
			| [] -> ()
			| l ->
				let ja = List.map (fun ja -> JObject ["name",JString "part";"sub",ja]) l in
				let js = JObject ["name",JString "document";"sub",JArray ja] in
				let buffer = Buffer.create 0 in
				write_json (Buffer.add_string buffer) js;
				prerr_endline (Buffer.contents buffer);
		in
		begin match run com (Parser.Incremental.file lexbuf.pos) with
			| _ when config.output_json ->
				print_json ();
				exit 0
			| Reject(sl,_) -> report_error sl
			| Accept _ -> ()
		end;
	with exc ->
		report_error [Printexc.to_string exc];
	end;
	close_in ch
;;

let read_file filename =
	let lines = ref [] in
	let chan = open_in filename in
	try
		while true; do
			lines := (Str.regexp (input_line chan)) :: !lines
		done; !lines
	with End_of_file ->
		close_in chan;
		List.rev !lines
;;

let ignores = ref (
	let ignores =
		if Sys.file_exists "ignore.txt" then read_file "ignore.txt"
		else []
	in
	(Str.regexp "\\.git") :: ignores
)

let is_ignored s =
	List.exists (fun r ->
		try ignore(Str.search_forward r s 0); true with Not_found -> false
	) !ignores

let explore_class_paths path =
	let rec loop dir =
		try
			let entries = Sys.readdir dir in
			Array.iter (fun file ->
				match file with
					| _ when is_ignored (dir ^ file) ->
						()
					| "." | ".." ->
						()
					| _ when Sys.is_directory (dir ^ file) ->
						loop (dir ^ file ^ "/")
					| _ ->
						let l = String.length file in
						if l > 3 && String.sub file (l - 3) 3 = ".hx" then begin
							let filename = dir ^ file in
							incr num_files;
							parse filename
						end
			) entries;
		with Sys_error _ ->
			()
	in
	loop path;
;;

let usage = Printf.sprintf "haxe.native [options] [files]"
let args_spec = [
	("--json", Arg.Unit (fun () ->
		config.build_parse_tree <- true;
		config.output_json <- true;
	),"generate JSON parse tree");
	("--build-parse-tree", Arg.Unit (fun () ->
		config.build_parse_tree <- true;
	),"build parse tree");
	("--print-accept", Arg.Unit (fun () ->
		config.build_parse_tree <- true;
		config.debug_flags <- DAccept :: config.debug_flags;
	),"print parse tree on accept");
	("--print-sr", Arg.Unit (fun () ->
		config.debug_flags <- DShift :: DReduce :: config.debug_flags;
	),"print shift/reduce");
	("--print-offer", Arg.Unit (fun () ->
		config.debug_flags <- DOffer :: config.debug_flags;
	),"print offer");
	("--print-insert", Arg.Unit (fun () ->
		config.debug_flags <- DInsert :: config.debug_flags;
	),"print insertions");
	("--print-reject", Arg.Unit (fun () ->
		config.debug_flags <- DReject :: config.debug_flags;
	),"print rejections");
	("--print-start", Arg.Unit (fun () ->
		config.debug_flags <- DStart :: config.debug_flags;
	),"print start");
	("--keep-going", Arg.Unit (fun () ->
		quit_early := false
	),"don't quit if there's an exception");
]
let paths = ref []
let process args =
	let current = ref 0 in
	let args_callback s =
		paths := s :: !paths
	in
	(try
		Arg.parse_argv ~current (Array.of_list ("" :: args)) args_spec args_callback usage;
	with Arg.Bad _ ->
		())
;;
let args = (List.tl (Array.to_list Sys.argv)) in
process args;;
if !paths = [] then
	Arg.usage args_spec usage
else begin
	let now = Sys.time () in
	List.iter (fun s ->
		if s = stdin_filename then
			parse s
		else if not (Sys.file_exists s) then
			print_endline ("No such file or directory: " ^ s)
		else begin
			if Sys.is_directory s then begin
				print_endline ("Reading directory " ^ s);
				explore_class_paths (s ^ "/")
			end
				else parse s
		end
	) !paths;
	print_endline (Printf.sprintf "Parsed %i files, %i failed" !num_files !num_errors);
	print_endline (string_of_float (Sys.time () -. now) ^ "s");
end