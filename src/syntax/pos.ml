module Position = struct
	type t = Lexing.position

	let print p =
		let open Lexing in
		Printf.sprintf "%i:%i" p.pos_lnum (p.pos_cnum - p.pos_bol)
end

module Location = struct
	type t = {
		file : string;
		position : Position.t;
	}
end

module Range = struct
	type t = {
		first : Position.t;
		last : Position.t;
	}

	let make first last = {
		first = first;
		last = last;
	}

	let null = make (Lexing.dummy_pos) (Lexing.dummy_pos)
end