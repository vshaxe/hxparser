(*
	The Haxe Compiler
	Copyright (C) 2005-2017  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Globals
open Ops

type pos = Pos.Range.t

type while_flag =
	| NormalWhile
	| DoWhile

type type_path = {
	tpackage : string list;
	tname : string;
	tparams : type_param_or_const list;
	tsub : string option;
}

and placed_type_path = type_path * pos

and type_param_or_const =
	| TPType of type_hint
	| TPExpr of expr

and complex_type =
	| CTPath of type_path
	| CTFunction of type_hint list * type_hint
	| CTAnonymous of class_field list
	| CTParent of type_hint
	| CTExtend of placed_type_path list * class_field list
	| CTOptional of type_hint
	| CTNamed of placed_name * type_hint

and type_hint = complex_type * pos

and func = {
	f_params : type_param list;
	f_args : (placed_name * bool * metadata * type_hint option * expr option) list;
	f_type : type_hint option;
	f_expr : expr option;
}

and placed_name = string * pos

and expr_def =
	| EConst of constant
	| EArray of expr * expr
	| EBinop of binop * expr * expr
	| EField of expr * string
	| EParenthesis of expr
	| EObjectDecl of (placed_name * expr) list
	| EArrayDecl of expr list
	| ECall of expr * expr list
	| ENew of placed_type_path * expr list
	| EUnop of unop * unop_flag * expr
	| EVars of (placed_name * type_hint option * expr option) list
	| EFunction of string option * func
	| EBlock of expr list
	| EFor of expr * expr
	| EIn of expr * expr
	| EIf of expr * expr * expr option
	| EWhile of expr * expr * while_flag
	| ESwitch of expr * (expr list * expr option * expr option * pos) list * (expr option * pos) option
	| ETry of expr * (placed_name * type_hint * expr * pos) list
	| EReturn of expr option
	| EBreak
	| EContinue
	| EUntyped of expr
	| EThrow of expr
	| ECast of expr * type_hint option
	| EDisplay of expr * bool
	| EDisplayNew of placed_type_path
	| ETernary of expr * expr * expr
	| ECheckType of expr * type_hint
	| EMeta of metadata_entry * expr

and expr = expr_def * pos

and type_param = {
	tp_name : placed_name;
	tp_params :	type_param list;
	tp_constraints : type_hint list;
	tp_meta : metadata;
}

and documentation = string option

and metadata_entry = (Meta.strict_meta * expr list * pos)
and metadata = metadata_entry list

and access =
	| APublic
	| APrivate
	| AStatic
	| AOverride
	| ADynamic
	| AInline
	| AMacro

and class_field_kind =
	| FVar of type_hint option * expr option
	| FFun of func
	| FProp of placed_name * placed_name * type_hint option * expr option

and class_field = {
	cff_name : placed_name;
	cff_doc : documentation;
	cff_pos : pos;
	mutable cff_meta : metadata;
	mutable cff_access : access list;
	mutable cff_kind : class_field_kind;
}

type enum_flag =
	| EPrivate
	| EExtern

type class_flag =
	| HInterface
	| HExtern
	| HPrivate
	| HExtends of placed_type_path
	| HImplements of placed_type_path

type abstract_flag =
	| APrivAbstract
	| AFromType of type_hint
	| AToType of type_hint
	| AIsType of type_hint
	| AExtern

type enum_constructor = {
	ec_name : placed_name;
	ec_doc : documentation;
	ec_meta : metadata;
	ec_args : (string * bool * type_hint) list;
	ec_pos : pos;
	ec_params : type_param list;
	ec_type : type_hint option;
}

type ('a,'b) definition = {
	d_name : placed_name;
	d_doc : documentation;
	d_params : type_param list;
	d_meta : metadata;
	d_flags : 'a list;
	d_data : 'b;
}

type import_mode =
	| INormal
	| IAsName of string
	| IAll

type import = placed_name list * import_mode

type type_def =
	| EClass of (class_flag, class_field list) definition
	| EEnum of (enum_flag, enum_constructor list) definition
	| ETypedef of (enum_flag, type_hint) definition
	| EAbstract of (abstract_flag, class_field list) definition
	| EImport of import
	| EUsing of placed_name list

type type_decl = type_def * pos

type package = string list * type_decl list

let pos = snd

let s_escape ?(hex=true) s =
	let b = Buffer.create (String.length s) in
	for i = 0 to (String.length s) - 1 do
		match s.[i] with
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| c when int_of_char c < 32 && hex -> Buffer.add_string b (Printf.sprintf "\\x%.2X" (int_of_char c))
		| c -> Buffer.add_char b c
	done;
	Buffer.contents b

let s_constant = function
	| Int s -> s
	| Float s -> s
	| String s -> "\"" ^ s_escape s ^ "\""
	| Ident s -> s
	| Regexp (r,o) -> "~/" ^ r ^ "/"

let s_access = function
	| APublic -> "public"
	| APrivate -> "private"
	| AStatic -> "static"
	| AOverride -> "override"
	| ADynamic -> "dynamic"
	| AInline -> "inline"
	| AMacro -> "macro"

let rec s_binop = function
	| OpAdd -> "+"
	| OpMult -> "*"
	| OpDiv -> "/"
	| OpSub -> "-"
	| OpAssign -> "="
	| OpEq -> "=="
	| OpNotEq -> "!="
	| OpGte -> ">="
	| OpLte -> "<="
	| OpGt -> ">"
	| OpLt -> "<"
	| OpAnd -> "&"
	| OpOr -> "|"
	| OpXor -> "^"
	| OpBoolAnd -> "&&"
	| OpBoolOr -> "||"
	| OpShr -> ">>"
	| OpUShr -> ">>>"
	| OpShl -> "<<"
	| OpMod -> "%"
	| OpAssignOp op -> s_binop op ^ "="
	| OpInterval -> "..."
	| OpArrow -> "=>"

let s_unop = function
	| Increment -> "++"
	| Decrement -> "--"
	| Not -> "!"
	| Neg -> "-"
	| NegBits -> "~"

let s_expr e =
	let rec s_expr_inner tabs (e,_) =
		match e with
		| EConst c -> s_constant c
		| EArray (e1,e2) -> s_expr_inner tabs e1 ^ "[" ^ s_expr_inner tabs e2 ^ "]"
		| EBinop (op,e1,e2) -> s_expr_inner tabs e1 ^ " " ^ s_binop op ^ " " ^ s_expr_inner tabs e2
		| EField (e,f) -> s_expr_inner tabs e ^ "." ^ f
		| EParenthesis e -> "(" ^ (s_expr_inner tabs e) ^ ")"
		| EObjectDecl fl -> "{ " ^ (String.concat ", " (List.map (fun ((n,_),e) -> n ^ " : " ^ (s_expr_inner tabs e)) fl)) ^ " }"
		| EArrayDecl el -> "[" ^ s_expr_list tabs el ", " ^ "]"
		| ECall (e,el) -> s_expr_inner tabs e ^ "(" ^ s_expr_list tabs el ", " ^ ")"
		| ENew (t,el) -> "new " ^ s_complex_type_path tabs t ^ "(" ^ s_expr_list tabs el ", " ^ ")"
		| EUnop (op,Postfix,e) -> s_expr_inner tabs e ^ s_unop op
		| EUnop (op,Prefix,e) -> s_unop op ^ s_expr_inner tabs e
		| EFunction (Some n,f) -> "function " ^ n ^ s_func tabs f
		| EFunction (None,f) -> "function" ^ s_func tabs f
		| EVars vl -> "var " ^ String.concat ", " (List.map (s_var tabs) vl)
		| EBlock [] -> "{ }"
		| EBlock el -> s_block tabs el "{" "\n" "}"
		| EFor (e1,e2) -> "for (" ^ s_expr_inner tabs e1 ^ ") " ^ s_expr_inner tabs e2
		| EIn (e1,e2) -> s_expr_inner tabs e1 ^ " in " ^ s_expr_inner tabs e2
		| EIf (e,e1,None) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1
		| EIf (e,e1,Some e2) -> "if (" ^ s_expr_inner tabs e ^ ") " ^ s_expr_inner tabs e1 ^ " else " ^ s_expr_inner tabs e2
		| EWhile (econd,e,NormalWhile) -> "while (" ^ s_expr_inner tabs econd ^ ") " ^ s_expr_inner tabs e
		| EWhile (econd,e,DoWhile) -> "do " ^ s_expr_inner tabs e ^ " while (" ^ s_expr_inner tabs econd ^ ")"
		| ESwitch (e,cases,def) -> "switch " ^ s_expr_inner tabs e ^ " {\n\t" ^ tabs ^ String.concat ("\n\t" ^ tabs) (List.map (s_case tabs) cases) ^
			(match def with None -> "" | Some (def,_) -> "\n\t" ^ tabs ^ "default:" ^
			(match def with None -> "" | Some def -> s_expr_omit_block tabs def)) ^ "\n" ^ tabs ^ "}"
		| ETry (e,catches) -> "try " ^ s_expr_inner tabs e ^ String.concat "" (List.map (s_catch tabs) catches)
		| EReturn e -> "return" ^ s_opt_expr tabs e " "
		| EBreak -> "break"
		| EContinue -> "continue"
		| EUntyped e -> "untyped " ^ s_expr_inner tabs e
		| EThrow e -> "throw " ^ s_expr_inner tabs e
		| ECast (e,Some (t,_)) -> "cast (" ^ s_expr_inner tabs e ^ ", " ^ s_complex_type tabs t ^ ")"
		| ECast (e,None) -> "cast " ^ s_expr_inner tabs e
		| ETernary (e1,e2,e3) -> s_expr_inner tabs e1 ^ " ? " ^ s_expr_inner tabs e2 ^ " : " ^ s_expr_inner tabs e3
		| ECheckType (e,(t,_)) -> "(" ^ s_expr_inner tabs e ^ " : " ^ s_complex_type tabs t ^ ")"
		| EMeta (m,e) -> s_metadata tabs m ^ " " ^ s_expr_inner tabs e
		| EDisplay (e1,iscall) -> Printf.sprintf "#DISPLAY(%s, %b)" (s_expr_inner tabs e1) iscall
		| EDisplayNew tp -> Printf.sprintf "#DISPLAY_NEW(%s)" (s_complex_type_path tabs tp)
	and s_expr_list tabs el sep =
		(String.concat sep (List.map (s_expr_inner tabs) el))
	and s_complex_type_path tabs (t,_) =
		Printf.sprintf "%s%s%s"
			(s_type_path (t.tpackage,t.tname))
			(match t.tsub with None -> "" | Some s -> "." ^ s)
			(s_type_param_or_consts tabs t.tparams)
	and s_type_param_or_consts tabs pl =
		if List.length pl > 0
		then "<" ^ (String.concat "," (List.map (s_type_param_or_const tabs) pl)) ^ ">"
		else ""
	and s_type_param_or_const tabs p =
		match p with
		| TPType (t,_) -> s_complex_type tabs t
		| TPExpr e -> s_expr_inner tabs e
	and s_complex_type tabs ct =
		match ct with
		| CTPath t -> s_complex_type_path tabs (t,Pos.Range.null)
		| CTFunction (cl,(c,_)) -> if List.length cl > 0 then String.concat " -> " (List.map (fun (t,_) -> s_complex_type tabs t) cl) else "Void" ^ " -> " ^ s_complex_type tabs c
		| CTAnonymous fl -> "{ " ^ String.concat "; " (List.map (s_class_field tabs) fl) ^ "}";
		| CTParent(t,_) -> "(" ^ s_complex_type tabs t ^ ")"
		| CTOptional(t,_) -> "?" ^ s_complex_type tabs t
		| CTExtend (tl, fl) -> "{> " ^ String.concat " >, " (List.map (s_complex_type_path tabs) tl) ^ ", " ^ String.concat ", " (List.map (s_class_field tabs) fl) ^ " }"
		| CTNamed ((n,_),(t,_)) -> n ^ " : " ^ s_complex_type tabs t
	and s_class_field tabs f =
		match f.cff_doc with
		| Some s -> "/**\n\t" ^ tabs ^ s ^ "\n**/\n"
		| None -> "" ^
		if List.length f.cff_meta > 0 then String.concat ("\n" ^ tabs) (List.map (s_metadata tabs) f.cff_meta) else "" ^
		if List.length f.cff_access > 0 then String.concat " " (List.map s_access f.cff_access) else "" ^
		match f.cff_kind with
		| FVar (t,e) -> "var " ^ (fst f.cff_name) ^ s_opt_type_hint tabs t " : " ^ s_opt_expr tabs e " = "
		| FProp ((get,_),(set,_),t,e) -> "var " ^ (fst f.cff_name) ^ "(" ^ get ^ "," ^ set ^ ")" ^ s_opt_type_hint tabs t " : " ^ s_opt_expr tabs e " = "
		| FFun func -> "function " ^ (fst f.cff_name) ^ s_func tabs func
	and s_metadata tabs (s,e,_) =
		"@" ^ Meta.to_string s ^ if List.length e > 0 then "(" ^ s_expr_list tabs e ", " ^ ")" else ""
	and s_opt_expr tabs e pre =
		match e with
		| Some s -> pre ^ s_expr_inner tabs s
		| None -> ""
	and s_opt_type_hint tabs t pre =
		match t with
		| Some(t,_) -> pre ^ s_complex_type tabs t
		| None -> ""
	and s_func tabs f =
		s_type_param_list tabs f.f_params ^
		"(" ^ String.concat ", " (List.map (s_func_arg tabs) f.f_args) ^ ")" ^
		s_opt_type_hint tabs f.f_type ":" ^
		s_opt_expr tabs f.f_expr " "
	and s_type_param tabs t =
		fst (t.tp_name) ^ s_type_param_list tabs t.tp_params ^
		if List.length t.tp_constraints > 0 then ":(" ^ String.concat ", " (List.map ((fun (t,_) -> s_complex_type tabs t)) t.tp_constraints) ^ ")" else ""
	and s_type_param_list tabs tl =
		if List.length tl > 0 then "<" ^ String.concat ", " (List.map (s_type_param tabs) tl) ^ ">" else ""
	and s_func_arg tabs ((n,_),o,_,t,e) =
		if o then "?" else "" ^ n ^ s_opt_type_hint tabs t ":" ^ s_opt_expr tabs e " = "
	and s_var tabs ((n,_),t,e) =
		n ^ (s_opt_type_hint tabs t ":") ^ s_opt_expr tabs e " = "
	and s_case tabs (el,e1,e2,_) =
		"case " ^ s_expr_list tabs el ", " ^
		(match e1 with None -> ":" | Some e -> " if (" ^ s_expr_inner tabs e ^ "):") ^
		(match e2 with None -> "" | Some e -> s_expr_omit_block tabs e)
	and s_catch tabs ((n,_),(t,_),e,_) =
		" catch(" ^ n ^ ":" ^ s_complex_type tabs t ^ ") " ^ s_expr_inner tabs e
	and s_block tabs el opn nl cls =
		 opn ^ "\n\t" ^ tabs ^ (s_expr_list (tabs ^ "\t") el (";\n\t" ^ tabs)) ^ ";" ^ nl ^ tabs ^ cls
	and s_expr_omit_block tabs e =
		match e with
		| (EBlock [],_) -> ""
		| (EBlock el,_) -> s_block (tabs ^ "\t") el "" "" ""
		| _ -> s_expr_inner (tabs ^ "\t") e ^ ";"
	in s_expr_inner "" e

let expr_of_type_path (sl,s) p =
	match sl with
	| [] -> (EConst(Ident s),p)
	| s1 :: sl ->
		let e1 = (EConst(Ident s1),p) in
		let e = List.fold_left (fun e s -> (EField(e,s),p)) e1 sl in
		EField(e,s),p