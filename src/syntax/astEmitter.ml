open Ast
open Ops

type 'a t_pos = 'a * Pos.Range.t
type t_metadata_entry = metadata_entry
type t_annotations = string option * metadata
type t_path = string list
type t_pos_path = placed_name list
type t_literal = constant
type t_const = constant

type t_block_element = expr
type t_object_field_name = (string * pos * Ast.quote_status)
type t_object_field = (t_object_field_name * expr)
type t_call_args = expr list
type t_var_declaration = (placed_name * type_hint option * expr option)
type t_catch = (placed_name * type_hint * expr * pos)
type t_case = (expr list * expr option * expr option * pos)
type t_expr = expr

type t_constraint = type_hint list
type t_type_path_parameter_kind = type_param_or_const
type t_type_path = placed_type_path
type t_complex_type = type_hint

type t_function_argument = (placed_name * bool * metadata * type_hint option * expr option)
type t_function = string option * func
type t_field_expr = expr option
type t_modifier = placed_access
type t_class_field = class_field
type t_enum_field_arg = (string * bool * type_hint)
type t_enum_field = enum_constructor
type t_anonymous_field = class_field
type t_anonymous_type_fields = class_field list

type t_type_decl_parameter = type_param
type t_import_mode = import_mode
type t_abstract_relation = abstract_flag
type t_class_relation = class_flag
type t_common_flag = enum_flag
type t_class_flag = class_flag
type t_class = (t_class_flag * string t_pos option * t_type_decl_parameter list * t_class_relation list * t_class_field list)
type t_decl = type_decl
type t_package = string list

type t_lambda_arg = (t_expr * bool * unit list * t_complex_type option * t_expr option) * t_function_argument list

let emit_path ident idents =
	(ident :: idents)

let emit_pos_path ident idents =
	(ident :: idents)

let emit_metadata_entry meta p =
	(meta,[],p)

let emit_metadata_entry_with_args meta el p =
	(meta,el,p)

let emit_function name tl args cto e =
	let f = {
		f_params = tl;
		f_type = cto;
		f_args = args;
		f_expr = Some e;
	} in
	name,f

let emit_lambda_arg args =
	let rec loop acc eo e = match fst e with
		| EMeta(meta,e) -> loop (meta :: acc) eo e
		| EConst(Ident s) -> (s,snd e),eo,List.rev acc
		| EBinop(OpAssign,(EConst(Ident s),p),e1) -> (s,snd e),Some e1,List.rev acc
		| _ -> assert false
	in
	let e,opt,_,ct,eo = fst args  in
	let e,eo,meta = loop [] eo e in
	let args = (e,opt,meta,ct,eo) :: (snd args) in
	args

let emit_function2 name tl args cto e =
	let f = {
		f_params = tl;
		f_type = cto;
		f_args = args;
		f_expr = Some e;
	} in
	name,f

let emit_annotations so metadata =
	(so,metadata)

let emit_literal_string s =
	String s

let emit_literal_int s =
	Int s

let emit_literal_float s =
	Float s

let emit_literal_regex (s1,s2) =
	Regexp(s1,s2)

let emit_const_ident s =
	Ident s

let emit_const_literal lit =
	lit

let emit_call_args el =
	el

let emit_assignment e =
	e

let emit_var_declaration name cto eo =
	(name,cto,eo)

let emit_catch name ct e p =
	(name,ct,e,p)

let emit_case el1 eo el2 p =
	let b = match el2 with [] -> None | _ -> Some (EBlock el2,p) in
	(el1,eo,b,p)

let emit_default el p =
	let b = match el with [] -> None | _ -> Some (EBlock el,p) in
	[EConst(Ident "_"),p],None,b,p

let emit_object_field name e =
	(name,e)

let emit_object_field_name_ident s p =
	s,p,NoQuotes

let emit_object_field_name_string s p =
	s,p,DoubleQuotes

let emit_unknown p =
	EConst(Ident "null"),p

let emit_block_element_var vl p =
	EVars vl,p

let emit_block_element_inline_function f p =
	EFunction(fst f,snd f),p

let emit_block_element_expr e p =
	e

let emit_this p =
	EConst(Ident "this"),p

let emit_true p =
	EConst(Ident "true"),p

let emit_false p =
	EConst(Ident "false"),p

let emit_null p =
	EConst(Ident "null"),p

let emit_block_expr el p =
	EBlock el,p

let emit_var_declaration_expr v p =
	EVars [v],p

let emit_metadata_expr meta e p =
	EMeta(meta,e),p

let emit_throw_expr e p =
	EThrow e,p

let emit_if_expr e1 e2 eo p =
	EIf(e1,e2,eo),p

let emit_return_expr eo p =
	EReturn eo,p

let emit_break_expr p =
	EBreak,p

let emit_continue_expr p =
	EContinue,p

let emit_do_expr e1 e2 p =
	EWhile(e1,e2,DoWhile),p

let emit_try_expr e catches p =
	ETry(e,catches),p

let emit_switch_expr e cases p =
	ESwitch(e,cases,None),p

let emit_for_expr e1 e2 p =
	EFor(e1,e2),p

let emit_while_expr e1 e2 p =
	EWhile(e1,e2,NormalWhile),p

let emit_untyped_expr e p =
	EUntyped e,p

let emit_object_decl_expr fl p =
	EObjectDecl fl,p

let emit_unsafe_cast_expr e p =
	ECast(e,None),p

let emit_safe_cast_expr e ct p =
	ECast(e,Some ct),p

let emit_new_expr path el p =
	ENew(path,el),p

let emit_parenthesis_expr e p =
	EParenthesis e,p

let emit_typecheck_expr e ct p =
	ECheckType(e,ct),p

let emit_is_expr e path p_is p =
	let t,t_p = path in
	let e_is = EField((EConst(Ident "Std"),Pos.Range.null),"is"),p_is in
	let e2 = expr_of_type_path (t.tpackage,t.tname) t_p in
	ECall(e_is,[e;e2]),p

let emit_array_decl_expr el p =
	EArrayDecl el,p

let emit_function_expr f p =
	EFunction(fst f,snd f),p

let emit_unary_prefix_expr op e p =
	EUnop(op,Prefix,e),p

let emit_field_expr e s p =
	EField(e,s),p

let emit_call_expr e el p =
	ECall(e,el),p

let emit_array_expr e1 e2 p =
	EArray(e1,e2),p

let emit_binop_expr e1 op e2 p =
	EBinop(op,e1,e2),p

let emit_unary_postfix_expr e op p =
	EUnop(op,Postfix,e),p

let emit_ternary_expr e1 e2 e3 p =
	ETernary(e1,e2,e3),p

let emit_in_expr e1 e2 p =
	EBinop(OpIn,e1,e2),p

let emit_dotint_expr s p =
	EConst(Float(s ^ ".")),p

let emit_dollarident_expr name p =
	EConst (Ident name),p

let emit_macro_escape_expr name e p =
	EMeta(((Meta.Custom (fst name)),[],snd name),e),p

let emit_macro_expr e p = e (* TODO *)

let emit_const_expr const p = (EConst const,p)

(* Type hints *)

let emit_type_path path params p =
	match List.rev path with
	| [] -> assert false
	| name :: pack -> { tpackage = List.rev pack; tname = name; tsub = None; tparams = params },p

let emit_complex_type_path path p = CTPath (fst path),p
let emit_complex_type_parent ct p = CTParent ct,p
let emit_complex_type_extension paths fields p = CTExtend(paths,fields),p
let emit_complex_type_anonymous fields p = CTAnonymous fields,p
let emit_complex_type_optional ct p = CTOptional ct,p

let emit_complex_type_function ct1 ct2 p =
	match fst ct2 with
	| CTFunction (args,r) ->
		CTFunction (ct1 :: args,r),p
	| _ ->
		CTFunction ([ct1],ct2),p

let emit_type_path_parameter_complex_type ct =
	TPType ct

let emit_type_path_parameter_bracket el p =
	TPExpr (EArrayDecl el,p)

let emit_type_path_parameter_literal lit p =
	TPExpr (EConst lit,p)

(* Fields *)

let emit_function_argument annotations opt name cto eo =
	(name,(match opt with None -> false | Some _ -> true),snd annotations,cto,eo)

let emit_field_expr_none = None
let emit_field_expr_block e = Some e
let emit_field_expr_expr e = Some e

let emit_static_modifier p = AStatic,p
let emit_macro_modifier p = AMacro,p
let emit_public_modifier p = APublic,p
let emit_private_modifier p = APrivate,p
let emit_override_modifier p = AOverride,p
let emit_dynamic_modifier p = ADynamic,p
let emit_inline_modifier p = AInline,p
let emit_extern_modifier p = AExtern,p

let emit_function_field annotations modifiers name tl args cto e p =
	let f = {
		f_params = tl;
		f_args = args;
		f_type = cto;
		f_expr = e;
	} in
	let cff = {
		cff_name = name;
		cff_doc = fst annotations;
		cff_pos = p;
		cff_meta = snd annotations;
		cff_access = modifiers;
		cff_kind = FFun f
		} in
		cff

let emit_variable_field annotations modifiers name cto eo p =
	let cff = {
		cff_name = name;
		cff_doc = fst annotations;
		cff_pos = p;
		cff_meta = snd annotations;
		cff_access = modifiers;
		cff_kind = FVar(cto,eo);
		} in
		cff

let emit_property_field annotations modifiers name get set cto eo p =
	let cff = {
		cff_name = name;
		cff_doc = fst annotations;
		cff_pos = p;
		cff_meta = snd annotations;
		cff_access = modifiers;
		cff_kind = FProp(get,set,cto,eo);
		} in
		cff

let emit_enum_field_arg opt name ct =
	(name,(match opt with None -> false | Some _ -> true),ct)

let emit_enum_field annotations name tl args cto p =
	let ef = {
		ec_name = name;
		ec_doc = fst annotations;
		ec_meta = snd annotations;
		ec_args = args;
		ec_params = tl;
		ec_type = cto;
		ec_pos = p;
	} in
	ef

let emit_anonymous_class_fields fields = fields
let emit_anonymous_type_fields fields = fields

let emit_anonymous_type_field opt name ct p =
	let cff = {
		cff_name = name;
		cff_meta = (match opt with None -> [] | Some _ -> [Meta.Optional,[],Pos.Range.null]);
		cff_access = [];
		cff_doc = None;
		cff_kind = FVar(Some ct,None);
		cff_pos = p;
	} in
	cff

(* Type declaration *)

let emit_constraints_none = []

let emit_constraints_single ct = [ct]

let emit_constraints_multiple ctl = ctl

let emit_type_decl_parameter annotations name constraints = {
	tp_name = name;
	tp_params = []; (* TODO! *)
	tp_constraints = constraints;
	tp_meta = snd annotations;
}

let emit_class_flag_interface = HInterface

let emit_class_flag_class = HInterface (* TODO! *)

let emit_common_flag_extern = EExtern

let emit_common_flag_private = EPrivate

let emit_class_relation_extends path = HExtends path

let emit_class_relation_implements path = HImplements path

let emit_abstract_relation_from ct = AbFrom ct

let emit_abstract_relation_to ct = AbTo ct

let emit_import_mode_all = IAll

let emit_import_mode_alias name = IAsName name

let emit_import_mode_normal = INormal

let emit_import path mode p =
	(EImport (path,mode),p)

let emit_using path p =
	(EUsing path,p)

let emit_class flags name tl rl l =
	(flags,name,tl,rl,l)

let emit_class_decl annotations flags c p =
	let (flags2,name,tl,rl,l) = c in
	let flags = List.map (function
		| EPrivate -> HPrivate
		| EExtern -> HExtern
	) flags in
	let def = {
		d_name = (match name with None -> "",p (* TODO: syntax error... *) | Some name -> name);
		d_doc = fst annotations;
		d_params = tl;
		d_meta = snd annotations;
		d_flags = flags2 :: flags @ rl;
		d_data = l;
	} in
	(EClass def,p)

let emit_enum_decl annotations flags name tl l p =
	let def = {
		d_name = name;
		d_doc = fst annotations;
		d_params = tl;
		d_meta = snd annotations;
		d_flags = flags;
		d_data = l;
	} in
	(EEnum def,p)

let emit_typedef_decl annotations flags name tl ct p =
	let def = {
		d_name = name;
		d_doc = fst annotations;
		d_params = tl;
		d_meta = snd annotations;
		d_flags = flags;
		d_data = ct
	} in
	(ETypedef def,p)

let emit_abstract_decl annotations flags name tl st rl l p =
	let flags = List.map (fun c -> match c with EPrivate -> AbPrivate | EExtern -> AbExtern) flags in
	let flags = (match st with None -> flags | Some t -> AbOver t :: flags) in
	let def = {
		d_name = name;
		d_doc = fst annotations;
		d_params = tl;
		d_meta = snd annotations;
		d_flags = flags @ rl;
		d_data = l;
	} in
	(EAbstract def,p)

let emit_package path = match path with
	| None -> []
	| Some path -> path