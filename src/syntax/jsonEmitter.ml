module EnumApi = struct
	type enum_api =
		| DoubleQuote
		| SingleQuote

		| NIdent
		| NString

		| PConstIdent
		| PConstLiteral

		| Var
		| InlineFunction
		| Expr

		| Case
		| Default

		| EVar
		| EMetadata
		| EMacro
		| EThrow
		| EIf
		| EReturn
		| EReturnExpr
		| EBreak
		| EContinue
		| EDo
		| ETry
		| ESwitch
		| EFor
		| EWhile
		| EUntyped
		| EObjectDecl
		| EConst
		| EUnsafeCast
		| ESafeCast
		| ENew
		| EParenthesis
		| ECheckType
		| EIs
		| EArrayDecl
		| EFunction
		| EUnaryPrefix
		| EField
		| ECall
		| EArrayAccess
		| EUnaryPostfix
		| EBinop
		| ETernary
		| EIn
		| EIntDot
		| EDollarIdent
		| EMacroEscape
		| EBlock

		| Simple
		| WithArgs

		| PPrivate
		| PExtern

		| PLiteralString
		| PLiteralInt
		| PLiteralFloat
		| PLiteralRegex

		| ArrayExpr
		| Type
		| Literal

		| Static
		| Macro
		| Public
		| Private
		| Override
		| Dynamic
		| Inline

		| MNone
		| MBlock
		| MExpr

		| Function
		| Variable
		| Property

		| ClassNotation
		| ShortNotation

		| Parenthesis
		| StructuralExtension
		| AnonymousStructure
		| Optional
		| TypePath
		| CTFunction

		| CMultiple
		| CSingle
		| CNone

		| Extends
		| Implements

		| To
		| From

		| IAs
		| IAll
		| INormal

		| ImportDecl
		| UsingDecl
		| ClassDecl
		| EnumDecl
		| TypedefDecl
		| AbstractDecl

	let info = function
		| DoubleQuote -> 0,"DoubleQuote"
		| SingleQuote -> 1,"SingleQuote"

		| NIdent -> 0,"NIdent"
		| NString -> 1,"NString"

		| PConstIdent -> 0,"PConstIdent"
		| PConstLiteral -> 1,"PConstLiteral"

		| Var -> 0,"Var"
		| InlineFunction -> 1,"InlineFunction"
		| Expr -> 2,"Expr"

		| Case -> 0,"Case"
		| Default -> 1,"Default"

		| EVar -> 0,"EVar"
		| EMetadata -> 1,"EMetadata"
		| EMacro -> 2,"EMacro"
		| EThrow -> 3,"EThrow"
		| EIf -> 4,"EIf"
		| EReturn -> 5,"EReturn"
		| EReturnExpr -> 6,"EReturnExpr"
		| EBreak -> 7,"EBreak"
		| EContinue -> 8,"EContinue"
		| EDo -> 9,"EDo"
		| ETry -> 10,"ETry"
		| ESwitch -> 11,"ESwitch"
		| EFor -> 12,"EFor"
		| EWhile -> 13,"EWhile"
		| EUntyped -> 14,"EUntyped"
		| EObjectDecl -> 15,"EObjectDecl"
		| EConst -> 16,"EConst"
		| EUnsafeCast -> 17,"EUnsafeCast"
		| ESafeCast -> 18,"ESafeCast"
		| ENew -> 19,"ENew"
		| EParenthesis -> 20,"EParenthesis"
		| ECheckType -> 21,"ECheckType"
		| EIs -> 22,"EIs"
		| EArrayDecl -> 23,"EArrayDecl"
		| EFunction -> 24,"EFunction"
		| EUnaryPrefix -> 25,"EUnaryPrefix"
		| EField -> 26,"EField"
		| ECall -> 27,"ECall"
		| EArrayAccess -> 28,"EArrayAccess"
		| EUnaryPostfix -> 29,"EUnaryPostfix"
		| EBinop -> 30,"EBinop"
		| ETernary -> 31,"ETernary"
		| EIn -> 32,"EIn"
		| EIntDot -> 33,"EIntDot"
		| EDollarIdent -> 34,"EDollarIdent"
		| EMacroEscape -> 35,"EMacroEscape"
		| EBlock -> 36,"EBlock"

		| Simple -> 0,"Simple"
		| WithArgs -> 1,"WithArgs"

		| PPrivate -> 0,"PPrivate"
		| PExtern -> 1,"PExtern"

		| PLiteralString -> 0,"PLiteralString"
		| PLiteralInt -> 1,"PLiteralInt"
		| PLiteralFloat -> 2,"PLiteralFloat"
		| PLiteralRegex -> 3,"PLiteralRegex"

		| ArrayExpr -> 0,"ArrayExpr"
		| Type -> 1,"Type"
		| Literal -> 2,"Literal"

		| Static -> 0,"Static"
		| Macro -> 1,"Macro"
		| Public -> 2,"Public"
		| Private -> 3,"Private2"
		| Override -> 4,"Override"
		| Dynamic -> 5,"Dynamic"
		| Inline -> 6,"Inline"

		| MNone -> 0,"None"
		| MBlock -> 1,"Block"
		| MExpr -> 2,"Expr"

		| Function -> 0,"Function"
		| Variable -> 1,"Variable"
		| Property -> 2,"Property"

		| ClassNotation -> 0,"ClassNotation"
		| ShortNotation -> 1,"ShortNotation"

		| Parenthesis -> 0,"Parenthesis"
		| StructuralExtension -> 1,"StructuralExtension"
		| AnonymousStructure -> 2,"AnonymousStructure"
		| Optional -> 3,"Optional"
		| TypePath -> 4,"TypePath"
		| CTFunction -> 5,"Function"

		| CMultiple -> 0,"Multiple"
		| CSingle -> 1,"Single"
		| CNone -> 2,"None"

		| Extends -> 0,"Extends"
		| Implements -> 1,"Implements"

		| To -> 0,"To"
		| From -> 1,"From"

		| IAs -> 0,"IAs"
		| IAll -> 1,"IAll"
		| INormal -> 2,"INormal"

		| ImportDecl -> 0,"ImportDecl"
		| UsingDecl -> 1,"UsingDecl"
		| ClassDecl -> 2,"ClassDecl"
		| EnumDecl -> 3,"EnumDecl"
		| TypedefDecl -> 4,"TypedefDecl"
		| AbstractDecl -> 5,"AbstractDecl"
end

module JsonEmitter(Api : JsonApi.JsonApi) = struct
	open EnumApi

	type 'a t_pos = 'a * Pos.Range.t
	type t_metadata_entry = Api.t
	type t_annotations = Api.t
	type t_path = Api.t
	type t_pos_path = Api.t
	type t_string_literal = Api.t
	type t_literal = Api.t
	type t_const = Api.t

	type t_block_element = Api.t
	type t_object_field_name = Api.t
	type t_object_field = Api.t
	type t_call_args = Api.t
	type t_var_declaration = Api.t
	type t_catch = Api.t
	type t_case = Api.t
	type t_expr = Api.t

	type t_constraint = Api.t
	type t_type_path_parameter_kind = Api.t
	type t_type_path = Api.t
	type t_complex_type = Api.t

	type t_function_argument = Api.t
	type t_function = Api.t
	type t_field_expr = Api.t
	type t_modifier = Api.t
	type t_function_name = Api.t
	type t_property_ident = Api.t
	type t_class_field = Api.t
	type t_enum_field_arg = Api.t
	type t_enum_field = Api.t
	type t_anonymous_field = Api.t
	type t_anonymous_type_fields = Api.t

	type t_type_decl_parameter = Api.t
	type t_import_mode = Api.t
	type t_abstract_relation = Api.t
	type t_class_relation = Api.t
	type t_common_flag = Api.t
	type t_class_flag = Api.t
	type t_class = Api.t
	type t_decl = Api.t

	type t_package = Api.t
	type t_file = Api.t
	type t_decls_only = Api.t
	type t_class_fields_only = Api.t
	type t_block_elements_only = Api.t

	let enum enum_name constructor arguments =
		let index,name = EnumApi.info constructor in
		Api.jarray ((Api.jint (index)) :: (Api.jstring name) :: arguments)

	let opt = function
		| None -> Api.jnull
		| Some t -> t

	let tokn = Api.jint 0 (* We just need something that's not null *)
	let tok = Api.jnull
	let arr = Api.jarray

	let arrsep b l = match l with
		| hd :: tl -> Api.jobject [
			"arg",hd;
			"args",Api.jarray (List.map (fun j -> Api.jobject ["comma",tok;"arg",j]) tl);
			"comma",if b then tokn else tok;
		]
		| [] -> Api.jnull

	let str = Api.jstring

	let arropt = function
		| [] -> Api.jnull
		| l -> arr l

	let type_parameters l = match l with
		| [] -> Api.jnull
		| _ -> Api.jobject ["params",arrsep false l]

	let emit_path ident idents =
		Api.jobject [
			"ident",str ident;
			"idents",arr (List.map str idents);
		]

	let emit_pos_path ident idents =
		emit_path (fst ident) (List.map fst idents)

	let emit_metadata_entry meta p =
		enum "Metadata" Simple [tok]

	let emit_metadata_entry_with_args meta el p =
		enum "Metadata" WithArgs [tok;arrsep false el;tok]

	let emit_function name tl args cto e =
		Api.jobject [
			"params",type_parameters tl;
			"args",arrsep false args;
			"typeHint",opt cto;
			"expr",e
		]

	let emit_annotations so metadata =
		Api.jobject [
			"doc",(match so with None -> Api.jnull | Some s -> str s);
			"metadata",arr metadata;
		]

	let emit_literal_string_double_quoted s =
		enum "StringToken" DoubleQuote [str s]

	let emit_literal_string_single_quoted s =
		enum "StringToken" SingleQuote [str s]

	let emit_literal_string s =
		enum "Literal" PLiteralString [s]

	let emit_literal_int s =
		enum "Literal" PLiteralInt [str s]

	let emit_literal_float s =
		enum "Literal" PLiteralFloat [str s]

	let emit_literal_regex (s1,s2) =
		enum "Literal" PLiteralRegex [str s1;str s2]

	let emit_const_ident s =
		enum "NConst" PConstIdent [str s]

	let emit_const_literal lit =
		enum "NConst" PConstLiteral [lit]

	let emit_call_args el =
		Api.jobject [
			"args",arrsep false el;
		]

	let emit_assignment e =
		Api.jobject [
			"expr",e
		]

	let emit_var_declaration name cto eo =
		Api.jobject [
			"name",str (fst name);
			"typeHint",opt cto;
			"assignment",opt eo
		]

	let emit_catch name ct e p =
		Api.jobject [
			"typeHint",ct;
			"expr",e;
		]

	let emit_case el1 eo el2 p =
		enum "Case" Case [tok;arrsep false el1;opt eo;tok;arr el2]

	let emit_default el p =
		enum "Case" Default [tok;tok;arr el]

	let emit_object_field name e =
		Api.jobject [
			"name",name;
			"expr",e
		]

	let emit_object_field_name_ident s p =
		enum "ObjectFieldName" NIdent [str s]

	let emit_object_field_name_string s p =
		enum "ObjectFieldName" NString [s]

	let emit_unknown p = Api.jnull

	let emit_block_element_var vl p =
		enum "BlockElement" Var [tok;arrsep false vl;tok]

	let emit_block_element_inline_function f p =
		enum "BlockElement" InlineFunction [tok;tok;f;tok]

	let emit_block_element_expr e p =
		enum "BlockElement" Expr [e;tok]

	let emit_this p =
		enum "Expr" EConst [emit_const_ident "this"]

	let emit_true p =
		enum "Expr" EConst [emit_const_ident "true"]

	let emit_false p =
		enum "Expr" EConst [emit_const_ident "false"]

	let emit_null p =
		enum "Expr" EConst [emit_const_ident "null"]

	let emit_block_expr el p =
		enum "Expr" EBlock [tok;arr el;tok]

	let emit_var_declaration_expr v p =
		enum "Expr" EVar [tok;v]

	let emit_metadata_expr meta e p =
		enum "Expr" EMetadata [meta;e]

	let emit_throw_expr e p =
		enum "Expr" EThrow [tok;e;]

	let emit_if_expr e1 e2 eo p =
		enum "Expr" EIf [tok;tok;e1;tok;e2;opt eo]

	let emit_return_expr eo p = match eo with
		| None -> enum "Expr" EReturn [tok]
		| Some e -> enum "Expr" EReturnExpr [tok;e]

	let emit_break_expr p =
		enum "Expr" EBreak [tok]

	let emit_continue_expr p =
		enum "Expr" EContinue [tok]

	let emit_do_expr e1 e2 p =
		enum "Expr" EDo [tok;e1;tok;tok;e2;tok]

	let emit_try_expr e catches p =
		enum "Expr" ETry [tok;e;arr catches]

	let emit_switch_expr e cases p =
		enum "Expr" ESwitch [tok;e;tok;arr cases;tok]

	let emit_for_expr e1 e2 p =
		enum "Expr" EFor [tok;tok;e1;tok;e2]

	let emit_while_expr e1 e2 p =
		enum "Expr" EWhile [tok;tok;e1;tok;e2]

	let emit_untyped_expr e p =
		enum "Expr" EUntyped [tok;e]

	let emit_object_decl_expr fl b p =
		enum "Expr" EObjectDecl [tok;arrsep b fl;tok]

	let emit_unsafe_cast_expr e p =
		enum "Expr" EUnsafeCast [tok;e]

	let emit_safe_cast_expr e ct p =
		enum "Expr" ESafeCast [tok;tok;e;tok;ct;tok]

	let emit_new_expr path el p =
		enum "Expr" ENew [tok;path;el]

	let emit_parenthesis_expr e p =
		enum "Expr" EParenthesis [tok;e;tok]

	let emit_typecheck_expr e ct p =
		enum "Expr" ECheckType [tok;e;tok;ct;tok]

	let emit_is_expr e path p_is p =
		enum "Expr" EIs [tok;e;tok;path;tok]

	let emit_array_decl_expr el b p =
		enum "Expr" EArrayDecl [tok;arrsep b el;tok]

	let emit_function_expr f p =
		enum "Expr" EFunction [tok;f]

	let emit_unary_prefix_expr op e p =
		enum "Expr" EUnaryPrefix [tok;e]

	let emit_field_expr e s p =
		enum "Expr" EField [e;str s]

	let emit_call_expr e el p =
		enum "Expr" ECall [e;el]

	let emit_array_expr e1 e2 p =
		enum "Expr" EArrayAccess [e1;tok;e2;tok]

	let emit_binop_expr e1 op e2 p =
		enum "Expr" EBinop [e1;tok;e2]

	let emit_unary_postfix_expr e op p =
		enum "Expr" EUnaryPostfix [e;tok]

	let emit_ternary_expr e1 e2 e3 p =
		enum "Expr" ETernary [e1;tok;e2;tok;e3]

	let emit_in_expr e1 e2 p =
		enum "Expr" EIn [e1;tok;e2]

	let emit_dotint_expr s p =
		enum "Expr" EIntDot [str s;tok]

	let emit_dollarident_expr name p =
		enum "Expr" EDollarIdent [str name]

	let emit_macro_escape_expr name e p =
		enum "Expr" EMacroEscape [str (fst name);tok;e;tok]

	let emit_macro_expr e p =
		enum "Expr" EMacro [e]

	let emit_const_expr const p =
		enum "Expr" EConst [const]

	(* Type hints *)

	let emit_type_path path params p =
		Api.jobject [
			"path",path;
			"params",type_parameters params;
		]

	let emit_complex_type_path path p =
		enum "ComplexType" TypePath [path]

	let emit_complex_type_parent ct p =
		enum "ComplexType" Parenthesis [tok;ct;tok]

	let emit_complex_type_extension paths fields p =
		enum "ComplexType" StructuralExtension [tok;arr paths;fields;tok]

	let emit_complex_type_anonymous fields p =
		enum "ComplexType" AnonymousStructure [tok;fields;tok]

	let emit_complex_type_optional ct p =
		enum "ComplexType" Optional [tok;ct]

	let emit_complex_type_function ct1 ct2 p =
		enum "ComplexType" Function [ct1;tok;ct2] (* TODO: Compose these here? *)

	let emit_type_path_parameter_complex_type ct =
		enum "TypePathParameter" Type [ct]

	let emit_type_path_parameter_bracket el b p =
		enum "TypePathParameter" ArrayExpr [tok;arrsep b el;tok]

	let emit_type_path_parameter_literal lit p =
		enum "TypePathParameter" Literal [lit]

	(* Fields *)

	let emit_function_argument annotations optq name cto eo =
		Api.jobject [
			"annotations",annotations;
			"questionMark",(match optq with None -> Api.jnull | Some _ -> tokn); (* TODO: This is stupid *)
			"name",str (fst name);
			"typeHint",opt cto;
			"assignment",opt eo
		]

	let emit_field_expr_none =
		enum "MethodExpr" MNone [tok]

	let emit_field_expr_block e =
		enum "MethodExpr" MBlock [e]

	let emit_field_expr_expr e =
		enum "MethodExpr" MExpr [e;tok]

	let emit_static_modifier =
		enum "FieldModifier" Static [tok]

	let emit_macro_modifier =
		enum "FieldModifier" Macro [tok]

	let emit_public_modifier =
		enum "FieldModifier" Public [tok]

	let emit_private_modifier =
		enum "FieldModifier" Private [tok]

	let emit_override_modifier =
		enum "FieldModifier" Override [tok]

	let emit_dynamic_modifier =
		enum "FieldModifier" Dynamic [tok]

	let emit_inline_modifier =
		enum "FieldModifier" Inline [tok]

	let emit_function_field annotations modifiers name tl args cto e p =
		enum "ClassField" Function [annotations;arr modifiers;str (fst name);arropt tl;arr args;opt cto;e]

	let emit_variable_field annotations modifiers name cto eo p =
		enum "ClassField" Variable [annotations;arr modifiers;str (fst name);opt cto;opt eo]

	let emit_property_field annotations modifiers name get set cto eo p =
		enum "ClassField" Property [annotations;arr modifiers;tok;str (fst name);tok;str (fst name);tok;str (fst name);tok;opt cto;opt eo]

	let emit_enum_field_arg optq name ct =
		Api.jobject [
			"questionMark",(match optq with None -> Api.jnull | Some _ -> tokn); (* TODO: This is stupid *)
			"name",str name;
			"typeHint",ct
		]

	let emit_enum_field annotations name tl args cto p =
		Api.jobject [
			"annotations",annotations;
			"name",str (fst name);
			"params",type_parameters tl;
			"args",arrsep false args;
			"typeHint",opt cto;
		]

	let emit_anonymous_class_fields fields =
		enum "AnonymousStructureFields" ClassNotation [arr fields]

	let emit_anonymous_type_fields fields b =
		enum "AnonymousStructureFields" ShortNotation [arrsep b fields]

	let emit_anonymous_type_field optq name ct p =
		Api.jobject [
			"questionMark",(match optq with None -> Api.jnull | Some _ -> Api.jint 0); (* TODO: This is stupid *)
			"name",str (fst name);
			"typeHint",ct
		]

	(* Type declaration *)

	let emit_constraints_none =
		enum "Constraints" CNone []

	let emit_constraints_single ct =
		enum "Constraints" CSingle [tok;ct]

	let emit_constraints_multiple ctl =
		enum "Constraints" CMultiple [tok;tok;arrsep false ctl;tok]

	let emit_type_decl_parameter annotations name constraints =
		Api.jobject [
			"annotations",annotations;
			"name",str (fst name);
			"constraints",constraints
		]

	(* TODO: Figure these out *)

	let emit_class_flag_interface = Api.jnull

	let emit_class_flag_class = Api.jnull

	let emit_common_flag_extern =
		enum "NCommonFlag" PExtern [tok]

	let emit_common_flag_private =
		enum "NCommonFlag" PPrivate [tok]

	let emit_class_relation_extends path =
		enum "ClassRelation" Extends [tok;path]

	let emit_class_relation_implements path =
		enum "ClassRelation" Implements [tok;path]

	let emit_abstract_relation_from ct =
		enum "AbstractRelation" From [tok;ct]

	let emit_abstract_relation_to ct =
		enum "AbstractRelation" To [tok;ct]

	let emit_import_mode_all =
		enum "ImportMode" IAll [tok]

	let emit_import_mode_alias name =
		enum "ImportMode" IAs [tok;str name]

	let emit_import_mode_normal =
		enum "ImportMode" INormal []

	let emit_import path mode p =
		enum "Decl" ImportDecl [Api.jobject [
			"path",path;
			"mode",mode;
		]]

	let emit_using path p =
		enum "Decl" UsingDecl [Api.jobject ["path",path]]

	let emit_class flags name tl rl l =
		Api.jobject [
			"params",type_parameters tl;
			"relations",arr rl;
			"fields",arr l;
		]

	let emit_class_decl annotations flags c p =
		enum "Decl" ClassDecl [Api.jobject [
			"annotations",annotations;
			"flags",arr flags;
			"decl",c
		]]

	let emit_enum_decl annotations flags name tl l p =
		enum "Decl" EnumDecl [Api.jobject [
			"annotations",annotations;
			"flags",arr flags;
			"name",str (fst name);
			"params",type_parameters tl;
			"fields",arr l
		]]

	let emit_typedef_decl annotations flags name tl ct p =
		enum "Decl" TypedefDecl [Api.jobject [
			"annotations",annotations;
			"flags",arr flags;
			"name",str (fst name);
			"params",type_parameters tl;
			"type",ct
		]]

	let emit_abstract_decl annotations flags name tl st rl l p =
		enum "Decl" AbstractDecl [Api.jobject [
			"annotations",annotations;
			"flags",arr flags;
			"name",str (fst name);
			"params",type_parameters tl;
			"underlyingType",opt st;
			"relations",arr rl;
			"fields",arr l
		]]

	let emit_package path =
		Api.jobject [
			"path",opt path
		]

	let emit_file pack decls =
		Api.jobject [
			"pack",opt pack;
			"decls",arr decls;
		]

	let emit_decls_only decls = arr decls

	let emit_class_fields_only cffl = arr cffl

	let emit_block_elements_only el = arr el
end