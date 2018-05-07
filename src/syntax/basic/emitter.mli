type pos = Pos.Range.t

module type Sig = sig
	type 'a t_pos = 'a * pos
	type t_metadata_entry
	type t_annotations
	type t_path
	type t_pos_path
	type t_literal
	type t_const

	(* Expr *)

	type t_block_element
	type t_object_field_name
	type t_object_field
	type t_call_args
	type t_var_declaration
	type t_catch
	type t_case
	type t_expr

	(* Type hints *)

	type t_constraint
	type t_type_path_parameter_kind
	type t_type_path
	type t_complex_type

	(* Fields *)

	type t_function_argument
	type t_function
	type t_field_expr
	type t_modifier
	type t_class_field
	type t_enum_field_arg
	type t_enum_field
	type t_anonymous_field
	type t_anonymous_type_fields

	(* Type declaration *)

	type t_type_decl_parameter
	type t_import_mode
	type t_abstract_relation
	type t_class_relation
	type t_common_flag
	type t_class_flag
	type t_class
	type t_decl
	type t_package

	val emit_path : string -> string list -> t_path
	val emit_pos_path : string t_pos -> string t_pos list -> t_pos_path
	val emit_metadata_entry : Meta.strict_meta -> Pos.Range.t -> t_metadata_entry
	val emit_metadata_entry_with_args : Meta.strict_meta -> t_expr list -> Pos.Range.t -> t_metadata_entry
	val emit_function : string option -> t_type_decl_parameter list -> t_function_argument list -> t_complex_type option -> t_expr -> t_function
	val emit_lambda_arg : (t_expr * bool * t_metadata_entry list * t_complex_type option * t_expr option) * t_function_argument list -> t_function_argument list
	val emit_annotations : string option -> t_metadata_entry list -> t_annotations

	(* Expr *)

	val emit_literal_string : string -> t_literal
	val emit_literal_int : string -> t_literal
	val emit_literal_float : string -> t_literal
	val emit_literal_regex : (string * string) -> t_literal
	val emit_const_ident : string -> t_const
	val emit_const_literal : t_literal -> t_const

	val emit_call_args : t_expr list -> t_call_args
	val emit_assignment : t_expr -> t_expr
	val emit_var_declaration : string t_pos -> t_complex_type option -> t_expr option -> t_var_declaration

	val emit_catch : string t_pos -> t_complex_type -> t_expr -> pos -> t_catch
	val emit_case : t_expr list -> t_expr option -> t_block_element list -> pos -> t_case
	val emit_default : t_block_element list -> pos -> t_case
	val emit_object_field : t_object_field_name -> t_expr -> t_object_field
	val emit_object_field_name_ident : string -> pos -> t_object_field_name
	val emit_object_field_name_string : string -> pos -> t_object_field_name
	val emit_unknown : pos -> t_expr

	val emit_block_element_var : t_var_declaration list -> pos -> t_block_element
	val emit_block_element_inline_function : t_function -> pos -> t_block_element
	val emit_block_element_expr : t_expr -> pos -> t_block_element

	val emit_this : pos -> t_expr
	val emit_true : pos -> t_expr
	val emit_false : pos -> t_expr
	val emit_null : pos -> t_expr
	val emit_block_expr : t_block_element list -> pos -> t_expr
	val emit_var_declaration_expr : t_var_declaration -> pos -> t_expr
	val emit_metadata_expr : t_metadata_entry -> t_expr -> pos -> t_expr
	val emit_throw_expr : t_expr -> pos -> t_expr
	val emit_if_expr : t_expr -> t_expr -> t_expr option -> pos -> t_expr
	val emit_return_expr : t_expr option -> pos -> t_expr
	val emit_break_expr : pos -> t_expr
	val emit_continue_expr : pos -> t_expr
	val emit_do_expr : t_expr -> t_expr -> pos -> t_expr
	val emit_try_expr : t_expr -> t_catch list -> pos -> t_expr
	val emit_switch_expr : t_expr -> t_case list -> pos -> t_expr
	val emit_for_expr : t_expr -> t_expr -> pos -> t_expr
	val emit_while_expr : t_expr -> t_expr -> pos -> t_expr
	val emit_untyped_expr : t_expr -> pos -> t_expr
	val emit_object_decl_expr : t_object_field list -> pos -> t_expr
	val emit_unsafe_cast_expr : t_expr -> pos -> t_expr
	val emit_safe_cast_expr : t_expr -> t_complex_type -> pos -> t_expr
	val emit_new_expr : t_type_path -> t_call_args -> pos -> t_expr
	val emit_parenthesis_expr : t_expr -> pos -> t_expr
	val emit_typecheck_expr : t_expr -> t_complex_type -> pos -> t_expr
	val emit_is_expr : t_expr -> t_type_path -> pos -> pos -> t_expr
	val emit_array_decl_expr : t_expr list -> pos -> t_expr
	val emit_function_expr : t_function -> pos -> t_expr
	val emit_unary_prefix_expr : Ops.unop -> t_expr -> pos -> t_expr
	val emit_field_expr : t_expr -> string -> pos -> t_expr
	val emit_call_expr : t_expr -> t_call_args -> pos -> t_expr
	val emit_array_expr : t_expr -> t_expr -> pos -> t_expr
	val emit_binop_expr : t_expr -> Ops.binop -> t_expr -> pos -> t_expr
	val emit_unary_postfix_expr : t_expr -> Ops.unop -> pos -> t_expr
	val emit_ternary_expr : t_expr -> t_expr -> t_expr -> pos -> t_expr
	val emit_in_expr : t_expr -> t_expr -> pos -> t_expr
	val emit_dotint_expr : string -> pos -> t_expr
	val emit_dollarident_expr : string -> pos -> t_expr
	val emit_macro_escape_expr : string t_pos -> t_expr -> pos -> t_expr
	val emit_macro_expr : t_expr -> pos -> t_expr
	val emit_const_expr : t_const -> pos -> t_expr

	(* Type hints *)

	val emit_type_path : t_path -> t_type_path_parameter_kind list -> pos -> t_type_path

	val emit_complex_type_path : t_type_path -> pos -> t_complex_type
	val emit_complex_type_parent : t_complex_type -> pos -> t_complex_type
	val emit_complex_type_extension : t_type_path list -> t_anonymous_type_fields -> pos -> t_complex_type
	val emit_complex_type_anonymous : t_anonymous_type_fields -> pos -> t_complex_type
	val emit_complex_type_optional : t_complex_type -> pos -> t_complex_type
	val emit_complex_type_function : t_complex_type -> t_complex_type -> pos -> t_complex_type
	val emit_complex_type_named : string t_pos -> t_complex_type -> pos -> t_complex_type
	val emit_complex_type_named_function : t_complex_type list -> t_complex_type -> pos -> t_complex_type

	val emit_type_path_parameter_complex_type : t_complex_type -> t_type_path_parameter_kind
	val emit_type_path_parameter_bracket : t_expr list -> pos -> t_type_path_parameter_kind
	val emit_type_path_parameter_literal : t_literal -> pos -> t_type_path_parameter_kind

	(* Fields *)

	val emit_function_argument : t_annotations -> unit option -> string t_pos -> t_complex_type option -> t_expr option -> t_function_argument

	val emit_field_expr_none : t_field_expr
	val emit_field_expr_block : t_expr -> t_field_expr
	val emit_field_expr_expr : t_expr -> t_field_expr

	val emit_static_modifier : pos -> t_modifier
	val emit_macro_modifier : pos -> t_modifier
	val emit_public_modifier : pos -> t_modifier
	val emit_private_modifier : pos -> t_modifier
	val emit_override_modifier : pos -> t_modifier
	val emit_dynamic_modifier : pos -> t_modifier
	val emit_inline_modifier : pos -> t_modifier
	val emit_extern_modifier : pos -> t_modifier

	val emit_function_field : t_annotations -> t_modifier list -> string t_pos -> t_type_decl_parameter list -> t_function_argument list -> t_complex_type option -> t_field_expr -> pos -> t_class_field
	val emit_variable_field : t_annotations -> t_modifier list -> string t_pos -> t_complex_type option -> t_expr option -> pos -> t_class_field
	val emit_property_field : t_annotations -> t_modifier list -> string t_pos -> string t_pos -> string t_pos -> t_complex_type option -> t_expr option -> pos -> t_class_field

	val emit_enum_field_arg : unit option -> string -> t_complex_type -> t_enum_field_arg
	val emit_enum_field : t_annotations -> string t_pos -> t_type_decl_parameter list -> t_enum_field_arg list -> t_complex_type option -> pos -> t_enum_field

	val emit_anonymous_class_fields : t_class_field list -> t_anonymous_type_fields
	val emit_anonymous_type_fields : t_anonymous_field list -> t_anonymous_type_fields
	val emit_anonymous_type_field : unit option -> string t_pos -> t_complex_type -> pos -> t_anonymous_field

	(* Type declaration *)

	val emit_constraints_none : t_constraint
	val emit_constraints_single : t_complex_type -> t_constraint
	val emit_constraints_multiple : t_complex_type list -> t_constraint
	val emit_type_decl_parameter : t_annotations -> string t_pos -> t_constraint -> t_type_decl_parameter

	val emit_common_flag_extern : t_common_flag
	val emit_common_flag_private : t_common_flag
	val emit_class_flag_class : t_class_flag
	val emit_class_flag_interface : t_class_flag
	val emit_class_relation_extends : t_type_path -> t_class_relation
	val emit_class_relation_implements : t_type_path -> t_class_relation
	val emit_abstract_relation_from : t_complex_type -> t_abstract_relation
	val emit_abstract_relation_to : t_complex_type -> t_abstract_relation

	val emit_import_mode_alias : string -> t_import_mode
	val emit_import_mode_all : t_import_mode
	val emit_import_mode_normal : t_import_mode
	val emit_import : t_pos_path -> t_import_mode -> pos -> t_decl
	val emit_using : t_pos_path -> pos -> t_decl
	val emit_class : t_class_flag -> string t_pos option -> t_type_decl_parameter list -> t_class_relation list -> t_class_field list -> t_class
	val emit_class_decl : t_annotations -> t_common_flag list -> t_class -> pos -> t_decl
	val emit_enum_decl : t_annotations -> t_common_flag list -> string t_pos -> t_type_decl_parameter list -> t_enum_field list -> pos -> t_decl
	val emit_typedef_decl : t_annotations -> t_common_flag list -> string t_pos -> t_type_decl_parameter list -> t_complex_type -> pos -> t_decl
	val emit_abstract_decl : t_annotations -> t_common_flag list -> string t_pos -> t_type_decl_parameter list -> t_complex_type option -> t_abstract_relation list -> t_class_field list -> pos -> t_decl

	(* File *)

	val emit_package : t_path option -> t_package
end