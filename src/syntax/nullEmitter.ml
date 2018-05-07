type 'a t_pos = 'a * Pos.Range.t
type t_metadata_entry = unit
type t_annotations = unit
type t_path = unit
type t_pos_path = unit
type t_literal = unit
type t_const = unit

type t_block_element = unit
type t_object_field_name = unit
type t_object_field = unit
type t_call_args = unit
type t_var_declaration = unit
type t_catch = unit
type t_case = unit
type t_expr = unit

type t_constraint = unit
type t_type_path_parameter_kind = unit
type t_type_path = unit
type t_complex_type = unit

type t_function_argument = unit
type t_function = unit
type t_field_expr = unit
type t_modifier = unit
type t_function_name = unit
type t_property_ident = unit
type t_class_field = unit
type t_enum_field_arg = unit
type t_enum_field = unit
type t_anonymous_field = unit
type t_anonymous_type_fields = unit

type t_type_decl_parameter = unit
type t_import_mode = unit
type t_abstract_relation = unit
type t_class_relation = unit
type t_common_flag = unit
type t_class_flag = unit
type t_class = unit
type t_decl = unit
type t_package = unit

let emit_path ident idents = ()

let emit_pos_path ident idents = ()

let emit_metadata_entry meta p = ()

let emit_metadata_entry_with_args meta el p = ()

let emit_function name tl args cto e = ()

let emit_lambda_arg arg = []

let emit_annotations so metadata = ()

let emit_literal_string s = ()

let emit_literal_int s = ()

let emit_literal_float s = ()

let emit_literal_regex (s1,s2) = ()

let emit_const_ident s = ()

let emit_const_literal lit = ()

let emit_call_args el = ()

let emit_assignment e = ()

let emit_var_declaration name cto eo = ()

let emit_catch name ct e p = ()

let emit_case el1 eo el2 p = ()

let emit_default el p = ()

let emit_object_field name e = ()

let emit_object_field_name_ident s p = ()

let emit_object_field_name_string s p = ()

let emit_unknown p = ()

let emit_block_element_var vl p = ()

let emit_block_element_inline_function f p = ()

let emit_block_element_expr e p = ()

let emit_this p = ()

let emit_true p = ()

let emit_false p = ()

let emit_null p = ()

let emit_block_expr el p = ()

let emit_var_declaration_expr v p = ()

let emit_metadata_expr meta e p = ()

let emit_throw_expr e p = ()

let emit_if_expr e1 e2 eo p = ()

let emit_return_expr eo p = ()

let emit_break_expr p = ()

let emit_continue_expr p = ()

let emit_do_expr e1 e2 p = ()

let emit_try_expr e catches p = ()

let emit_switch_expr e cases p = ()

let emit_for_expr e1 e2 p = ()

let emit_while_expr e1 e2 p = ()

let emit_untyped_expr e p = ()

let emit_object_decl_expr fl p = ()

let emit_unsafe_cast_expr e p = ()

let emit_safe_cast_expr e ct p = ()

let emit_new_expr path el p = ()

let emit_parenthesis_expr e p = ()

let emit_typecheck_expr e ct p = ()

let emit_is_expr e path p_is p = ()

let emit_array_decl_expr el p = ()

let emit_function_expr f p = ()

let emit_unary_prefix_expr op e p = ()

let emit_field_expr e s p = ()

let emit_call_expr e el p = ()

let emit_array_expr e1 e2 p = ()

let emit_binop_expr e1 op e2 p = ()

let emit_unary_postfix_expr e op p = ()

let emit_ternary_expr e1 e2 e3 p = ()

let emit_in_expr e1 e2 p = ()

let emit_dotint_expr s p = ()

let emit_dollarident_expr name p = ()

let emit_macro_escape_expr name e p = ()

let emit_macro_expr e p = ()

let emit_const_expr const p = ()

(* Type hints *)

let emit_type_path (path : t_path) params p = ()

let emit_complex_type_path path p = ()
let emit_complex_type_parent ct p = ()
let emit_complex_type_extension paths fields p = ()
let emit_complex_type_anonymous fields p = ()
let emit_complex_type_optional ct p = ()

let emit_complex_type_function ct1 ct2 p = ()

let emit_complex_type_named n ct p = ()

let emit_complex_type_named_function ctl ct p = ()

let emit_type_path_parameter_complex_type ct = ()

let emit_type_path_parameter_bracket el p = ()

let emit_type_path_parameter_literal lit p = ()

(* Fields *)

let emit_function_argument annotations opt name cto eo = ()

let emit_field_expr_none = ()
let emit_field_expr_block e = ()
let emit_field_expr_expr e = ()

let emit_static_modifier _ = ()
let emit_macro_modifier _ = ()
let emit_public_modifier _ = ()
let emit_private_modifier _ = ()
let emit_override_modifier _ = ()
let emit_dynamic_modifier _ = ()
let emit_inline_modifier _ = ()
let emit_extern_modifier _ = ()

let emit_function_field annotations modifiers name tl args cto e p = ()

let emit_variable_field annotations modifiers name cto eo p = ()

let emit_property_field annotations modifiers name get set cto eo p = ()

let emit_enum_field_arg opt name ct = ()

let emit_enum_field annotations name tl args cto p = ()

let emit_anonymous_class_fields fields = ()
let emit_anonymous_type_fields fields = ()

let emit_anonymous_type_field opt name ct p = ()

(* Type declaration *)

let emit_constraints_none = ()

let emit_constraints_single ct = ()

let emit_constraints_multiple ctl = ()

let emit_type_decl_parameter annotations name constraints = ()

let emit_class_flag_interface = ()

let emit_class_flag_class = ()

let emit_common_flag_extern = ()

let emit_common_flag_private = ()

let emit_class_relation_extends path = ()

let emit_class_relation_implements path = ()

let emit_abstract_relation_from ct = ()

let emit_abstract_relation_to ct = ()

let emit_import_mode_all = ()

let emit_import_mode_alias name = ()

let emit_import_mode_normal = ()

let emit_import path mode p = ()

let emit_using path p = ()

let emit_class flags name tl rl l = ()

let emit_class_decl annotations flags c p = ()

let emit_enum_decl annotations flags name tl l p = ()

let emit_typedef_decl annotations flags name tl ct p = ()

let emit_abstract_decl annotations flags name tl st rl l p = ()

let emit_package path = ()