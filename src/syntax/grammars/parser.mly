(* Start and types *)

%start <Emitter.t_file> file
%start <Emitter.t_expr> expr_only
%start <Emitter.t_class_fields_only> class_fields_only
%start <Emitter.t_decls_only> decls_only
%start <Emitter.t_block_elements_only> block_elements_only
%start <unit> unused

%type <Emitter.t_path> path

%on_error_reduce expr_open expr_closed expr
%on_error_reduce path complex_type

%parameter<Emitter : Emitter.Sig >

%{
	open Emitter
%}

%%

(* Various *)

lplist(X):
	| %prec LOWEST { [] }
	| x = X; xs = lplist(X) { x :: xs }

lpoption(X):
	| %prec LOWEST { None }
	| x = X { Some x }

metadata:
	| name = METADATA { emit_metadata_entry (Meta.Custom name) (mk $startpos $endpos) }
	| name = METADATA_OPEN; el = separated_list(COMMA, expr); PCLOSE { emit_metadata_entry_with_args (Meta.Custom name) el (mk $startpos $endpos) }

documentation:
	| comment = COMMENT { Some comment }
	| { None }

annotations:
	| doc = documentation; meta = metadata* { emit_annotations doc meta }

%inline pos(f):
	| x = f { x,mk $startpos $endpos }

dollar_ident:
	| s = ident | s = DOLLAR_IDENT { s }

dot_ident:
	| s = DOT_IDENT { s }
	| DOT { "." }

path:
	| ident = dollar_ident; l = lplist(dot_ident) { emit_path ident l }

path_with_pos:
	| ident = pos(dollar_ident); l = lplist(pos(dot_ident)) { emit_pos_path ident l }

(* Expression *)

call_args:
	| POPEN; el = separated_list(COMMA, expr); PCLOSE { emit_call_args el }

assignment:
	| ASSIGN; e1 = expr { emit_assignment e1 }

var_declaration:
	| name = pos(dollar_ident); ct = lpoption(type_hint); eo = lpoption(assignment) {
		emit_var_declaration name ct eo
	}

var_declarations_next:
	| %prec LOWEST { [] }
	| COMMA; vl = var_declarations { vl }

var_declarations:
	| v = var_declaration; vl = var_declarations_next { v :: vl }

%inline else_expr:
	| ELSE; e1 = expr { e1 }

%inline catch:
	| CATCH; POPEN; name = pos(dollar_ident); ct = type_hint; PCLOSE; e1 = expr {
		emit_catch name ct e1 (mk $startpos $endpos)
	}

%inline guard:
	| IF; POPEN; e1 = expr; PCLOSE { e1 }

case:
	| CASE; el = separated_nonempty_list(COMMA,expr); eg = guard?; COLON; el2 = block_element* {
		emit_case el eg el2 (mk $startpos $endpos)
	}
	| DEFAULT; COLON; el = block_element* {
		emit_default el (mk $startpos $endpos)
	}

func:
	| name = dollar_ident?; tl = type_decl_parameters; POPEN; el = separated_list(COMMA,function_argument); PCLOSE; ct = type_hint?; e1 = expr {
		emit_function name tl el ct e1
	}

array_elements_next:
	| { [],false }
	| COMMA { [],true }
	| COMMA; e1 = expr; el = array_elements_next { e1 :: (fst el),snd el }

array_elements:
	| { [],false }
	| e1 = expr; el = array_elements_next { e1 :: (fst el),snd el }

object_field_name:
	| name = dollar_ident { emit_object_field_name_ident name (mk $startpos $endpos) }
	| name = string { emit_object_field_name_string name (mk $startpos $endpos) }

object_field:
	| name = object_field_name; COLON; e = expr {
		emit_object_field name e
	}

object_fields_next:
	| { [],false }
	| COMMA { [],true }
	| COMMA; f = object_field; fl = object_fields_next { f :: (fst fl),snd fl }

%inline object_fields:
	| f = object_field; fl = object_fields_next { f :: (fst fl),snd fl }

macro_expr:
	| type_hint { emit_unknown (mk $startpos $endpos) }
	| VAR; var_declarations { emit_unknown (mk $startpos $endpos) }
	| class_decl2 { emit_unknown (mk $startpos $endpos) }
	| expr_open %prec MACRO { emit_unknown (mk $startpos $endpos) }
	| expr_closed { emit_unknown (mk $startpos $endpos) }

block_element:
	| VAR; vl = var_declarations; SEMICOLON {
		emit_block_element_var vl (mk $startpos $endpos)
	}
	| INLINE; FUNCTION; f = func; SEMICOLON {
		emit_block_element_inline_function f (mk $startpos $endpos)
	}
	| e = expr_open; SEMICOLON { emit_block_element_expr e (mk $startpos $endpos) }
	| e = expr_closed; SEMICOLON { emit_block_element_expr e (mk $startpos $endpos) }

field_expr:
	| SEMICOLON { emit_field_expr_none }
	| e = expr_block { emit_field_expr_block e }
	| e = expr; SEMICOLON { emit_field_expr_expr e }

expr_block:
	| BROPEN; BRCLOSE { emit_block_expr [] (mk $startpos $endpos) }
	| BROPEN; el = nonempty_list(block_element); BRCLOSE { emit_block_expr el (mk $startpos $endpos) }

expr_var:
	| VAR; v = var_declaration { emit_var_declaration_expr v (mk $startpos $endpos) }

expr_binop:
	| e1 = expr_open; op = op; e2 = expr_inline { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = expr_open; op = op_bit; e2 = expr_inline %prec AND { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = expr_open; op = op_compare; e2 = expr_inline %prec EQUALS { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = expr_open; op = op_assign; e2 = expr_inline %prec ASSIGN { emit_binop_expr e1 op e2 (mk $startpos $endpos) }

expr_closed:
	| m = metadata; e1 = expr { emit_metadata_expr m e1 (mk $startpos $endpos) }
	| MACRO; e = macro_expr { emit_macro_expr e (mk $startpos $endpos) }
	| expr_block { $1 }
	| THROW; e1 = expr { emit_throw_expr e1 (mk $startpos $endpos) }
	| IF; POPEN; e1 = expr; PCLOSE; e2 = expr; eo = lpoption(else_expr) { emit_if_expr e1 e2 eo (mk $startpos $endpos) }
	| RETURN { emit_return_expr None (mk $startpos $endpos) }
	| RETURN; e = expr { emit_return_expr (Some e) (mk $startpos $endpos) }
	| BREAK { emit_break_expr (mk $startpos $endpos) }
	| CONTINUE { emit_continue_expr (mk $startpos $endpos) }
	| DO; e1 = expr; WHILE; POPEN; e2 = expr; PCLOSE { emit_do_expr e1 e2 (mk $startpos $endpos) }
	| TRY; e1 = expr; catches = lplist(catch); { emit_try_expr e1 catches (mk $startpos $endpos) }
	| SWITCH; e1 = expr; BROPEN; cases = case*; BRCLOSE { emit_switch_expr e1 cases (mk $startpos $endpos) }
	| FOR; POPEN; e1 = expr; PCLOSE; e2 = expr { emit_for_expr e1 e2 (mk $startpos $endpos) }
	| WHILE; POPEN; e1 = expr; PCLOSE; e2 = expr { emit_while_expr e1 e2 (mk $startpos $endpos) }
	| UNTYPED; e1 = expr { emit_untyped_expr e1 (mk $startpos $endpos) }

expr_open:
	| BROPEN; fl = object_fields; BRCLOSE { emit_object_decl_expr (fst fl) (snd fl) (mk $startpos $endpos) }
	| CAST; e1 = expr { emit_unsafe_cast_expr e1 (mk $startpos $endpos) }
	| CAST; POPEN; e1 = expr; COMMA; ct = complex_type; PCLOSE { emit_safe_cast_expr e1 ct (mk $startpos $endpos) }
	| NEW; tp = type_path; el = call_args { emit_new_expr tp el (mk $startpos $endpos) }
	| POPEN; e1 = expr; PCLOSE { emit_parenthesis_expr e1 (mk $startpos $endpos) }
	| POPEN; e1 = expr; COLON; ct = complex_type; PCLOSE { emit_typecheck_expr e1 ct (mk $startpos $endpos) }
	| POPEN; e1 = expr; is = pos(IS); tp = type_path; PCLOSE { emit_is_expr e1 tp (snd is) (mk $startpos $endpos) }
	| BKOPEN; el = array_elements; BKCLOSE { emit_array_decl_expr (fst el) (snd el) (mk $startpos $endpos) }
	| FUNCTION; f = func { emit_function_expr f (mk $startpos $endpos) }
	| op = unary_prefix; e1 = expr_inline %prec INCREMENT { emit_unary_prefix_expr op e1 (mk $startpos $endpos) }
	| e1 = expr_open; name = dot_ident { emit_field_expr e1 name (mk $startpos $endpos) }
	| e1 = expr_open; el = call_args { emit_call_expr e1 el (mk $startpos $endpos) }
	| e1 = expr_open; BKOPEN; e2 = expr; BKCLOSE { emit_array_expr e1 e2 (mk $startpos $endpos) }
	| expr_binop { $1 }
	| e1 = expr_open; op = unary_postfix { emit_unary_postfix_expr e1 op (mk $startpos $endpos) }
	| e1 = expr_open; QUESTIONMARK; e2 = expr; COLON; e3 = expr {
		emit_ternary_expr e1 e2 e3 (mk $startpos $endpos)
	}
	| e1 = expr_open; IN; e2 = expr { emit_in_expr e1 e2 (mk $startpos $endpos) }
	| s = INT; DOT { emit_dotint_expr s (mk $startpos $endpos) }
	| s = DOLLAR_IDENT %prec LOWEST { emit_dollarident_expr s (mk $startpos $endpos) }
	| s = pos(DOLLAR_IDENT); BROPEN; e1 = expr; BRCLOSE { emit_macro_escape_expr s e1 (mk $startpos $endpos) }
	| const = const { emit_const_expr const (mk $startpos $endpos) }
	| e1 = keyword_ident { e1 }

%inline expr_inline:
	| e = expr_closed | e = expr_open | e = expr_var { e }

expr:
	| e = expr_closed | e = expr_open %prec LOWEST | e = expr_var { e }

(* Type hints *)

%inline structural_extension:
	| GT; tp = type_path; COMMA { tp }

anonymous_type_field:
	| opt = QUESTIONMARK?; name = pos(dollar_ident); ct = type_hint {
		emit_anonymous_type_field opt name ct (mk $startpos $endpos)
	}

anonymous_type_fields_short_next:
	| { [],false }
	| COMMA { [],true }
	| COMMA; f = anonymous_type_field; fl = anonymous_type_fields_short_next { f :: (fst fl),snd fl }

anonymous_type_fields_short:
	| { [],false }
	| f = anonymous_type_field; fl = anonymous_type_fields_short_next { f :: (fst fl),snd fl }

anonymous_type_fields:
	| l = class_field+ { emit_anonymous_class_fields l }
	| l = anonymous_type_fields_short { emit_anonymous_type_fields (fst l) (snd l) }

complex_type:
	| POPEN; ct = complex_type; PCLOSE { emit_complex_type_parent ct (mk $startpos $endpos) }
	| BROPEN; l = structural_extension+; cffl = anonymous_type_fields; BRCLOSE {
		emit_complex_type_extension l cffl (mk $startpos $endpos)
	}
	| BROPEN; l = anonymous_type_fields; BRCLOSE { emit_complex_type_anonymous l (mk $startpos $endpos) }
	| QUESTIONMARK; ct = complex_type; { emit_complex_type_optional ct (mk $startpos $endpos) }
	| tp = type_path { emit_complex_type_path tp (mk $startpos $endpos) }
	| ct1 = complex_type; ARROW; ct2 = complex_type {
		emit_complex_type_function ct1 ct2 (mk $startpos $endpos)
	}

type_path_parameter:
	| BKOPEN; el = array_elements; BKCLOSE { emit_type_path_parameter_bracket (fst el) (snd el) (mk $startpos $endpos) }
	| ct = complex_type { emit_type_path_parameter_complex_type ct }
	| cst = literal { emit_type_path_parameter_literal cst (mk $startpos $endpos) }
	/*| e = expr { TPExpr e }*/

type_path_parameters:
	| LT; tl = separated_nonempty_list(COMMA,type_path_parameter); GT { tl }
	| %prec LOWEST { [] }

type_path:
	| path = path; l = type_path_parameters {
		emit_type_path path l (mk $startpos $endpos)
	}

type_hint:
	| COLON; ct = complex_type { ct }

(* Field *)

%inline function_argument:
	| annotations = annotations; opt = QUESTIONMARK?; name = pos(dollar_ident); ct = type_hint?; eo = assignment? {
		emit_function_argument annotations opt name ct eo
	}

%inline function_arguments:
	| l = separated_list(COMMA,function_argument) { l }

modifier:
	| STATIC { emit_static_modifier }
	| MACRO { emit_macro_modifier }
	| PUBLIC { emit_public_modifier }
	| PRIVATE { emit_private_modifier }
	| OVERRIDE { emit_override_modifier }
	| DYNAMIC { emit_dynamic_modifier }
	| INLINE { emit_inline_modifier }

function_name:
	| s = pos(dollar_ident) { s }
	| s = pos(NEW) { "new",snd s }

property_ident:
	| s = pos(ident) { s }
	| s = pos(DYNAMIC) { "dynamic",snd s }
	| s = pos(DEFAULT) { "default",snd s }
	| s = pos(NULL) { "null",snd s }

class_field:
	| annotations = annotations; ml = modifier*; FUNCTION; name = function_name; tl = type_decl_parameters; POPEN; args = function_arguments; PCLOSE; ct = type_hint? eo = field_expr {
		emit_function_field annotations ml name tl args ct eo (mk $startpos $endpos)
	}
	| annotations = annotations; ml = modifier*; VAR; name = pos(dollar_ident); ct = type_hint?; eo = assignment?; SEMICOLON {
		emit_variable_field annotations ml name ct eo (mk $startpos $endpos)
	}
	| annotations = annotations; ml = modifier*; VAR; name = pos(dollar_ident); POPEN; get = property_ident; COMMA; set = property_ident; PCLOSE; ct = type_hint?; eo = assignment?; SEMICOLON {
		emit_property_field annotations ml name get set ct eo (mk $startpos $endpos)
	}

%inline enum_field_arg:
	| opt = QUESTIONMARK?; name = dollar_ident; ct = type_hint; {
		emit_enum_field_arg opt name ct
	}

enum_field_args:
	| { [] }
	| POPEN; l = separated_list(COMMA,enum_field_arg); PCLOSE { l }

%inline enum_field:
	| annotations = annotations; name = pos(dollar_ident); tl = type_decl_parameters; args = enum_field_args; ct = type_hint?; SEMICOLON {
		emit_enum_field annotations name tl args ct (mk $startpos $endpos)
	}

(* Type declaration *)

class_relations:
	| EXTENDS; path = type_path { emit_class_relation_extends path }
	| IMPLEMENTS; path = type_path { emit_class_relation_implements path }

abstract_relations:
	| TO; ct = complex_type { emit_abstract_relation_to ct }
	| FROM; ct = complex_type { emit_abstract_relation_from ct }

%inline underlying_type:
	| POPEN; ct = complex_type; PCLOSE { ct }

common_flags:
	| PRIVATE { emit_common_flag_private }
	| EXTERN { emit_common_flag_extern }

constraints:
	| COLON; POPEN; ct = complex_type; COMMA; ctl = separated_nonempty_list(COMMA,complex_type); PCLOSE {
		emit_constraints_multiple (ct :: ctl)
	}
	| COLON; ct = complex_type { emit_constraints_single ct }
	| { emit_constraints_none }

%inline type_decl_parameter:
	| annotations = annotations; name = pos(dollar_ident); ctl = constraints {
		emit_type_decl_parameter annotations name ctl
	}

type_decl_parameters:
	| LT; tl = separated_nonempty_list(COMMA,type_decl_parameter); GT { tl }
	| { [] }

import_mode:
	| IN; ident = ident { emit_import_mode_alias ident }
	| AS; ident = ident { emit_import_mode_alias ident }
	| DOTSTAR { emit_import_mode_all }
	| { emit_import_mode_normal }

class_or_interface:
	| CLASS { emit_class_flag_class }
	| INTERFACE { emit_class_flag_interface }

%inline class_decl2:
	| flags = class_or_interface; name = pos(dollar_ident)?; tl = type_decl_parameters; rl = class_relations*; BROPEN; l = class_field*; BRCLOSE {
		emit_class flags name tl rl l
	}

decl:
	| IMPORT; path = path_with_pos; mode = import_mode; SEMICOLON { emit_import path mode (mk $startpos $endpos) }
	| USING; path = path_with_pos; SEMICOLON { emit_using path (mk $startpos $endpos) }
	| annotations = annotations; flags = common_flags*; c = class_decl2 {
		emit_class_decl annotations flags c (mk $startpos $endpos)
	}
	| annotations = annotations; flags = common_flags*; ENUM; name = pos(dollar_ident); tl = type_decl_parameters; BROPEN; l = enum_field*; BRCLOSE {
		emit_enum_decl annotations flags name tl l (mk $startpos $endpos)
	}
	| annotations = annotations; flags = common_flags*; TYPEDEF; name = pos(dollar_ident); tl = type_decl_parameters; ASSIGN; ct = complex_type; SEMICOLON? {
		emit_typedef_decl annotations flags name tl ct (mk $startpos $endpos)
	}
	| annotations = annotations; flags = common_flags*; ABSTRACT; name = pos(dollar_ident); tl = type_decl_parameters; st = underlying_type?; rl = abstract_relations*; BROPEN; l = class_field*; BRCLOSE {
		emit_abstract_decl annotations flags name tl st rl l (mk $startpos $endpos)
	}

(* File *)

package:
	| PACKAGE; path = path?; SEMICOLON { emit_package path }

file:
	| package = package?; decls = decl*; EOF { emit_file package decls }

(* Entry points *)

expr_only:
	| expr = expr; EOF { expr }

class_fields_only:
	| cff = class_field+; EOF { emit_class_fields_only cff }

decls_only:
	| decls = decl+; EOF { emit_decls_only decls }

block_elements_only:
	| el = block_element+; EOF { emit_block_elements_only el }

unused:
	| WHITESPACE | COMMENTLINE | NEWLINE | NONSENSE | SHARPIF | SHARPELSE
	| SHARPELSEIF | SHARPEND | SHARPERROR | SHARPLINE | UNCLOSED { }