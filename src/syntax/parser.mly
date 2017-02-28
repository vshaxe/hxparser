%{
open Ast

let mk = Pos.Range.make

let make_is e (t,p_t) p p_is =
	let e_is = EField((EConst(Ident "Std"),Pos.Range.null),"is"),p_is in
	let e2 = expr_of_type_path (t.tpackage,t.tname) p_t in
	ECall(e_is,[e;e2]),p

let make_class annotation flags c p =
	let (flags2,name,tl,rl,l) = c in
	let def = {
		d_name = (match name with None -> "",p (* TODO: syntax error... *) | Some name -> name);
		d_doc = fst annotation;
		d_params = tl;
		d_meta = snd annotation;
		d_flags = (List.map fst flags) @ flags2 @ rl;
		d_data = l;
	} in
	(EClass def,p)
%}

(* Token definitions *)

%token EOF
%token PACKAGE IMPORT USING PUBLIC CLASS INTERFACE ENUM TYPEDEF ABSTRACT PRIVATE STATIC INLINE MACRO OVERRIDE DYNAMIC EXTERN FUNCTION EXTENDS IMPLEMENTS FROM TO AS IS
%token VAR CAST THROW NEW IF ELSE WHILE DO FOR TRY CATCH RETURN BREAK CONTINUE SWITCH CASE DEFAULT UNTYPED THIS TRUE FALSE NULL IN
%token BROPEN BRCLOSE POPEN PCLOSE BKOPEN BKCLOSE COMMA COLON SEMICOLON QUESTIONMARK DOT
%token ASSIGN PLUS MINUS STAR SLASH
%token EQUALS NOTEQUALS LT LTE GT GTE BOOLAND BOOLOR SHL SHR USHR ARROW INTERVAL DOUBLEARROW
%token EXCLAMATION PERCENT AND OR XOR DOTSTAR
%token ASSIGNMOD ASSIGNAND ASSIGNOR ASSIGNXOR ASSIGNPLUS ASSIGNMINUS ASSIGNSTAR ASSIGNSLASH ASSIGNSHL ASSIGNSHR ASSIGNUSHR ASSIGNBOOLOR ASSIGNBOOLAND
%token INCREMENT DECREMENT TILDE
%token <string> IDENT DOLLAR_IDENT DOT_IDENT STRING STRING2 INT FLOAT METADATA METADATA_OPEN COMMENT
%token <string * string> REGEX
(* Comment these out if you generate messages, they are unused. *)
%token <string> COMMENTLINE WHITESPACE NEWLINE NONSENSE
%token SHARPIF SHARPELSE SHARPELSEIF SHARPEND SHARPERROR SHARPLINE
%token <token> UNCLOSED

(* Precedence *)

%nonassoc LOWEST
%left COMMA
%right RETURN MACRO
%left CATCH ELSE WHILE IF
%left IN IS
%right ASSIGN ASSIGNMOD ASSIGNAND ASSIGNOR ASSIGNXOR ASSIGNPLUS ASSIGNMINUS ASSIGNSTAR ASSIGNSLASH ASSIGNSHL ASSIGNSHR ASSIGNUSHR ASSIGNBOOLOR ASSIGNBOOLAND
%right DOUBLEARROW
%left BOOLOR
%left BOOLAND
%left INTERVAL
%left EQUALS NOTEQUALS GT LT LTE GTE
%left OR AND XOR
%left SHL SHR USHR
%left PLUS MINUS
%left STAR SLASH
%left PERCENT
%right INCREMENT DECREMENT EXCLAMATION
%left ARROW
%right QUESTIONMARK
%right POPEN BKOPEN BROPEN
%nonassoc NONDOT BRCLOSE
%left DOT_IDENT COLON DOT

(* Start and types *)

%start <string list option * Ast.type_decl list> file
%start <Ast.expr> expr_only
%start <Ast.expr> sharp_condition
%start <string> sharp_error_message
%start <string> sharp_line_number
%start <Ast.class_field list> class_fields_only
%start <Ast.type_decl> class_decl_only
%start <Ast.expr list> block_elements_only
%start <unit> unused

%on_error_reduce expr_open expr_closed expr
%on_error_reduce path complex_type

%%

(* Various *)

lplist(X):
	| %prec LOWEST { [] }
	| x = X; xs = lplist(X) { x :: xs }

lpoption(X):
	| %prec LOWEST { None }
	| x = X { Some x }

metadata:
	| name = METADATA { (Meta.Custom name,[],mk $startpos $endpos) }
	| name = METADATA_OPEN; el = separated_list(COMMA, expr); PCLOSE { (Meta.Custom name,el,mk $startpos $endpos) }

documentation:
	| comment = COMMENT { Some comment }
	| { None }

annotations:
	| doc = documentation; meta = metadata* { doc,meta }

%inline pos(f):
	| x = f { x,mk $startpos $endpos }

ident:
	| s = IDENT { s }
	| FROM { "from" }
	| TO { "to" }
	| AS { "as" }
	| IS { "is" }

dollar_ident:
	| s = ident | s = DOLLAR_IDENT { s }

dot_ident:
	| s = DOT_IDENT { s }
	| DOT { "." }

path:
	| ident = dollar_ident; l = lplist(dot_ident) { ident :: l }

path_with_pos:
	| ident = pos(dollar_ident); l = lplist(pos(dot_ident)) { ident :: l }

(* Operators *)

op_assign:
	| ASSIGN { OpAssign }
	| ASSIGNMOD { OpAssignOp(OpMod) }
	| ASSIGNAND { OpAssignOp(OpAnd) }
	| ASSIGNOR { OpAssignOp(OpOr) }
	| ASSIGNXOR { OpAssignOp(OpXor) }
	| ASSIGNPLUS { OpAssignOp(OpAdd) }
	| ASSIGNMINUS { OpAssignOp(OpSub) }
	| ASSIGNSTAR { OpAssignOp(OpMult) }
	| ASSIGNSLASH { OpAssignOp(OpDiv) }
	| ASSIGNSHL { OpAssignOp(OpShl) }
	| ASSIGNSHR { OpAssignOp(OpShr) }
	| ASSIGNUSHR { OpAssignOp(OpUShr) }
	| ASSIGNBOOLOR { OpAssignOp(OpBoolOr) }
	| ASSIGNBOOLAND { OpAssignOp(OpBoolAnd) }

op_compare:
	| EQUALS { OpEq }
	| NOTEQUALS { OpNotEq }
	| GT { OpGt }
	| LT { OpLt }
	| LTE { OpLte }
	| GTE { OpGte }

op_bit:
	| OR { OpOr }
	| AND { OpAnd }
	| XOR { OpXor }

%inline op:
	| PERCENT { OpMod }
	| STAR { OpMult }
	| SLASH { OpDiv }
	| PLUS { OpAdd }
	| MINUS { OpSub }
	| SHL { OpShl }
	| SHR { OpShr }
	| USHR { OpUShr }
	| INTERVAL { OpInterval }
	| BOOLAND { OpBoolAnd }
	| BOOLOR { OpBoolOr }
	| DOUBLEARROW { OpArrow }

unary_prefix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| TILDE { NegBits }
	| EXCLAMATION { Not }
	| MINUS { Neg }

unary_postfix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| EXCLAMATION { Not }

(* Expression *)

string:
	| s = STRING | s = STRING2 { s }

literal_string:
	| s = string { String s }

literal_int:
	| s = INT %prec NONDOT { Int s }

literal_float:
	| s = FLOAT { Float s }

literal_regex:
	| s = REGEX { Regexp(fst s,snd s) }

literal:
	| literal_string | literal_int | literal_float | literal_regex { $1 }

%inline const:
	| s = ident { Ident s }
	| s = literal { s }

call_args:
	| POPEN; el = separated_list(COMMA, expr); PCLOSE { el }

assignment:
	| ASSIGN; e1 = expr { e1 }

var_declaration:
	| name = pos(dollar_ident); ct = lpoption(type_hint); eo = lpoption(assignment) { (name,ct,eo) }

var_declarations_next:
	| %prec LOWEST { [] }
	| COMMA; vl = var_declarations { vl }

var_declarations:
	| v = var_declaration; vl = var_declarations_next { v :: vl }

else_expr:
	| ELSE; e1 = expr { e1 }

catch:
	| CATCH; POPEN; name = pos(dollar_ident); ct = type_hint; PCLOSE; e1 = expr { (name,ct,e1,mk $startpos $endpos) }

guard:
	| IF; POPEN; e1 = expr; PCLOSE { e1 }

case:
	| CASE; el = separated_nonempty_list(COMMA,expr); eg = guard?; COLON; el2 = block_element* {
		let b = match el2 with [] -> None | _ -> Some (EBlock el2,mk $startpos(el2) $endpos(el2)) in
		(el,eg,b,mk $startpos $endpos) }
	| DEFAULT; COLON; el = block_element* {
		let b = match el with [] -> None | _ -> Some (EBlock el,mk $startpos(el) $endpos(el)) in
		([EConst(Ident "_"),mk $startpos($1) $endpos($1)],None,b,mk $startpos $endpos)
	}

func:
	| name = dollar_ident?; tl = type_decl_parameters; POPEN; el = separated_list(COMMA,function_argument); PCLOSE; ct = type_hint?; e1 = expr {
		let f = {
			f_params = tl;
			f_type = ct;
			f_args = el;
			f_expr = Some e1
		} in
		name,f
	}

array_elements_next:
	| { [] }
	| COMMA; el = array_elements { el }

array_elements:
	| { [] }
	| e1 = expr; el = array_elements_next { e1 :: el }

object_field_name:
	| name = dollar_ident { name }
	| name = string { name }

object_field:
	| name = object_field_name; COLON; e = expr { ((name,mk $startpos(name) $endpos(name)),e) }

object_fields_next:
	| { [] }
	| COMMA; fl = object_fields { fl }

object_fields:
	| %prec LOWEST { [] }
	| f = object_field; fl = object_fields_next { f :: fl }

macro_expr_type_hint:
	| type_hint { EConst(Ident "null"),mk $startpos $endpos }

macro_expr_var:
	| VAR; var_declarations { EConst(Ident "null"),mk $startpos $endpos }

macro_expr_class_decl:
	| class_decl2 { EConst(Ident "null"),mk $startpos $endpos }

macro_expr_expr:
	| expr_open %prec MACRO { EConst(Ident "null"),mk $startpos $endpos }
	| expr_closed { EConst(Ident "null"),mk $startpos $endpos }

macro_expr:
	| macro_expr_type_hint | macro_expr_var | macro_expr_class_decl | macro_expr_expr { $1 }

block_element_var:
	| VAR; vl = var_declarations; SEMICOLON { EVars(vl),mk $startpos $endpos }

block_element_inline_function:
	| INLINE; FUNCTION; f = func; SEMICOLON { EFunction(fst f,snd f),mk $startpos $endpos }

block_element_expr:
	| e = expr_open; SEMICOLON { e }
	| e = expr_closed; SEMICOLON { e }

%inline block_element:
	| block_element_var | block_element_inline_function | block_element_expr { $1 }

field_expr_none:
	| SEMICOLON { None }

field_expr_block:
	| e = expr_block { Some e }

field_expr_expr:
	| e = expr; SEMICOLON { Some e }

%inline field_expr:
	| field_expr_none | field_expr_block | field_expr_expr { $1 }

keyword_ident:
	| THIS { EConst (Ident "this"),mk $startpos $endpos }
	| TRUE { EConst (Ident "true"),mk $startpos $endpos }
	| FALSE { EConst (Ident "false"),mk $startpos $endpos }
	| NULL { EConst (Ident "null"),mk $startpos $endpos }

expr_empty_block:
	| BROPEN; BRCLOSE { EBlock [],mk $startpos $endpos }

expr_nonempty_block:
	| BROPEN; el = nonempty_list(block_element); BRCLOSE { EBlock el,mk $startpos $endpos }

%inline expr_block:
	| expr_empty_block | expr_nonempty_block { $1 }

expr_var:
	| VAR; v = var_declaration { EVars([v]),mk $startpos $endpos }

expr_metadata:
	| m = metadata; e1 = expr { EMeta(m,e1),mk $startpos $endpos }

expr_throw:
	| THROW; e1 = expr { EThrow e1,mk $startpos $endpos }

expr_if:
	| IF; POPEN; e1 = expr; PCLOSE; e2 = expr; eo = lpoption(else_expr) { EIf(e1,e2,eo),mk $startpos $endpos }

expr_return:
	| RETURN { EReturn None,mk $startpos $endpos }

expr_return_value:
	| RETURN; e = expr { EReturn (Some e),mk $startpos $endpos }

expr_break:
	| BREAK { EBreak,mk $startpos $endpos }

expr_continue:
	| CONTINUE { EContinue,mk $startpos $endpos }

expr_do:
	| DO; e1 = expr; WHILE; POPEN; e2 = expr; PCLOSE { EWhile(e2,e1,DoWhile),mk $startpos $endpos }

expr_try:
	| TRY; e1 = expr; catches = lplist(catch); { ETry(e1,catches),mk $startpos $endpos }

expr_switch:
	| SWITCH; e1 = expr; BROPEN; cases = case*; BRCLOSE { ESwitch(e1,cases,None),mk $startpos $endpos }

expr_for:
	| FOR; POPEN; e1 = expr; PCLOSE; e2 = expr { EFor(e1,e2),mk $startpos $endpos }

expr_while:
	| WHILE; POPEN; e1 = expr; PCLOSE; e2 = expr { EWhile(e1,e2,NormalWhile),mk $startpos $endpos }

expr_untyped:
	| UNTYPED; e1 = expr { EUntyped e1,mk $startpos $endpos }

expr_object_declaration:
	| BROPEN; fl = object_fields; BRCLOSE { EObjectDecl fl,mk $startpos $endpos }

expr_unsafe_cast:
	| CAST; e1 = expr { ECast(e1,None),mk $startpos $endpos }

expr_safe_cast:
	| CAST; POPEN; e1 = expr; COMMA; ct = complex_type; PCLOSE { ECast(e1,Some ct),mk $startpos $endpos }

expr_new:
	| NEW; tp = type_path; el = call_args { ENew(tp,el),mk $startpos $endpos }

lambda_function_argument:
	| name = expr; ct = lpoption(type_hint); eo = assignment? { (name,false,[],ct,eo) }
	| QUESTIONMARK; name = pos(dollar_ident); ct = lpoption(type_hint); eo = assignment? { ((EConst(Ident (fst name)),snd name),true,[],ct,eo) }

lambda_function_arguments:
	| COMMA; l = separated_nonempty_list(COMMA,function_argument) { l }
	| { [] }

expr_typechecks:
	| POPEN; arg = lambda_function_argument; args = lambda_function_arguments; PCLOSE { arg,args }

expr_is:
	| POPEN; e1 = expr; is = pos(IS); tp = type_path; PCLOSE { make_is e1 tp (snd is) (mk $startpos $endpos) }

expr_array_declaration:
	| BKOPEN; el = array_elements; BKCLOSE { EArrayDecl el,mk $startpos $endpos }

expr_function:
	| FUNCTION; f = func { EFunction(fst f,snd f),mk $startpos $endpos }

expr_unary_prefix:
	| op = unary_prefix; e1 = expr_inline %prec INCREMENT { EUnop(op,Prefix,e1),mk $startpos $endpos }

expr_field:
	| e1 = expr_open; name = dot_ident { EField(e1,name),mk $startpos $endpos }

expr_call:
	| e1 = expr_open; el = call_args { ECall(e1,el),mk $startpos $endpos }

expr_array_access:
	| e1 = expr_open; BKOPEN; e2 = expr; BKCLOSE { EArray(e1,e2),mk $startpos $endpos }

expr_binop:
	| e1 = expr_open; op = op; e2 = expr_inline { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = expr_open; op = op_bit; e2 = expr_inline %prec AND { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = expr_open; op = op_compare; e2 = expr_inline %prec EQUALS { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = expr_open; op = op_assign; e2 = expr_inline %prec ASSIGN { EBinop(op,e1,e2),mk $startpos $endpos }

expr_unary_postfix:
	| e1 = expr_open; op = unary_postfix { EUnop(op,Postfix,e1),mk $startpos $endpos }

expr_ternary:
	| e1 = expr_open; QUESTIONMARK; e2 = expr; COLON; e3 = expr { ETernary(e1,e2,e3),mk $startpos $endpos }

expr_in:
	| e1 = expr_open; IN; e2 = expr { EIn(e1,e2),mk $startpos $endpos }

expr_dotint:
	| s = INT; DOT { EConst(Float (s ^ ".")),mk $startpos $endpos }

expr_dollarident:
	| s = DOLLAR_IDENT %prec LOWEST { EConst(Ident s),mk $startpos $endpos }

expr_macro_escape:
	| s = pos(DOLLAR_IDENT); BROPEN; e1 = expr; BRCLOSE { EMeta(((Meta.Custom (fst s)),[],snd s),e1),mk $startpos $endpos }

expr_macro:
	| MACRO; e = macro_expr { e }

expr_const:
	| const = const { EConst const,mk $startpos $endpos }

expr_keyword_ident:
	| e1 = keyword_ident { e1 }

expr_lambda:
	| args = expr_typechecks; ARROW; e1 = expr {
		let args = match fst args with
		| ((EConst(Ident s),p),opt,meta,ct,eo) -> ((s,p),opt,meta,ct,eo) :: (snd args)
		| _ -> $syntaxerror
		in
		let f = {
			f_params = [];
			f_type = None;
			f_args = args;
			f_expr = Some e1
		} in
		EFunction(None, f),mk $startpos $endpos
	}
	| POPEN; PCLOSE; ARROW; e1 = expr {
		let f = {
			f_params = [];
			f_type = None;
			f_args = [];
			f_expr = Some e1
		} in
		EFunction(None, f),mk $startpos $endpos
	}

expr_closed:
	| expr_metadata | expr_macro | expr_block | expr_throw | expr_if | expr_return | expr_return_value | expr_break | expr_continue
	| expr_do | expr_try | expr_switch | expr_for | expr_while | expr_untyped | expr_lambda { $1 }

expr_open:
	| expr_object_declaration | expr_unsafe_cast | expr_safe_cast | expr_new
	| expr_is | expr_array_declaration | expr_function | expr_unary_prefix
	| expr_field | expr_call | expr_array_access | expr_binop | expr_unary_postfix
	| expr_ternary | expr_in | expr_dotint | expr_dollarident | expr_macro_escape
	| expr_const | expr_keyword_ident { $1 }
	| tc = expr_typechecks {
		let e = match tc with
		| (e,false,[],None,None),[] -> EParenthesis e
		| (e,false,[],Some ct,None),[] -> ECheckType(e,ct)
		| _ -> $syntaxerror
		in
		e,mk $startpos $endpos
	}

%inline expr_inline:
	| e = expr_closed | e = expr_open | e = expr_var { e }

expr:
	| e = expr_closed | e = expr_open %prec LOWEST | e = expr_var { e }

(* Type hints *)

structural_extension:
	| GT; tp = type_path; COMMA { tp }

anonymous_type_field:
	| opt = QUESTIONMARK?; name = pos(dollar_ident); ct = type_hint {
		let cff = {
			cff_name = name;
			cff_meta = (match opt with None -> [] | Some _ -> [Meta.Optional,[],Pos.Range.null]);
			cff_access = [];
			cff_doc = None;
			cff_kind = FVar(Some ct,None);
			cff_pos = mk $symbolstartpos $endpos;
		} in
		cff
	}

anonymous_type_fields_short_next:
	| { [] }
	| COMMA; fl = anonymous_type_fields_short { fl }

anonymous_type_fields_short:
	| { [] }
	| f = anonymous_type_field; fl = anonymous_type_fields_short_next { f :: fl }

anonymous_type_fields:
	| l = class_field+ { l }
	| l = anonymous_type_fields_short { l }

complex_type_parent:
	| POPEN; ct = complex_type; PCLOSE { CTParent ct,mk $startpos $endpos }

complex_type_extension:
	| BROPEN; l = structural_extension+; cffl = anonymous_type_fields; BRCLOSE { CTExtend(l,cffl),mk $startpos $endpos }

complex_type_anonymous:
	| BROPEN; l = anonymous_type_fields; BRCLOSE { CTAnonymous l,mk $startpos $endpos }

complex_type_optional:
	| QUESTIONMARK; ct = complex_type; { CTOptional ct,mk $startpos $endpos }

complex_type_path:
	| tp = type_path { CTPath (fst tp),snd tp }

complex_type_function:
	| ct1 = complex_type; ARROW; ct2 = complex_type {
		match fst ct2 with
		| CTFunction (args,r) ->
			CTFunction (ct1 :: args,r),mk $startpos $endpos
		| _ ->
			CTFunction ([ct1],ct2),mk $startpos $endpos
	}

complex_type:
	| complex_type_parent | complex_type_extension | complex_type_anonymous | complex_type_optional
	| complex_type_path | complex_type_function { $1 }

type_path_parameter_bracket:
	| BKOPEN; el = array_elements; BKCLOSE { TPExpr(EArrayDecl el,mk $startpos $endpos) }

type_path_parameter_complex_type:
	| ct = complex_type { TPType ct }

type_path_parameter_literal:
	| cst = literal { TPExpr (EConst cst,mk $startpos $endpos) }

%inline type_path_parameter:
	| type_path_parameter_bracket | type_path_parameter_complex_type | type_path_parameter_literal { $1 }
	/*| e = expr { TPExpr e }*/

type_path_parameters:
	| LT; tl = separated_nonempty_list(COMMA,type_path_parameter); GT { tl }
	| %prec LOWEST { [] }

type_path:
	| path = path; l = type_path_parameters {
		match List.rev path with
		| [] -> assert false
		| name :: pack -> { tpackage = List.rev pack; tname = name; tsub = None; tparams = l }, mk $startpos $endpos
	}

type_hint:
	| COLON; ct = complex_type { ct }

(* Field *)

function_argument:
	| annotation = annotations; opt = QUESTIONMARK?; name = pos(dollar_ident); ct = type_hint?; eo = assignment? { (name,(match opt with None -> false | Some _ -> true),snd annotation,ct,eo) }

function_arguments:
	| l = separated_list(COMMA,function_argument) { l }

modifier:
	| STATIC { AStatic }
	| MACRO { AMacro }
	| PUBLIC { APublic }
	| PRIVATE { APrivate }
	| OVERRIDE { AOverride }
	| DYNAMIC { ADynamic }
	| INLINE { AInline }

function_name:
	| s = pos(dollar_ident) { s }
	| s = pos(NEW) { "new",pos s }

property_ident:
	| s = pos(ident) { s }
	| s = pos(DYNAMIC) { "dynamic",pos s }
	| s = pos(DEFAULT) { "default",pos s }
	| s = pos(NULL) { "null",pos s }

function_field:
	| annotation = annotations; ml = modifier*; FUNCTION; name = function_name; tl = type_decl_parameters; POPEN; args = function_arguments; PCLOSE; ct = type_hint? eo = field_expr {
		let f = {
			f_params = tl;
			f_args = args;
			f_type = ct;
			f_expr = eo;
		} in
		let cff = {
			cff_name = name;
			cff_doc = fst annotation;
			cff_pos = mk $startpos $endpos;
			cff_meta = snd annotation;
			cff_access = ml;
			cff_kind = FFun f
		 } in
		 cff
	}

variable_field:
	| annotation = annotations; ml = modifier*; VAR; name = pos(dollar_ident); ct = type_hint?; eo = assignment?; SEMICOLON {
		let cff = {
			cff_name = name;
			cff_doc = fst annotation;
			cff_pos = mk $startpos $endpos;
			cff_meta = snd annotation;
			cff_access = ml;
			cff_kind = FVar(ct,eo);
		 } in
		 cff
	}

property_field:
	| annotation = annotations; ml = modifier*; VAR; name = pos(dollar_ident); POPEN; get = property_ident; COMMA; set = property_ident; PCLOSE; ct = type_hint?; eo = assignment?; SEMICOLON {
		let cff = {
			cff_name = name;
			cff_doc = fst annotation;
			cff_pos = mk $startpos $endpos;
			cff_meta = snd annotation;
			cff_access = ml;
			cff_kind = FProp(get,set,ct,eo);
		 } in
		 cff
	}

%inline class_field:
	| cff = function_field | cff = variable_field | cff = property_field { cff }

enum_field_arg:
	| opt = QUESTIONMARK?; name = dollar_ident; ct = type_hint; { (name,(match opt with None -> false | Some _ -> true),ct) }

enum_field_args:
	| { [] }
	| POPEN; l = separated_list(COMMA,enum_field_arg); PCLOSE { l }

enum_field:
	| annotation = annotations; name = pos(dollar_ident); tl = type_decl_parameters; args = enum_field_args; ct = type_hint?; SEMICOLON {
		let ef = {
			ec_name = name;
			ec_doc = fst annotation;
			ec_meta = snd annotation;
			ec_args = args;
			ec_params = tl;
			ec_type = ct;
			ec_pos = mk $startpos $endpos
		} in
		ef
	}


(* Type declaration *)

class_relations:
	| EXTENDS; path = type_path { HExtends path }
	| IMPLEMENTS; path = type_path { HImplements path }

abstract_relations:
	| TO; ct = complex_type { AToType ct }
	| FROM; ct = complex_type { AFromType ct }

underlying_type:
	| POPEN; ct = complex_type; PCLOSE { ct }

common_flags:
	| PRIVATE { HPrivate,EPrivate }
	| EXTERN { HExtern,EExtern }

constraints_multiple:
	| COLON; POPEN; ct = complex_type; COMMA; ctl = separated_nonempty_list(COMMA,complex_type); PCLOSE { ct :: ctl }

constraints_single:
	| COLON; ct = complex_type { [ct] }

constraints_none:
	| { [] }

%inline constraints:
	| constraints_multiple | constraints_single | constraints_none { $1 }

type_decl_parameter:
	| annotation = annotations; name = pos(dollar_ident); ctl = constraints {
		let tp = {
			tp_name = name;
			tp_params = [];
			tp_constraints = ctl;
			tp_meta = snd annotation
		} in
		tp
	}

type_decl_parameters:
	| LT; tl = separated_nonempty_list(COMMA,type_decl_parameter); GT { tl }
	| { [] }

import_mode_alias:
	| IN; ident = ident { IAsName ident }
	| AS; ident = ident { IAsName ident }

import_mode_all:
	| DOTSTAR { IAll }

%inline import_mode:
	| import_mode_alias | import_mode_all { $1 }
	| { INormal }

import:
	| path = path_with_pos; mode = import_mode { EImport(path,mode),mk $startpos $endpos }

class_or_interface:
	| CLASS { [] }
	| INTERFACE { [HInterface] }

%inline class_decl2:
	| flags = class_or_interface; name = pos(dollar_ident)?; tl = type_decl_parameters; rl = class_relations*; BROPEN; l = class_field*; BRCLOSE { flags,name,tl,rl,l }

import_decl:
	| IMPORT; import = import; SEMICOLON { import }

using_decl:
	| USING; path = path_with_pos; SEMICOLON { EUsing path,mk $startpos $endpos }

class_decl:
	| annotation = annotations; flags = common_flags*; c = class_decl2 { make_class annotation flags c (mk $startpos $endpos) }

enum_decl:
	| annotation = annotations; flags = common_flags*; ENUM; name = dollar_ident; tl = type_decl_parameters; BROPEN; l = enum_field*; BRCLOSE {
		let def = {
			d_name = (name,mk $startpos(name) $endpos(name));
			d_doc = fst annotation;
			d_params = tl;
			d_meta = snd annotation;
			d_flags = List.map snd flags;
			d_data = l;
		} in
		(EEnum def,mk $startpos $endpos)
	}

typedef_decl:
	| annotation = annotations; flags = common_flags*; TYPEDEF; name = dollar_ident; tl = type_decl_parameters; ASSIGN; ct = complex_type; SEMICOLON? {
		let def = {
			d_name = (name,mk $startpos(name) $endpos(name));
			d_doc = fst annotation;
			d_params = tl;
			d_meta = snd annotation;
			d_flags = List.map snd flags;
			d_data = ct
		} in
		(ETypedef def,mk $startpos $endpos)
	}

abstract_decl:
	| annotation = annotations; flags = common_flags*; ABSTRACT; name = dollar_ident; tl = type_decl_parameters; st = underlying_type?; rl = abstract_relations*; BROPEN; l = class_field*; BRCLOSE {
		let flags = List.map (fun (_,c) -> match c with EPrivate -> APrivAbstract | EExtern -> AExtern) flags in
		let flags = (match st with None -> flags | Some t -> AIsType t :: flags) in
		let def = {
			d_name = (name,mk $startpos(name) $endpos(name));
			d_doc = fst annotation;
			d_params = tl;
			d_meta = snd annotation;
			d_flags = flags @ rl;
			d_data = l;
		} in
		(EAbstract def,mk $startpos $endpos)
	}

%inline decl:
	| import = import_decl { import }
	| using = using_decl { using }
	| c = class_decl { c }
	| en = enum_decl { en }
	| t = typedef_decl { t }
	| a = abstract_decl { a }

(* File *)

package:
	| PACKAGE; l = loption(path); SEMICOLON { l }

file:
	| package = package?; decls = decl*; EOF { package,decls }

(* Conditional compilation *)

sharp_condition_both:
	| e = const { EConst e,mk $startpos $endpos }
	| MACRO { EConst(Ident "macro"),mk $startpos $endpos }
	| e1 = keyword_ident { e1 }
	| POPEN; e = sharp_condition_any; PCLOSE { e }
	| op = unary_prefix; e1 = sharp_condition { EUnop(op,Prefix,e1),mk $startpos $endpos }

sharp_condition_any:
	| e = sharp_condition_both { e }
	| e1 = sharp_condition_any; op = op; e2 = sharp_condition_any { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = sharp_condition_any; op = op_assign; e2 = sharp_condition_any %prec ASSIGN { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = sharp_condition_any; op = op_bit; e2 = sharp_condition_any %prec OR { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = sharp_condition_any; op = op_compare; e2 = sharp_condition_any %prec EQUALS { EBinop(op,e1,e2),mk $startpos $endpos }

(* Entry points *)

sharp_condition:
	| e = sharp_condition_both { e }

sharp_error_message:
	| s = string { s }

sharp_line_number:
	| i = INT { i }

expr_only:
	| expr = expr; EOF { expr }

class_fields_only:
	| cff = class_field+; EOF { cff }

class_decl_only:
	| c = class_decl2; EOF { make_class (None,[]) [] c (mk $startpos $endpos) }

block_elements_only:
	| el = block_element+; EOF { el }

unused:
	| WHITESPACE | COMMENTLINE | NEWLINE | NONSENSE | SHARPIF | SHARPELSE
	| SHARPELSEIF | SHARPEND | SHARPERROR | SHARPLINE | UNCLOSED { }