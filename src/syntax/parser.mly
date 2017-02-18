%{
open Ast

let mk = Pos.Range.make

let make_is e (t,p_t) p p_is =
	let e_is = EField((EConst(Ident "Std"),Pos.Range.null),"is"),p_is in
	let e2 = expr_of_type_path (t.tpackage,t.tname) p_t in
	ECall(e_is,[e;e2]),p
%}

(* Token definitions *)

%token EOF
%token PACKAGE IMPORT USING PUBLIC CLASS INTERFACE ENUM TYPEDEF ABSTRACT PRIVATE STATIC INLINE MACRO OVERRIDE DYNAMIC EXTERN FUNCTION EXTENDS IMPLEMENTS FROM TO AS IS
%token VAR CAST THROW NEW IF ELSE WHILE DO FOR TRY CATCH RETURN BREAK CONTINUE SWITCH CASE DEFAULT UNTYPED THIS TRUE FALSE NULL IN
%token BROPEN BRCLOSE POPEN PCLOSE BKOPEN BKCLOSE COMMA COLON SEMICOLON QUESTIONMARK DOT
%token ASSIGN PLUS MINUS STAR SLASH
%token EQUALS NOTEQUALS LT LTE GT BOOLAND BOOLOR SHL ARROW INTERVAL DOUBLEARROW
%token EXCLAMATION PERCENT AND OR XOR DOTSTAR
%token ASSIGNMOD ASSIGNAND ASSIGNOR ASSIGNXOR ASSIGNPLUS ASSIGNMINUS ASSIGNSTAR ASSIGNSLASH ASSIGNSHL ASSIGNBOOLOR ASSIGNBOOLAND
%token INCREMENT DECREMENT TILDE
%token <string> IDENT DOLLAR_IDENT DOT_IDENT STRING INT FLOAT METADATA METADATA_OPEN COMMENT
%token <string * string> REGEX
(* Comment these out if you generate messages, they are unused. *)
%token <string> COMMENTLINE WHITESPACE
%token SHARPIF SHARPELSE SHARPELSEIF SHARPEND SHARPERROR SHARPLINE

(* Precedence *)

%nonassoc LOWEST
%left COMMA
%right CAST UNTYPED RETURN THROW MACRO
%left CATCH ELSE WHILE IF
%left IN IS
%right ASSIGN ASSIGNMOD ASSIGNAND ASSIGNOR ASSIGNXOR ASSIGNPLUS ASSIGNMINUS ASSIGNSTAR ASSIGNSLASH ASSIGNSHL ASSIGNBOOLOR ASSIGNBOOLAND
%right DOUBLEARROW
%left BOOLOR
%left BOOLAND
%left INTERVAL
%left EQUALS NOTEQUALS GT LT LTE
%left OR AND XOR
%left SHL
%left PLUS MINUS
%left STAR SLASH
%left PERCENT
%right INCREMENT DECREMENT TILDE EXCLAMATION
%left ARROW
%right QUESTIONMARK
%right POPEN BKOPEN BROPEN
%left DOT_IDENT COLON

(* Start and types *)

%start file
%start expr_only
%start sharp_condition
%start sharp_error_message
%start sharp_line_number
%type <string list option * Ast.type_decl list> file
%type <Ast.expr> expr_only
%type <Ast.expr> sharp_condition
%type <string> sharp_error_message
%type <string> sharp_line_number

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

path:
	| ident = dollar_ident; l = lplist(DOT_IDENT) { ident :: l }

path_with_pos:
	| ident = pos(dollar_ident); l = lplist(pos(DOT_IDENT)) { ident :: l }

(* Operators *)

%inline op:
	| PERCENT { OpMod }
	| STAR { OpMult }
	| SLASH { OpDiv }
	| PLUS { OpAdd }
	| MINUS { OpSub }
	| SHL { OpShl }
	| OR { OpOr }
	| AND { OpAnd }
	| XOR { OpXor }
	| EQUALS { OpEq }
	| NOTEQUALS { OpNotEq }
	| GT { OpGt }
	| LT { OpLt }
	| LTE { OpLte }
	| INTERVAL { OpInterval }
	| BOOLAND { OpBoolAnd }
	| BOOLOR { OpBoolOr }
	| DOUBLEARROW { OpArrow }
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
	| ASSIGNBOOLOR { OpAssignOp(OpBoolOr) }
	| ASSIGNBOOLAND { OpAssignOp(OpBoolAnd) }
	| GT; ASSIGN { OpAssignOp(OpGt) }
	| GT; GT; ASSIGN { OpAssignOp(OpShr) }
	| GT; GT { if $endpos($1) <> $startpos($2) then $syntaxerror; OpShr }
	| GT; GT; GT; ASSIGN { OpAssignOp(OpUShr) }
	| GT; GT; GT { OpUShr }

%inline unary_prefix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| TILDE { NegBits }
	| EXCLAMATION { Not }
	| MINUS { Neg }

%inline unary_postfix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| EXCLAMATION { Not }

(* Expression *)

string:
	| s = STRING { s }

literal:
	| s = string { String s }
	| s = INT { Int s }
	| s = FLOAT { Float s }
	| s = REGEX { Regexp(fst s,snd s) }

const:
	| s = ident { Ident s }
	| s = literal { s }

object_field_name:
	| name = dollar_ident { name }
	| name = STRING { name }

object_field:
	| name = object_field_name; COLON; e = expr { ((name,mk $startpos(name) $endpos(name)),e) }

call_args:
	| POPEN; el = separated_list(COMMA, expr); PCLOSE { el }

assignment:
	| ASSIGN; e1 = expr_inline { e1 }

var_declaration:
	| name = pos(dollar_ident); ct = lpoption(type_hint); eo = lpoption(assignment) { (name,ct,eo) }

var_declarations_next:
	| %prec LOWEST { [] }
	| COMMA; vl = var_declarations { vl }

var_declarations:
	| v = var_declaration; vl = var_declarations_next { v :: vl }

else_expr:
	| ELSE; e1 = expr_inline { e1 }

catch:
	| CATCH; POPEN; name = pos(dollar_ident); ct = type_hint; PCLOSE; e1 = expr_inline %prec LOWEST { (name,ct,e1,mk $startpos $endpos) }

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
	| name = dollar_ident?; tl = type_decl_parameters; POPEN; el = separated_list(COMMA,function_argument); PCLOSE; ct = type_hint?; e1 = expr_inline %prec LOWEST {
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

object_fields_next:
	| { [] }
	| COMMA; el = object_fields { el }

object_fields:
	| { [] }
	| f = object_field; fl = object_fields_next { f :: fl }

macro_expr:
	| type_hint { EConst(Ident "null"),mk $startpos $endpos }
	| VAR; var_declarations { EConst(Ident "null"),mk $startpos $endpos }
	| class_decl { EConst(Ident "null"),mk $startpos $endpos }
	| expr_open %prec MACRO { EConst(Ident "null"),mk $startpos $endpos }
	| expr_closed { EConst(Ident "null"),mk $startpos $endpos }

block_element:
	| VAR; vl = var_declarations; SEMICOLON { EVars(vl),mk $startpos $endpos }
	| INLINE; FUNCTION; f = func; SEMICOLON { EFunction(fst f,snd f),mk $startpos $endpos }
	| e = expr_open; SEMICOLON { e }
	| e = expr_closed; SEMICOLON { e }

block_expr:
	| BROPEN; BRCLOSE { EBlock [],mk $startpos $endpos }
	| BROPEN; el = nonempty_list(block_element); BRCLOSE { EBlock el,mk $startpos $endpos }

field_expr:
	| SEMICOLON { None }
	| e = block_expr { Some e }
	| e = expr; SEMICOLON { Some e }

keyword_ident:
	| THIS { EConst (Ident "this"),mk $startpos $endpos }
	| TRUE { EConst (Ident "true"),mk $startpos $endpos }
	| FALSE { EConst (Ident "false"),mk $startpos $endpos }
	| NULL { EConst (Ident "null"),mk $startpos $endpos }

%inline expr_var:
	| VAR; v = var_declaration { EVars([v]),mk $startpos $endpos }

expr_closed:
	| m = metadata; e1 = expr_inline %prec LOWEST { EMeta(m,e1),mk $startpos $endpos }
	| MACRO; e = macro_expr { e }
	| e = block_expr { e }
	| THROW; e1 = expr_inline { EThrow e1,mk $startpos $endpos }
	| IF; POPEN; e1 = expr; PCLOSE; e2 = expr_inline; eo = lpoption(else_expr) { EIf(e1,e2,eo),mk $startpos $endpos }
	| RETURN { EReturn None,mk $startpos $endpos }
	| RETURN; e = expr_inline { EReturn (Some e),mk $startpos $endpos }
	| BREAK { EBreak,mk $startpos $endpos }
	| CONTINUE { EContinue,mk $startpos $endpos }
	| DO; e1 = expr; WHILE; POPEN; e2 = expr; PCLOSE { EWhile(e2,e1,DoWhile),mk $startpos $endpos }
	| TRY; e1 = expr_inline; catches = lplist(catch); { ETry(e1,catches),mk $startpos $endpos }
	| SWITCH; e1 = expr; BROPEN; cases = case*; BRCLOSE { ESwitch(e1,cases,None),mk $startpos $endpos }
	| FOR; POPEN; e1 = expr; PCLOSE; e2 = expr_inline %prec LOWEST { EFor(e1,e2),mk $startpos $endpos }
	| WHILE; POPEN; e1 = expr; PCLOSE; e2 = expr_inline %prec LOWEST { EWhile(e1,e2,NormalWhile),mk $startpos $endpos }
	| UNTYPED; e1 = expr_inline { EUntyped e1,mk $startpos $endpos }

expr_open:
	| BROPEN; f = object_field; fl = object_fields_next BRCLOSE { EObjectDecl (f :: fl),mk $startpos $endpos }
	| const = const { EConst const,mk $startpos $endpos }
	| e1 = keyword_ident { e1 }
	| CAST; e1 = expr_inline { ECast(e1,None),mk $startpos $endpos }
	| CAST; POPEN; e1 = expr; COMMA; ct = complex_type; PCLOSE { ECast(e1,Some ct),mk $startpos $endpos }
	| NEW; tp = type_path; el = call_args { ENew(tp,el),mk $startpos $endpos }
	| POPEN; e1 = expr; PCLOSE { EParenthesis e1,mk $startpos $endpos }
	| POPEN; e1 = expr; COLON; ct = complex_type; PCLOSE { ECheckType(e1,ct),mk $startpos $endpos }
	| POPEN; e1 = expr; is = pos(IS); tp = type_path; PCLOSE { make_is e1 tp (snd is) (mk $startpos $endpos) }
	| BKOPEN; el = array_elements; BKCLOSE { EArrayDecl el,mk $startpos $endpos }
	| FUNCTION; f = func { EFunction(fst f,snd f),mk $startpos $endpos }
	| op = unary_prefix; e1 = expr_inline { EUnop(op,Prefix,e1),mk $startpos $endpos }
	| e1 = expr_open; name = DOT_IDENT { EField(e1,name),mk $startpos $endpos }
	| e1 = expr_open; el = call_args { ECall(e1,el),mk $startpos $endpos }
	| e1 = expr_open; BKOPEN; e2 = expr; BKCLOSE { EArray(e1,e2),mk $startpos $endpos }
	| e1 = expr_open; op = op; e2 = expr_inline { EBinop(op,e1,e2),mk $startpos $endpos }
	| e1 = expr_open; op = unary_postfix { EUnop(op,Postfix,e1),mk $startpos $endpos }
	| e1 = expr_open; QUESTIONMARK; e2 = expr; COLON; e3 = expr_inline { ETernary(e1,e2,e3),mk $startpos $endpos }
	| e1 = expr_open; IN; e2 = expr_inline { EIn(e1,e2),mk $startpos $endpos }
	| s = INT; DOT { EConst(Float (s ^ ".")),mk $startpos $endpos }
	| s = DOLLAR_IDENT %prec LOWEST { EConst(Ident s),mk $startpos $endpos }
	| s = pos(DOLLAR_IDENT); BROPEN; e1 = expr; BRCLOSE { EMeta(((Meta.Custom (fst s)),[],snd s),e1),mk $startpos $endpos }

%inline expr_inline:
	| e = expr_closed | e = expr_open | e = expr_var { e }

expr:
	| e = expr_inline { e }

(* Type hints *)

structural_extension:
	| GT; tp = type_path; COMMA { tp }

anonymous_type_field_next:
	| { [] }
	| COMMA; cff = anonymous_type_field { cff }

anonymous_type_field:
	| { [] }
	| opt = QUESTIONMARK?; name = pos(dollar_ident); COLON; ct = complex_type; cffl = anonymous_type_field_next {
		let cff = {
			cff_name = name;
			cff_meta = (match opt with None -> [] | Some _ -> [Meta.Optional,[],Pos.Range.null]);
			cff_access = [];
			cff_doc = None;
			cff_kind = FVar(Some ct,None);
			cff_pos = mk $symbolstartpos $endpos;
		} in
		cff :: cffl
	}

anonymous_type_fields:
	| l = class_field+ { l }
	| l = anonymous_type_field { l }

complex_type:
	| POPEN; ct = complex_type; PCLOSE { CTParent ct,mk $startpos $endpos }
	| BROPEN; l = structural_extension+; cffl = anonymous_type_fields; BRCLOSE { CTExtend(l,cffl),mk $startpos $endpos }
	| BROPEN; l = anonymous_type_fields; BRCLOSE { CTAnonymous l,mk $startpos $endpos }
	| QUESTIONMARK; ct = complex_type; { CTOptional ct,mk $startpos $endpos }
	| tp = type_path { CTPath (fst tp),snd tp }
	| ct1 = complex_type; ARROW; ct2 = complex_type {
		match fst ct2 with
		| CTFunction (args,r) ->
			CTFunction (ct1 :: args,r),mk $startpos $endpos
		| _ ->
			CTFunction ([ct1],ct2),mk $startpos $endpos
	}

type_path_parameter:
	| BKOPEN; el = array_elements; BKCLOSE { TPExpr(EArrayDecl el,mk $startpos $endpos) }
	| ct = complex_type { TPType ct }
	| cst = literal { TPExpr (EConst cst,mk $startpos $endpos) }
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

class_field:
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

constraints:
	| COLON; POPEN; ct = complex_type; COMMA; ctl = separated_nonempty_list(COMMA,complex_type); PCLOSE { ct :: ctl }
	| COLON; ct = complex_type { [ct] }
	| { [] }

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

import_mode:
	| IN; ident = ident { IAsName ident }
	| AS; ident = ident { IAsName ident }
	| DOTSTAR { IAll }
	| { INormal }

import:
	| path = path_with_pos; mode = import_mode { EImport(path,mode),mk $startpos $endpos }

class_or_interface:
	| CLASS { [] }
	| INTERFACE { [HInterface] }

class_decl:
	| flags = class_or_interface; name = pos(dollar_ident)?; tl = type_decl_parameters; rl = class_relations*; BROPEN; l = class_field*; BRCLOSE { flags,name,tl,rl,l }

decl:
	| annotations; IMPORT; import = import; SEMICOLON { import }
	| annotations; USING; path = path_with_pos; SEMICOLON { EUsing path,mk $startpos $endpos }
	| annotation = annotations; flags = common_flags*; c = class_decl {
		let (flags2,name,tl,rl,l) = c in
		let def = {
			d_name = (match name with None -> $syntaxerror | Some name -> name);
			d_doc = fst annotation;
			d_params = tl;
			d_meta = snd annotation;
			d_flags = (List.map fst flags) @ flags2 @ rl;
			d_data = l;
		} in
		(EClass def,mk $startpos $endpos)
	}
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

(* Entry points *)

sharp_condition:
	| e = sharp_condition_both { e }

sharp_error_message:
	| s = STRING { s }

sharp_line_number:
	| i = INT { i }

expr_only:
	| expr = expr; EOF { expr }