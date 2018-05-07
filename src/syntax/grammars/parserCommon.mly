%{
	open Ops

	let mk = Pos.Range.make
%}

%%

(* Operators *)

%public op_assign:
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

%public op_compare:
	| EQUALS { OpEq }
	| NOTEQUALS { OpNotEq }
	| GT { OpGt }
	| LT { OpLt }
	| LTE { OpLte }
	| GTE { OpGte }

%public op_bit:
	| OR { OpOr }
	| AND { OpAnd }
	| XOR { OpXor }

%public %inline op:
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

%public unary_prefix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| TILDE { NegBits }
	| EXCLAMATION { Not }
	| MINUS { Neg }

%public unary_postfix:
	| INCREMENT { Increment }
	| DECREMENT { Decrement }
	| EXCLAMATION { Not }

(* Expression *)

%public ident:
	| s = IDENT { s }
	| FROM { "from" }
	| TO { "to" }
	| AS { "as" }
	| IS { "is" }
	| FINAL { "final" }

%public string:
	| s = STRING | s = STRING2 { s }

literal_string:
	| s = string { emit_literal_string s }

literal_int:
	| s = INT %prec NONDOT { emit_literal_int s }

literal_float:
	| s = FLOAT { emit_literal_float s }

literal_regex:
	| s = REGEX { emit_literal_regex s }

%public literal:
	| literal_string | literal_int | literal_float | literal_regex { $1 }

%public %inline const:
	| s = ident { emit_const_ident s }
	| s = literal { emit_const_literal s }

%public keyword_ident:
	| THIS { emit_this (mk $startpos $endpos) }
	| TRUE { emit_true (mk $startpos $endpos) }
	| FALSE { emit_false (mk $startpos $endpos) }
	| NULL { emit_null (mk $startpos $endpos) }