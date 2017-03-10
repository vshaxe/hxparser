%{
	open AstPre

	let emit_null p = EConst (Ident "null"),p
	let emit_true p = EConst (Ident "true"),p
	let emit_false p = EConst (Ident "false"),p
	let emit_this p = EConst (Ident "this"),p
	let emit_literal_string s = String s
	let emit_literal_regex (s1,s2) = Regexp(s1,s2)
	let emit_literal_int s = Int s
	let emit_literal_float s = Float s
	let emit_const_ident s = Ident s
	let emit_const_literal lit = lit
	let emit_const_expr e p = EConst e,p
	let emit_unary_prefix_expr op e1 p = EUnop(op,Prefix,e1),p
	let emit_binop_expr e1 op e2 p = EBinop(op,e1,e2),p
%}

%start <AstPre.expr> sharp_condition
%start <string> sharp_error_message
%start <string> sharp_line_number

%%

sharp_condition_both:
	| e = const { emit_const_expr e (mk $startpos $endpos) }
	| MACRO { emit_const_expr (emit_const_ident "macro") (mk $startpos $endpos) }
	| e1 = keyword_ident { e1 }
	| POPEN; e = sharp_condition_any; PCLOSE { e }
	| op = unary_prefix; e1 = sharp_condition { emit_unary_prefix_expr op e1 (mk $startpos $endpos) }

sharp_condition_any:
	| e = sharp_condition_both { e }
	| e1 = sharp_condition_any; op = op; e2 = sharp_condition_any { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = sharp_condition_any; op = op_assign; e2 = sharp_condition_any %prec ASSIGN { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = sharp_condition_any; op = op_bit; e2 = sharp_condition_any %prec OR { emit_binop_expr e1 op e2 (mk $startpos $endpos) }
	| e1 = sharp_condition_any; op = op_compare; e2 = sharp_condition_any %prec EQUALS { emit_binop_expr e1 op e2 (mk $startpos $endpos) }

sharp_condition:
	| e = sharp_condition_both { e }

sharp_error_message:
	| s = string { s }

sharp_line_number:
	| i = INT { i }
