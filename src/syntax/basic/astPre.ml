open Ops

type pos = Pos.Range.t

type expr_def =
	| EConst of constant
	| EUnop of unop * unop_flag * expr
	| EBinop of binop * expr * expr

and expr = expr_def * pos