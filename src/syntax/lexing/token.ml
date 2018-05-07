open Tokens

type placed_token = (Tokens.token * Lexing.position * Lexing.position)

type trivia_flag =
	| TFNormal
	| TFImplicit
	| TFInserted
	| TFSkipped
	| TFSplit of placed_token * placed_token

type token_info = placed_token * trivia

and trivia = {
	tleading: placed_token list;
	ttrailing: placed_token list ref;
	tflags : trivia_flag list;
}

type range = Lexing.position * Lexing.position

type work_token = placed_token * trivia_flag * int

let rec s_token tk = match tk with
	| COLON -> ":"
	| SEMICOLON -> ";"
	| COMMA -> ","
	| DOT -> "."
	| PACKAGE -> "package"
	| IDENT s -> s
	| DOLLAR_IDENT s -> "$" ^ s
	| DOT_IDENT s -> "." ^ s
	| STRING s -> "\"" ^ s ^ "\""
	| STRING2 s -> "'" ^ s ^ "'"
	| COMMENT s -> "/*" ^ s ^ "*/"
	| REGEX(s1,s2) -> "~/" ^ s1 ^ "/" ^ s2
	| COMMENTLINE s -> "//" ^ s
	| METADATA s -> "@" ^ s
	| METADATA_OPEN s -> "@" ^ s ^ "("
	| INT s -> s
	| FLOAT s -> s
	| SHARPIF -> "#if"
	| SHARPELSE -> "#else"
	| SHARPELSEIF -> "#elseif"
	| SHARPEND -> "#end"
	| SHARPERROR -> "#error"
	| SHARPLINE -> "#line"
	| BRCLOSE -> "}"
	| BROPEN -> "{"
	| POPEN -> "("
	| PCLOSE -> ")"
	| BKOPEN -> "["
	| BKCLOSE -> "]"
	| QUESTIONMARK -> "?"
	| ASSIGN -> "="
	| PLUS -> "+"
	| LT -> "<"
	| GT -> ">"
	| ASSIGNMOD -> "%="
	| ASSIGNAND -> "&="
	| ASSIGNOR -> "|="
	| ASSIGNXOR -> "^="
	| ASSIGNPLUS -> "+="
	| ASSIGNMINUS -> "-="
	| ASSIGNSTAR -> "*="
	| ASSIGNSLASH -> "/="
	| ASSIGNSHL -> "<<="
	| ASSIGNSHR -> ">>="
	| ASSIGNUSHR -> ">>>="
	| ASSIGNBOOLOR -> "||="
	| ASSIGNBOOLAND -> "&&="
	| INCREMENT -> "++"
	| DECREMENT -> "--"
	| TILDE -> "~"
	| XOR -> "^"
	| STAR -> "*"
	| SLASH -> "/"
	| SHL -> "<<"
	| SHR -> ">>"
	| USHR -> ">>>"
	| PERCENT -> "%"
	| OR -> "|"
	| NOTEQUALS -> "!="
	| MINUS -> "-"
	| LTE -> "<="
	| GTE -> ">="
	| INTERVAL -> "..."
	| EXCLAMATION -> "!"
	| EQUALS -> "=="
	| DOUBLEARROW -> "=>"
	| BOOLOR -> "||"
	| BOOLAND -> "&&"
	| ARROW -> "->"
	| AND -> "&"
	| FUNCTION -> "function"
	| CLASS -> "class"
	| INTERFACE -> "interface"
	| EXTENDS -> "extends"
	| IMPLEMENTS -> "implements"
	| VAR -> "var"
	| WHILE -> "while"
	| USING -> "using"
	| UNTYPED -> "untyped"
	| TYPEDEF -> "typedef"
	| TRY -> "try"
	| TRUE -> "true"
	| THROW -> "throw"
	| THIS -> "this"
	| SWITCH -> "switch"
	| STATIC -> "static"
	| RETURN -> "return"
	| PUBLIC -> "public"
	| PRIVATE -> "private"
	| OVERRIDE -> "override"
	| NULL -> "null"
	| NEW -> "new"
	| MACRO -> "macro"
	| INLINE -> "inline"
	| IN -> "in"
	| IMPORT -> "import"
	| IF -> "if"
	| FOR -> "for"
	| FALSE -> "false"
	| EXTERN -> "extern"
	| ENUM -> "enum"
	| ELSE -> "else"
	| DYNAMIC -> "dynamic"
	| DOTSTAR -> ".*"
	| DO -> "do"
	| DEFAULT -> "default"
	| CONTINUE -> "continue"
	| CATCH -> "catch"
	| CAST -> "cast"
	| CASE -> "case"
	| BREAK -> "break"
	| ABSTRACT -> "abstract"
	| FROM -> "from"
	| TO -> "to"
	| AS -> "as"
	| IS -> "is"
	| FINAL -> "final"
	| EOF -> "<eof>"
	| WHITESPACE s -> s
	| NEWLINE s -> s
	| NONSENSE s -> s
	| UNCLOSED tok -> s_token tok