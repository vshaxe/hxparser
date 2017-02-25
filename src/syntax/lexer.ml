(*
	The Haxe Compiler
	Copyright (C) 2005-2016  Haxe Foundation
	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.
	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.
	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *)

open Sedlex_menhir
open Parser

exception Unclosed of string

let ident_lc = [%sedlex.regexp?
	(
		Star '_',
		'a'..'z',
		Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')
	)
	|
	Plus '_'
	|
	(
		Plus '_',
		'0'..'9',
		Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')
	)
]

let ident_uc = [%sedlex.regexp? Star '_', 'A'..'Z', Star ('_' | 'a'..'z' | 'A'..'Z' | '0'..'9')]

let ident = [%sedlex.regexp? ident_lc | ident_uc ]

let integer = [%sedlex.regexp? ('1'..'9', Star ('0'..'9')) | '0']

let metadata_ident = [%sedlex.regexp? Opt ':',ident,Star ('.',ident) ]

let dollar_ident = [%sedlex.regexp? '$',Opt (ident | integer)]

let dot_ident = [%sedlex.regexp? '.',(dollar_ident | ident)]

let rec skip_header lexbuf =
	let buf = lexbuf.stream in
	match%sedlex buf with
	| 0xfeff -> skip_header lexbuf
	| "#!", Star (Compl ('\n' | '\r')) -> skip_header lexbuf
	| "" | eof -> ()
	| _ -> assert false

let skip1 s = String.sub s 1 (String.length s - 1)

let rec preprocessor lexbuf =
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof -> update lexbuf; EOF
	| "#if" -> update lexbuf; SHARPIF
	| "#else" -> update lexbuf; SHARPELSE
	| "#elseif" -> update lexbuf; SHARPELSEIF
	| "#end" -> update lexbuf; SHARPEND
	| "\n" | "\r\n" -> update lexbuf; new_line lexbuf; NEWLINE (lexeme lexbuf)
	| Plus (Compl ('#' | '\n' | '\r' | '/' | '"' | '~' | '\'')) -> update lexbuf; WHITESPACE (lexeme lexbuf)
	| "//", Star (Compl ('\n' | '\r')) -> update lexbuf; WHITESPACE (lexeme lexbuf)
	| '/' | '#' | '~' -> update lexbuf; WHITESPACE (lexeme lexbuf)
	| "\"" -> update lexbuf; WHITESPACE("\"" ^ (try string (Buffer.create 0) lexbuf with Unclosed s -> s) ^ "\"")
	| "\'" -> update lexbuf; WHITESPACE("'" ^ (try string2 (Buffer.create 0) lexbuf with Unclosed s -> s) ^ "'")
	| "/*" -> update lexbuf; WHITESPACE("/*" ^ (try comment (Buffer.create 0) lexbuf with Unclosed s -> s) ^ "*/")
	| "~/" ->
		update lexbuf;
		let s1,s2 = try regexp (Buffer.create 0) lexbuf with Unclosed s -> s,"" in
		WHITESPACE("~/" ^ s1 ^ "/" ^ s2)
	| any -> update lexbuf; NONSENSE (lexeme lexbuf)
	| _ -> assert false

and token lexbuf =
	let buf = lexbuf.stream in
	match%sedlex buf with
	(* whitespace *)
	| eof -> update lexbuf; EOF
	| Plus (Chars " \t\r") -> update lexbuf; WHITESPACE (lexeme lexbuf)
	| "\n" | "\r\n" -> update lexbuf; new_line lexbuf; NEWLINE (lexeme lexbuf)
	| "//", Star (Compl ('\n' | '\r')) ->
		let s = lexeme lexbuf in
		update lexbuf;
		COMMENTLINE (String.sub s 2 ((String.length s)-2))
	(* numbers *)
	| "0x", Plus ('0'..'9'|'a'..'f'|'A'..'F') -> update lexbuf; INT (lexeme lexbuf)
	| integer -> update lexbuf; INT (lexeme lexbuf)
	| integer, '.', Plus '0'..'9' -> update lexbuf; FLOAT (lexeme lexbuf)
	| '.', Plus '0'..'9' -> update lexbuf; FLOAT (lexeme lexbuf)
	| integer, ('e'|'E'), Opt ('+'|'-'), Plus '0'..'9' -> update lexbuf; FLOAT (lexeme lexbuf)
	| integer, '.', Star '0'..'9', ('e'|'E'), Opt ('+'|'-'), Plus '0'..'9' -> update lexbuf; FLOAT (lexeme lexbuf)
	(* keywords *)
	| "package" -> update lexbuf; PACKAGE
	| "import" -> update lexbuf; IMPORT
	| "using" -> update lexbuf; USING
	| "class" ->  update lexbuf; CLASS
	| "interface" -> update lexbuf; INTERFACE
	| "enum" -> update lexbuf; ENUM
	| "typedef" -> update lexbuf; TYPEDEF
	| "abstract" -> update lexbuf; ABSTRACT
	| "private" -> update lexbuf; PRIVATE
	| "public" -> update lexbuf; PUBLIC
	| "static" -> update lexbuf; STATIC
	| "inline" -> update lexbuf; INLINE
	| "extern" -> update lexbuf; EXTERN
	| "macro" -> update lexbuf; MACRO
	| "override" -> update lexbuf; OVERRIDE
	| "dynamic" -> update lexbuf; DYNAMIC
	| "function" -> update lexbuf; FUNCTION
	| "extends" -> update lexbuf; EXTENDS
	| "implements" -> update lexbuf; IMPLEMENTS
	| "var" -> update lexbuf; VAR
	| "cast" -> update lexbuf; CAST
	| "throw" -> update lexbuf; THROW
	| "new" -> update lexbuf; NEW
	| "if" -> update lexbuf; IF
	| "else" -> update lexbuf; ELSE
	| "while" -> update lexbuf; WHILE
	| "do" -> update lexbuf; DO
	| "for" -> update lexbuf; FOR
	| "try" -> update lexbuf; TRY
	| "catch" -> update lexbuf; CATCH
	| "return" -> update lexbuf; RETURN
	| "break" -> update lexbuf; BREAK
	| "continue" -> update lexbuf; CONTINUE
	| "switch" -> update lexbuf; SWITCH
	| "case" -> update lexbuf; CASE
	| "default" -> update lexbuf; DEFAULT
	| "untyped" -> update lexbuf; UNTYPED
	| "this" -> update lexbuf; THIS
	| "true" -> update lexbuf; TRUE
	| "false" -> update lexbuf; FALSE
	| "null" -> update lexbuf; NULL
	| "in" -> update lexbuf; IN
	| "from" -> update lexbuf; FROM
	| "to" -> update lexbuf; TO
	| "as" -> update lexbuf; AS
	| "is" -> update lexbuf; IS
	(* punctuation *)
	| ":" -> update lexbuf; COLON
	| ";" -> update lexbuf; SEMICOLON
	| "," -> update lexbuf; COMMA
	| "{" -> update lexbuf; BROPEN
	| "}" -> update lexbuf; BRCLOSE
	| "(" -> update lexbuf; POPEN
	| ")" -> update lexbuf; PCLOSE
	| "[" -> update lexbuf; BKOPEN
	| "]" -> update lexbuf; BKCLOSE
	| "?" -> update lexbuf; QUESTIONMARK
	| ".*" -> update lexbuf; DOTSTAR
	| "." -> update lexbuf; DOT
	(* ops *)
	| "++" -> update lexbuf; INCREMENT
	| "--" -> update lexbuf; DECREMENT
	| "~"  -> update lexbuf; TILDE
	| "%=" -> update lexbuf; ASSIGNMOD
	| "&=" -> update lexbuf; ASSIGNAND
	| "|=" -> update lexbuf; ASSIGNOR
	| "^=" -> update lexbuf; ASSIGNXOR
	| "+=" -> update lexbuf; ASSIGNPLUS
	| "-=" -> update lexbuf; ASSIGNMINUS
	| "*=" -> update lexbuf; ASSIGNSTAR
	| "/=" -> update lexbuf; ASSIGNSLASH
	| "<<=" -> update lexbuf; ASSIGNSHL
	| "||=" -> update lexbuf; ASSIGNBOOLOR
	| "&&=" -> update lexbuf; ASSIGNBOOLAND
	| "==" -> update lexbuf; EQUALS
	| "!=" -> update lexbuf; NOTEQUALS
	| "<=" -> update lexbuf; LTE
	| "<" -> update lexbuf; LT
	| ">" -> update lexbuf; GT
	| "&&" -> update lexbuf; BOOLAND
	| "||" -> update lexbuf; BOOLOR
	| "<<" -> update lexbuf; SHL
	| "->" -> update lexbuf; ARROW
	| "..." -> update lexbuf; INTERVAL
	| "=>" -> update lexbuf; DOUBLEARROW
	| "!" -> update lexbuf; EXCLAMATION
	| "%" -> update lexbuf; PERCENT
	| "&" -> update lexbuf; AND
	| "|" -> update lexbuf; OR
	| "^" -> update lexbuf; XOR
	| "=" -> update lexbuf; ASSIGN
	| "+" -> update lexbuf; PLUS
	| "-" -> update lexbuf; MINUS
	| "*" -> update lexbuf; STAR
	| "/" -> update lexbuf; SLASH
	(* sequences *)
	| "\"" -> (try STRING (string (Buffer.create 0) lexbuf) with Unclosed s -> UNCLOSED (STRING s));
	| "\'" -> (try STRING2 (string2 (Buffer.create 0) lexbuf) with Unclosed s -> UNCLOSED (STRING2 s));
	| "/*" -> (try COMMENT (comment (Buffer.create 0) lexbuf) with Unclosed s -> UNCLOSED (COMMENT s));
	| "~/" -> (try REGEX (regexp (Buffer.create 0) lexbuf) with Unclosed s -> REGEX(s,""))
	| '@',metadata_ident -> update lexbuf; METADATA (skip1 (lexeme lexbuf))
	| '@',metadata_ident,"(" -> update lexbuf; METADATA_OPEN (let s = lexeme lexbuf in String.sub s 1 (String.length s - 2))
	| ident -> update lexbuf; IDENT (lexeme lexbuf)
	| dollar_ident -> update lexbuf; DOLLAR_IDENT (skip1 (lexeme lexbuf))
	| dot_ident -> update lexbuf; DOT_IDENT (skip1 (lexeme lexbuf))
	(* preprocessor *)
	| "#if" -> update lexbuf; SHARPIF
	| "#else" -> update lexbuf; SHARPELSE
	| "#elseif" -> update lexbuf; SHARPELSEIF
	| "#end" -> update lexbuf; SHARPEND
	| "#error" -> update lexbuf; SHARPERROR
	| "#line" -> update lexbuf; SHARPLINE
	| any -> update lexbuf; NONSENSE (lexeme lexbuf)
	| _ -> assert false (* should not happen *)

and string buffer lexbuf =
	let store () = Buffer.add_string buffer (lexeme lexbuf) in
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof -> raise (Unclosed (Buffer.contents buffer));
	| '\n' | '\r' | "\r\n" -> new_line lexbuf; store(); string buffer lexbuf
	| "\\\"" -> store(); string buffer lexbuf
	| "\\\\" -> store(); string buffer lexbuf
	| '\\' -> store(); string buffer lexbuf
	| '"' -> update lexbuf; Buffer.contents buffer
	| Plus (Compl ('"' | '\\' | '\r' | '\n')) -> store(); string buffer lexbuf
	| _ -> assert false

and string2 buffer lexbuf =
	let store () = Buffer.add_string buffer (lexeme lexbuf) in
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof -> raise (Unclosed (Buffer.contents buffer));
	| '\n' | '\r' | "\r\n" -> new_line lexbuf; store(); string2 buffer lexbuf
	| '\\' -> store(); string2 buffer lexbuf
	| "\\\\" -> store(); string2 buffer lexbuf
	| "\\'" -> store(); string2 buffer lexbuf
	| "'" -> update lexbuf; Buffer.contents buffer
	| "$$" | "\\$" | '$' -> store(); string2 buffer lexbuf
	| "${" ->
		store();
		let s = code_string (Buffer.create 0) lexbuf in
		Buffer.add_string buffer s;
		string2 buffer lexbuf
	| Plus (Compl ('\'' | '\\' | '\r' | '\n' | '$')) -> store(); string2 buffer lexbuf
	| _ -> assert false

and code_string buffer lexbuf =
	let add s = Buffer.add_string buffer s in
	let store () = add (lexeme lexbuf) in
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof -> raise (Unclosed (Buffer.contents buffer));
	| '\n' | '\r' | "\r\n" -> new_line lexbuf; store(); code_string buffer lexbuf
	| '{' | '/' -> store(); code_string buffer lexbuf
	| '}' ->
		store();
		Buffer.contents buffer;
	| '"' ->
		add "\"";
		let s = (try string (Buffer.create 0) lexbuf with Unclosed s -> s in
		add s;
		add "\"";
		code_string buffer lexbuf
	| "'" ->
		add "'";
		let s = (try string2 (Buffer.create 0) lexbuf with Unclosed s -> s in
		add s;
		add "'";
		code_string buffer lexbuf
	| "/*" ->
		let s = (try string (Buffer.create 0) lexbuf with Unclosed s -> s in
		add s;
		code_string buffer lexbuf
	| "//", Star (Compl ('\n' | '\r')) -> store(); code_string buffer lexbuf
	| Plus (Compl ('/' | '"' | '\'' | '{' | '}' | '\n' | '\r')) -> store(); code_string buffer lexbuf
	| _ -> assert false

and comment buffer lexbuf =
	let store () = Buffer.add_string buffer (lexeme lexbuf) in
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof -> raise (Unclosed (Buffer.contents buffer));
	| '\n' | '\r' | "\r\n" -> new_line lexbuf; store(); comment buffer lexbuf
	| "*/" -> update lexbuf; Buffer.contents buffer
	| '*' -> store(); comment buffer lexbuf
	| Plus (Compl ('*' | '\n' | '\r')) -> store(); comment buffer lexbuf
	| _ -> assert false

and regexp buffer lexbuf =
	let add s = Buffer.add_string buffer s in
	let store () = add (lexeme lexbuf) in
	let buf = lexbuf.stream in
	match%sedlex buf with
	| eof | '\n' | '\r' -> raise (Unclosed (Buffer.contents buffer));
	| '\\', '/' -> add "/"; regexp buffer lexbuf
	| '\\', 'r' -> add "\r"; regexp buffer lexbuf
	| '\\', 'n' -> add "\n"; regexp buffer lexbuf
	| '\\', 't' -> add "\t"; regexp buffer lexbuf
	| '\\', ('\\' | '$' | '.' | '*' | '+' | '^' | '|' | '{' | '}' | '[' | ']' | '(' | ')' | '?' | '-' | '0'..'9') -> store(); regexp buffer lexbuf
	| '\\', ('w' | 'W' | 'b' | 'B' | 's' | 'S' | 'd' | 'D' | 'x') -> store(); regexp buffer lexbuf
	| '\\', ('u' | 'U'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F'), ('0'..'9' | 'a'..'f' | 'A'..'F') -> store(); regexp buffer lexbuf
	(*| '\\', Compl '\\' -> error (Invalid_character (lexeme_char lexbuf 0)) (lexeme_end lexbuf - 1)*)
	| '/' -> Buffer.contents buffer,regexp_options lexbuf
	| Plus (Compl ('\\' | '/' | '\r' | '\n')) -> store(); regexp buffer lexbuf
	| _ -> assert false

and regexp_options lexbuf =
	let buf = lexbuf.stream in
	match%sedlex buf with
	| 'g' | 'i' | 'm' | 's' | 'u' ->
		let l = lexeme lexbuf in
		l ^ regexp_options lexbuf
	(*| 'a'..'z' -> error Invalid_option (lexeme_start lexbuf)*)
	| "" -> ""
	| _ -> assert false