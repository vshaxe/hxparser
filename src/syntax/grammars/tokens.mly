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
%nonassoc NONDOT
%left DOT_IDENT COLON DOT

%%