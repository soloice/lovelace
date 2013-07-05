%{
  open Template

  let parse_error = print_endline;;


  let make_pos n =
    let start_pos = Parsing.rhs_start_pos 1 in
      {
        lineno=start_pos.Lexing.pos_lnum;
        column=start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1
      }
  ;;

  let make_void pos = NameParam(("void", []), pos);;
%}


%token EOF

%token IGNORE
%token <string> ATOM
%token <string> VAR
%token <string> STRING
%token <string> CHAR

%token USE

%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE

%token PLUS MINUS TIMES DIVIDE

%token MATCH

%token EEQ ENE

%token EQ NE LE GE LT GT

%token COMMA SEMICOLON COLON DOT HASH

%token LARR
%token RARR

%token CASE END ENUM FUN IF IS OF RECORD TYPE WHEN


%right LARR
%right MATCH
%nonassoc EQ NE LE GE LT GT EEQ ENE
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Template.form list> program

%%



program:
| form_list EOF
    { $1 }
;


form_list:
|
    { [] }
| form_list form
    { $1 @ [$2] }
;


form:
| USE STRING
    { UseForm($2, (make_pos 1)) }
| TYPE typename IS typedef
    { TypeForm($2, $4, (make_pos 1)) }
| FUN ATOM typespec_list RARR typeparam IS clause_list END
    { FunForm($2, $3, $5, [], $7, (make_pos 1)) }
| FUN ATOM typespec_list WHEN expression_list RARR typeparam IS clause_list END
    { FunForm($2, $3, $7, $5, $9, (make_pos 1)) }
| FUN ATOM typespec_list IS clause_list END
    { FunForm($2, $3, (make_void (make_pos 1)), [], $5, (make_pos 1)) }
| FUN ATOM typespec_list WHEN expression_list IS clause_list END
    { FunForm($2, $3, (make_void (make_pos 1)), $5, $7, (make_pos 1)) }
;


typename:
| ATOM
    { ($1, []) }
| ATOM LBRACKET typeparam_list RBRACKET
    { ($1, $3) }
;


typeparam_list:
| typeparam
    { [$1] }
| typeparam_list COMMA typeparam
    { $1 @ [$3] }
;


typeparam:
| IGNORE
    { IgnoreParam((make_pos 1)) }
| VAR
    { VarParam($1, (make_pos 1)) }
| typename
    { NameParam($1, (make_pos 1)) }
;

typedef:
| typename
    { AliasType($1, (make_pos 1)) }
| ENUM enum_list END
    { EnumType($2, (make_pos 1)) }
| RECORD record_list END
    { RecordType($2, (make_pos 1)) }
;


record_list:
| record
    { [$1] }
| record_list COMMA record
    { $1 @ [$3] }
;


record:
| ATOM COLON typeparam
    { ($1, $3, (make_pos 1)) }
;


enum_list:
| enum
    { [$1] }
| enum_list SEMICOLON enum
    { $1 @ [$3] }
;


enum:
| ATOM
    { ($1, [], (make_pos 1)) }
| ATOM LBRACE typeparam_list RBRACE
    { ($1, $3, (make_pos 1)) }
;


typespec_list:
| LBRACKET RBRACKET
    { [] }
| LBRACKET typeparam_list RBRACKET
    { $2 }
;


clause_list:
| clause
    { [$1] }
| clause_list SEMICOLON clause
    { $1 @ [$3] }
;


clause:
| LPAREN RPAREN RARR expression_list
    { ([], [], $4, (make_pos 1)) }
| LPAREN expression_list RPAREN RARR expression_list
    { ($2, [], $5, (make_pos 1)) }
| LPAREN RPAREN WHEN expression_list RARR expression_list
    { ([], $4, $6, (make_pos 1)) }
| LPAREN expression_list RPAREN WHEN expression_list RARR expression_list
    { ($2, $5, $7, (make_pos 1)) }
;


expression_list:
| expression
    { [$1] }
| expression_list COMMA expression
    { $1 @ [$3] }
;


expression:
| prefixexp
    { $1 }

| IGNORE
    { IgnoreExpr (make_pos 1) }
| STRING
    { StringExpr($1, (make_pos 1)) }
| ATOM
    { EnumExpr($1, None, [], (make_pos 1)) }
| ATOM HASH typeparam
    { EnumExpr($1, Some($3), [], (make_pos 1)) }
| ATOM LBRACE expression_list RBRACE
    { EnumExpr($1, None, $3, (make_pos 1)) }
| ATOM HASH typeparam LBRACE expression_list RBRACE
    { EnumExpr($1, Some($3), $5, (make_pos 1)) }

| FUN typespec_list IS clause_list END
    { FunExpr(None, $2, (make_void (make_pos 1)), [], $4, (make_pos 1)) }
| FUN typespec_list WHEN expression_list IS clause_list END
    { FunExpr(None, $2, (make_void (make_pos 1)), $4, $6, (make_pos 1)) }
| FUN VAR typespec_list IS clause_list END
    { FunExpr(Some($2), $3, (make_void (make_pos 1)), [], $5, (make_pos 1)) }
| FUN VAR typespec_list WHEN expression_list IS clause_list END
    { FunExpr(Some($2), $3, (make_void (make_pos 1)), $5, $7, (make_pos 1)) }

| FUN typespec_list RARR typeparam IS clause_list END
    { FunExpr(None, $2, $4, [], $6, (make_pos 1)) }
| FUN typespec_list WHEN expression_list RARR typeparam IS clause_list END
    { FunExpr(None, $2, $6, $4, $8, (make_pos 1)) }
| FUN VAR typespec_list RARR typeparam IS clause_list END
    { FunExpr(Some($2), $3, $5, [], $7, (make_pos 1)) }
| FUN VAR typespec_list WHEN expression_list RARR typeparam IS clause_list END
    { FunExpr(Some($2), $3, $7, $5, $9, (make_pos 1)) }

| FUN ATOM typespec_list END
    { FunRefExpr($2, $3, (make_pos 1)) }

| ATOM LPAREN RPAREN
    { CallNameExpr($1, [], (make_pos 1)) }

| ATOM LPAREN expression_list RPAREN
    { CallNameExpr($1, $3, (make_pos 1)) }

| IF branch_list END
    { IfExpr($2, (make_pos 1)) }
| CASE expression OF case_list END
    { CaseExpr($2, $4, (make_pos 1)) }

| HASH LBRACE field_list RBRACE
    { NewRecordExpr(None, $3, (make_pos 1)) }
| HASH typeparam LBRACE field_list RBRACE
    { NewRecordExpr(Some($2), $4, (make_pos 1)) }

| expression MATCH expression
    { BinOpExpr(Match, $1, $3, (make_pos 1)) }

| expression EEQ expression
    { BinOpExpr(ExactEqual, $1, $3, (make_pos 1)) }
| expression ENE expression
    { BinOpExpr(NotExactEqual, $1, $3, (make_pos 1)) }

| expression LT expression
    { BinOpExpr(LessThan, $1, $3, (make_pos 1)) }
| expression GT expression
    { BinOpExpr(GreaterThan, $1, $3, (make_pos 1)) }
| expression LE expression
    { BinOpExpr(LessOrEqual, $1, $3, (make_pos 1)) }
| expression GE expression
    { BinOpExpr(GreaterOrEqual, $1, $3, (make_pos 1)) }

| expression LARR expression
    { BinOpExpr(Assign, $1, $3, (make_pos 1)) }

| expression PLUS expression
    { BinOpExpr(Plus, $1, $3, (make_pos 1)) }
| expression MINUS expression
    { BinOpExpr(Minus, $1, $3, (make_pos 1)) }
| expression TIMES expression
    { BinOpExpr(Times, $1, $3, (make_pos 1)) }
| expression DIVIDE expression
    { BinOpExpr(Divide, $1, $3, (make_pos 1)) }
;


field_list:
| field
    { [$1] }
| field_list COMMA field
    { $1 @ [$3] }
;


field:
| ATOM MATCH expression
    { ($1, $3) }
;


branch_list:
| branch
    { [$1] }
| branch_list SEMICOLON branch
    { $1 @ [$3] }
;


branch:
| expression_list RARR expression_list
    { ($1, $3) }
;


case_list:
| case
    { [$1] }
| case_list SEMICOLON case
    { $1 @ [$3] }
;


case:
| expression RARR expression_list
    { ($1, [], $3) }
| expression WHEN expression_list RARR expression_list
    { ($1, $3, $5) }
;


prefixexp:
| VAR
    { VarExpr($1, (make_pos 1)) }
| LPAREN expression RPAREN
    { $2 }
| prefixexp DOT ATOM
    { AttribExpr($1, $3, (make_pos 1)) }
| prefixexp HASH LBRACE field_list RBRACE
    { DeriveRecordExpr($1, $4, (make_pos 1)) }
| prefixexp LPAREN RPAREN
    { CallValueExpr($1, [], (make_pos 1)) }
| prefixexp LPAREN expression_list RPAREN
    { CallValueExpr($1, $3, (make_pos 1)) }
;
