{
  open Parser

  let incr_lnum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {
      pos with
	Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

  let keywords = [
    ("case",   CASE);
    ("end",    END);
    ("enum",   ENUM);
    ("fun",    FUN);
    ("if",     IF);
    ("is",     IS);
    ("of",     OF);
    ("record", RECORD);
    ("type",   TYPE);
    ("use",    USE);
    ("when",   WHEN);
  ]

}


let atom = ['a'-'z']['_' 'a'-'z' '0'-'9']*
let var = ['A'-'Z']['_' 'A'-'Z' 'a'-'z' '0'-'9']*


rule token = parse
  | eof
      { EOF }
  | '\n'
      {
        incr_lnum lexbuf;
	token lexbuf
      }
  | [' ' '\t']+
      { token lexbuf }
  | '_'
      { IGNORE }
  | atom as id
      {
        match List.mem_assoc id keywords with
          | true ->
            List.assoc id keywords
          | false ->
            ATOM id
      }
  | var as id
      { VAR id }
  | "--"
      { comment lexbuf }
  | "<<\""
      { string (Lexing.lexeme_end lexbuf) lexbuf }

  | "'" [^ '\\' '\n'] "'"
      {
        let s = Lexing.lexeme_start lexbuf
        and buffer = lexbuf.Lexing.lex_buffer in
        CHAR (String.sub buffer (s + 1) 1)
      }
  | "'\\" [^ '\n'] "'"
      {
        let s = Lexing.lexeme_start lexbuf
        and buffer = lexbuf.Lexing.lex_buffer in
        CHAR (String.sub buffer (s + 1) 2)
      }

  | "<-"
      { LARR }
  | "->"
      { RARR }

  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | '*'
      { TIMES }
  | '/'
      { DIVIDE }

  | '='
      { MATCH }
  | "=:="
      { EEQ }
  | "=/="
      { ENE }
  | "=="
      { EQ }
  | "/="
      { NE }
  | "=<"
      { LE }
  | ">="
      { GE }
  | '<'
      { LT }
  | '>'
      { GT }

  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | ','
      { COMMA }
  | ';'
      { SEMICOLON }
  | ':'
      { COLON }
  | '.'
      { DOT }
  | '#'
      { HASH }

and string start = parse
  | "\">>"
      {
        let e = Lexing.lexeme_start lexbuf
        and buffer = lexbuf.Lexing.lex_buffer in
        STRING (String.sub buffer start (e - start))
      }
  | [^'"' '\\']+
      { string start lexbuf }
  | '\\' _
      { string start lexbuf }
  | '"'
      { string start lexbuf }

and comment = parse
  | "[" '='* '['
      {
        let s = Lexing.lexeme_start lexbuf
        and e = Lexing.lexeme_end lexbuf in
        long_comment (e - s - 2) lexbuf
      }
  | [^ '\n']
      { short_comment lexbuf }
  | '\n'
      {
        incr_lnum lexbuf;
        token lexbuf
      }

and short_comment = parse
  | [^ '\n']*
      { token lexbuf }

and long_comment level = parse
  | ']' '='* "]--"
      {
        let s = Lexing.lexeme_start lexbuf
        and e = Lexing.lexeme_end lexbuf in
        if (e - s - 4) = level then
            token lexbuf
        else
            long_comment level lexbuf
      }
  | '\n'
      {
        incr_lnum lexbuf;
	long_comment level lexbuf
      }
  | [^ ']' '\n']+
      { long_comment level lexbuf }
  | ']'
      { long_comment level lexbuf }
