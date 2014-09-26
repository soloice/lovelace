Definitions.

Atom   = [a-z][0-9a-zA-Z_]*
Var    = [A-Z][0-9a-zA-Z_]*
Ignore = _[0-9a-zA-Z_]*
WS     = [\000-\s]+
Sym    = [(),;+-=]
Int    = [0-9]+
Key    = (case|of|fun|end|->)

Rules.

{Int}    : {token, {int, TokenLine, list_to_integer(TokenChars)}}.
{Key}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{Atom}   : {token, {atom, TokenLine, list_to_atom(TokenChars)}}.
{Var}    : {token, {var, TokenLine, list_to_atom(TokenChars)}}.
{Ignore} : {token, {ignore, TokenLine, list_to_atom(TokenChars)}}.
{Sym}    : {token, {list_to_atom(TokenChars), TokenLine}}.
{WS}     : skip_token.


Erlang code.