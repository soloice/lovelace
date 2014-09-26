-module(lol_test).

-export([test/0]).


test() ->
    {ok, Bin} = file:read_file("test.lol"),
    Str = binary_to_list(Bin),
    {ok, Tokens, _} = lol_lexer:string(Str),
    {ok, Forms} = lol_parser:parse(Tokens),

    IR = lol_scope_checker:check_forms(Forms),
    io:format("~s~n", [lol_debug_backend:emit(IR)]).

