-module(lol_debug_backend).

-export([emit/1]).


emit(Forms) ->
    iolist_to_binary(
      [[emit_func(Form) || Form <- Forms]]).


emit_func({'fun', Name, ArgCount, VarCount, Instructions}) ->
    [io_lib:format(
       "int ~s(~s) {~n",
       [Name,
        string:join(
          [io_lib:format("int v~w", [N])
           || N <- lists:seq(1, ArgCount)],
          ", ")]),
     [io_lib:format("  int v~w;~n", [N])
      || N <- lists:seq(ArgCount+1, VarCount)],
     [emit_inst(Inst) || Inst <- Instructions],
     <<"}\n\n">>].

emit_inst({const, {int, N}, Var}) ->
    io_lib:format("  v~w = ~w;~n", [Var, N]);
emit_inst({move, Var1, Var2}) ->
    io_lib:format("  v~w = v~w;~n", [Var2, Var1]);
emit_inst({label, Label}) ->
    io_lib:format("l~w:~n", [Label]);
emit_inst({call, {Name, Args}, Var}) ->
    io_lib:format(
      "  v~w = ~s(~s);~n",
      [Var,
       Name,
       string:join([io_lib:format("v~p", [Arg]) || Arg <- Args], ", ")
      ]);
emit_inst({jmp, Label}) ->
    io_lib:format("  goto l~w;~n", [Label]);
emit_inst({jne, Var, Label}) ->
    io_lib:format("  if (!v~w) goto l~w;~n", [Var, Label]);
emit_inst({return, Var}) ->
    io_lib:format("  return v~w~n", [Var]);
emit_inst({match_fail, _Var}) ->
    [<<"  fprintf(stderr, \"value mismatch\\n\");\n  exit(1);\n">>].
