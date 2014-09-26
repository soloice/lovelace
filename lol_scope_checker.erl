-module(lol_scope_checker).

-export([check_forms/1]).


-record(func_scope, {var_count, label_count, var_names}).


op_to_name('+') ->
    plus;
op_to_name('-') ->
    minus.


check_forms(Forms) ->
    lists:map(fun check_form/1, Forms).


check_form({'fun', {atom, _, Name}, Args, Exprs}) ->
    VarNames =
        lists:zip(
          [VarName || {var, _, VarName} <- Args],
          lists:seq(1, length(Args))),

    Scope =
        #func_scope{
           var_count=length(Args),
           label_count=0,
           var_names=VarNames},

    {Results, Scope1} =
        lists:mapfoldl(fun check_expression/2, Scope, Exprs),
    {Insts, Values} = lists:unzip(Results),
    Value = lists:last(Values),

    {'fun', Name, length(Args), Scope1#func_scope.var_count, lists:append(Insts) ++ [{return, Value}]}.


check_expression({int, _, Int}, Scope = #func_scope{var_count=VarCount}) ->
    ConstVar = VarCount + 1,
    {{[{const, {int, Int}, ConstVar}], ConstVar}, Scope#func_scope{var_count=VarCount+1}};
check_expression({var, _, VarName}, Scope = #func_scope{var_names=VarNames}) ->
    {VarName, Var} = proplists:lookup(VarName, VarNames),
    {{[], Var}, Scope};
check_expression({'call', {atom, _, Name}, Args}, Scope) ->
    {Results, Scope1} =
        lists:mapfoldl(fun check_expression/2, Scope, Args),
    {Insts, Values} = lists:unzip(Results),
    ResultVar = Scope1#func_scope.var_count + 1,
    {{lists:append(Insts) ++ [{call, {Name, Values}, ResultVar}], ResultVar}, Scope1#func_scope{var_count=ResultVar}};
check_expression({'call', {Op, _}, Args}, Scope) ->
    {Results, Scope1} =
        lists:mapfoldl(fun check_expression/2, Scope, Args),
    {Insts, Values} = lists:unzip(Results),

    ResultVar = Scope1#func_scope.var_count + 1,
    {{lists:append(Insts) ++ [{call, {op_to_name(Op), Values}, ResultVar}], ResultVar}, Scope1#func_scope{var_count=ResultVar}};
check_expression({'case', {var, _, VarName}, Clauses}, Scope=#func_scope{var_count=VarCount, label_count=LabelCount, var_names=VarNames}) ->
    {VarName, MatchVar} = proplists:lookup(VarName, VarNames),
    ResultVar = VarCount + 1,
    NextLabel = LabelCount + 1,
    LastLabel = LabelCount + 2,

    {Insts, {MatchVar, ResultVar, _NextLabel1, LastLabel, Scope1}} =
         lists:mapfoldl(
           fun check_case_clause/2,
           {MatchVar, ResultVar, NextLabel, LastLabel, Scope#func_scope{var_count=VarCount+1, label_count=LabelCount+2}},
           Clauses),

    {{lists:append(Insts) ++ [{match_fail, MatchVar}, {label, LastLabel}], ResultVar}, Scope1}.


check_case_clause({MatchSpecs, Expressions}, {MatchVar, ResultVar, NextLabel, LastLabel, Scope}) ->
    {Insts1, {MatchVar, NextLabel, Scope1}} =
        lists:mapfoldl(
          fun check_matchspec/2,
          {MatchVar, NextLabel, Scope},
          MatchSpecs),

    {Results, Scope2} =
        lists:mapfoldl(fun check_expression/2, Scope1, Expressions),

    {Insts2, Values} = lists:unzip(Results),
    Value = lists:last(Values),

    NextLabel1 = Scope2#func_scope.label_count + 1,
    Scope3 = Scope2#func_scope{label_count=NextLabel1},

    {lists:append(Insts1) ++ lists:append(Insts2) ++ [{move, Value, ResultVar}, {jmp, LastLabel}, {label, NextLabel}],
     {MatchVar, ResultVar, NextLabel1, LastLabel, Scope3#func_scope{var_names=Scope#func_scope.var_names}}}.


check_matchspec({int, _, N}, {MatchVar, NextLabel, Scope = #func_scope{var_count=VarCount}}) ->
    ConstVar = VarCount + 1,
    TestVar = VarCount + 2,
    Insts =
        [ {const, {int, N}, ConstVar},
          {call, {eq, [MatchVar, ConstVar]}, TestVar},
          {jne, TestVar, NextLabel}
        ],
    {Insts, {MatchVar, NextLabel, Scope#func_scope{var_count=VarCount+2}}};
check_matchspec({ignore, _, _}, {MatchVar, NextLabel, Scope}) ->
    {[], {MatchVar, NextLabel, Scope}}.
