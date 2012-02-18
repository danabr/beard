%%% @copyright Daniel Abrahamsson 2012
%%% @doc Module for emitting Erlang code for rendering a template.
-module(beard_emit).
-export([emit/1]).

emit(Template) ->
    Funs = extract_functions(Template),
    emit_functions(Funs).
    
extract_functions(Template) ->
    ef1([{"render", Template}], []).

ef1([], Funs) ->
    Funs;
ef1([{Name, Template}|Stack], Funs) ->
    {Fun, CalledFuns} = ef2(Name, Template),
    ef1(CalledFuns ++ Stack, [{Name, Fun}|Funs]). 

ef2(Name, Template) ->
    ef2(Name, Template, [], [], 0).

ef2(_, [], SubTemplate, CalledFuns, _Index) ->
    {SubTemplate, CalledFuns};
ef2(Name, [{context, Func, SubTemplate}|Template], Sub_Template, CalledFuns,
    Index) ->
    NewName = Name ++ "_" ++ integer_to_list(Index),
    ef2(Name, Template, Sub_Template ++ [{call, NewName, Func}],
        [{NewName, SubTemplate}|CalledFuns], Index+1);
ef2(Name, [Command|SubTemplate], Sub_Template, CalledFuns, Index) ->
    ef2(Name, SubTemplate, Sub_Template ++ [Command], CalledFuns, Index).

emit_functions([]) ->
    "";
emit_functions([{"render", Template}|T]) ->
    ["render(Context) ->\n", emit_template(Template),
     emit_functions(T), "."];
emit_functions([{Name, Template}|T]) ->
    [emit_list(Name, Template), ".\n", emit_functions(T)].

emit_list(Name, Template) ->
    [Name, "([]) -> \"\";\n",
     Name, "([Context|T]) ->\n",
     "[", emit_template(Template), "|", Name, "(T)]"].

emit_template(Template) ->
    ["[", string:join(emit_template_body(Template, ""), ","), "]"].

emit_template_body([], Operations) -> lists:reverse(Operations);
emit_template_body([{data, Data}|T], Acc) ->
    emit_template_body(T, [["\"", escape(Data), "\""]|Acc]);
emit_template_body([{call, Func}|T], Acc) ->
    emit_template_body(T, [[atom_to_list(Func), "(Context)"]|Acc]);
emit_template_body([{call, Func, ContextFunc}|T], Acc) ->
    emit_template_body(T, [[Func, "(", atom_to_list(ContextFunc),
                            "(Context))"]|Acc]).

escape([]) ->
    [];
escape([$\n|T]) ->
    [$\\,$n|escape(T)];
escape([$\\|T]) ->
    [$\\, $\\|emit(T)];
escape([$\"|T]) ->
    [$\\, $\"|emit(T)];
escape([H|T]) ->
    [H|escape(T)].
