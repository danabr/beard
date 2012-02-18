%%% @copyright Daniel Abrahamsson 2012
%%% @doc Module for rendering a template without compiling it to a module.
-module(beard_basic_renderer).

-export([render/3]).

render([], _Module, _Context) ->
    [];
render([{data, Data}|Template], Module, Context) ->
    [Data|render(Template, Module, Context)];
render([{call, Func}|Template], Module, Context) ->
    Data = Module:Func(Context),
    [Data|render(Template, Module, Context)];
render([{context, Func, SubTemplate}|Template], Module, Context) ->
    Data = case Module:Func(Context) of
        true ->
            render(SubTemplate, Module, Context);
        false ->
            "";
        List when is_list(List) ->
            [render(SubTemplate, Module, SubContext) || SubContext <- List]
    end,
    [Data|render(Template, Module, Context)].

  
