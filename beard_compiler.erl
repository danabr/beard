%%% @copyright Daniel Abrahamsson 2012
%%% @doc Module for compiling a template and a logic module into a Erlang
%%%      module.
%%% @end
-module(beard_compiler).
-export([string/1, file/1, beam/3]).

%%% @doc Parses the string B as a beard template
string(B) when is_binary(B) ->
    string(binary_to_list(B));
string(L) when is_list(L) ->
    {ok, Tokens, _} = beard_lex:string(L),
    beard_yecc:parse(Tokens).

%%% @doc Parses the file at FilePath as a beard template.
file(FilePath) ->
    {ok, Bin} = file:read_file(FilePath),
    string(Bin).

%%% @doc Compiles a template and a logic module into a Erlang module,
%%%      which will be placed in OutDir. The module itself will be named
%%%      after the logic module.
%%%      If the compilation fails, the Erlang source will be placed
%%%      as LogicPath ++ ".erl" for debugging purposes. Otherwise,
%%%      the source code will be removed.
%%% @end
beam(TemplatePath, LogicPath, OutDir) when is_list(TemplatePath),
                                            is_list(LogicPath),
                                            is_list(OutDir) ->
    {ok, Template} = file(TemplatePath),
    RenderingOutput = beard_emit:emit(Template),
    {ok, LogicMod} = file:read_file(LogicPath),
    Outfile = LogicPath ++ ".erl", 
    ok = file:write_file(Outfile, [LogicMod, RenderingOutput]),
    Opts = [{outdir, OutDir}, report_errors, report_warnings, verbose],
    case compile:file(LogicPath, Opts) of
        error ->
            preserve_output;
        {error, _, _} ->
            preserve_output;
        _Ok ->
            file:delete(Outfile)
    end.
