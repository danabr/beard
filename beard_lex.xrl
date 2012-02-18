%%% @copyright Daniel Abrahamsson, 2012
%%% @doc The beard lexer.
Definitions.

Rules.

{{#[^}]+}} :
    {match, [_, Function]} = re:run(TokenChars, "{{#([^}]+)}}", [{capture, all, list}]),
    {token, {subcontext_start, TokenLine, Function}}.
{{/[^}]+}} :
    {match, [_, Function]} = re:run(TokenChars, "{{/([^}]+)}}", [{capture, all, list}]),
    {token, {subcontext_end, TokenLine, Function}}.
{{[^}]+}} :
    {match, [_, Function]} = re:run(TokenChars, "{{([^}]+)}}", [{capture, all, list}]),
    {token, {call, TokenLine, Function}}.
{[^{]* : {token, {data, TokenLine, TokenChars}}.
[^{]+ : {token, {data, TokenLine, TokenChars}}.

Erlang code.
