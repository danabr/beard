%%% @copyright Daniel Abrahamsson, 2012
%%% @doc The beard parser
Nonterminals
template subtemplate.
Terminals
call subcontext_start subcontext_end data.
Rootsymbol template.
Endsymbol '$end'.

template -> subtemplate : ['$1'].
template -> subtemplate template : ['$1'|'$2'].
subtemplate -> call : {call, list_to_atom(element(3, '$1'))}.
subtemplate -> data : {data, element(3, '$1')}.
subtemplate -> subcontext_start template subcontext_end :
    {context, list_to_atom(element(3, '$1')), '$2'}.


