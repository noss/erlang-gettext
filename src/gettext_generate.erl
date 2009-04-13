%%% File    : gettext_generate.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Generate erlang module from po-file
%%% Created : 13 Apr 2009 by Christian <chsu79@gmail.com>

-module(gettext_generate).

-compile(export_all).


write([Mod, PO]) ->
    write(Mod, PO).

write(Mod, PO) ->
    Content = generate(Mod, PO),
    file:write_file(lists:concat([Mod, '.erl']), Content).

generate(Modname, POFilename) ->
    Translations = gettext:parse_po(POFilename),
    Head = ["-module(", atom_to_list(Modname), ").\n",
	    "-export([gettext/1]).\n"],
    Clauses = [generate_clause(K,V) || {K,V} <- Translations, not is_atom(K)],
    EndClause = ["gettext(X) -> atom_to_list(X)"],
    erlang:iolist_to_binary([Head, clause_join([Clauses|EndClause])]).

generate_clause(K, V) ->
    ["gettext('", K, "') ->\n",
     "  \"", V,"\""].

clause_join([Last]) ->
    [Last, ".\n"];
clause_join([H|Tl]) ->
    [H, ";\n" | clause_join(Tl)].

	      
