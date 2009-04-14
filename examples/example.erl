-module(example).
-export([run/0]).

gettext(Atom) ->
    case get(i18n_module) of
	undefined ->
	    atom_to_list(Atom);
	Mod ->
	    Mod:gettext(Atom)
    end.

run() ->
    io:format("~s~n", [gettext('Hello World')]).

    
