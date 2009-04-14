%%% File    : gettext_generate.erl
%%% Author  : Christian <chsu79@gmail.com>
%%% Description : Generate erlang module from po-file
%%% Created : 13 Apr 2009 by Christian <chsu79@gmail.com>

-module(gettext_generate).

-compile(export_all).

compile(Modname, PO) ->
    compile(Modname, PO, []).

compile(Modname, POFilename, Opts) ->
    [_Header | Translations] = gettext:parse_po(POFilename),
    
    Forms = generate_forms(Modname, Translations),
    
    {ok, Modname, Code} = compile:forms(Forms, Opts),
    
    code:purge(Modname),
    code:load_binary(Modname, POFilename, Code).

generate_forms(Modname, Translations) ->
    ModuleAst  = erl_syntax:attribute(
		   erl_syntax:atom(module), 
		   [erl_syntax:atom(Modname)]),
    
    ExportAst = erl_syntax:attribute(
		  erl_syntax:atom(export),
		  [erl_syntax:list(
		     [erl_syntax:arity_qualifier(
			erl_syntax:atom(gettext), 
			erl_syntax:integer(1))
		     ])
		  ]),
    
    
    FunctionAst = generate_function(Translations),
    
    [erl_syntax:revert(Ast) || Ast <- [ModuleAst, ExportAst, FunctionAst]].
    

generate_function(KVs) ->
    Clauses = [generate_clause(K, V) || {K,V} <- KVs],
    
    X = erl_syntax:variable("X"),
    ListToAtom = erl_syntax:atom(atom_to_list),
    EndClause = erl_syntax:clause(
		  [X], none,
		  [erl_syntax:application(none, ListToAtom, [X])]),
    
    erl_syntax:function(erl_syntax:atom(gettext), Clauses ++ [EndClause]).


generate_clause(K, V) ->
    erl_syntax:clause(
      [erl_syntax:atom(K)], 
      none, 
      [erl_syntax:string(V)]
     ).


	      
