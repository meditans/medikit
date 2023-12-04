:- module(medikit, [
                      % More operations on lists
                      list_sum/2,
                      list_product/2,

                      % Operation on subterms
                      subterms/3,

                      % Utilities for local knowledgebases
                      set_knowledge/2,

                      % Utilities for the chr store
                      find_constraint/2,
                      local_chr/3,

                      % Function expansion clauses for ξ
                      function_expansion/3
                  ]).

:- use_module(library(chr)).
:- use_module(library(function_expansion)).


% More list-related utilities

list_product(L, Prod) :-
    foldl([A,B,C] >> (C is A*B), L, 1, Prod).

list_sum(L, Sum) :- sum_list(L, Sum).


% Subterms matching a given predicate

subterms(Whole, Goal, Subterms) :-
    foldsubterms({Goal}/[A,S0,S1] >> (call(Goal,A), S1=[A|S0]),
                 Whole, [], Subterms).


% Local knowledgebases
:- meta_predicate set_knowledge(+, :).
set_knowledge(Knowledge, Module:Predicates) :-
    maplist({Module}/[P]>>retractall(Module:P), Predicates),
    abolish_all_tables,
    mapsubterms({Module}/[Subterm,_]>>(member(P, Predicates), subsumes_term(P, Subterm), assertz(Module:Subterm)),
                Knowledge, _).


% CHR utilities

find_constraint(Goal, Cs) :-
    findall(Goal, find_chr_constraint(Goal), Cs).

local_chr(Facts, Result, Res) :-
    thread_create((maplist(call, Facts),
                   find_constraint(Result, Ns),
                   thread_exit(Ns)), Id),
    thread_join(Id, exited(Res)).


% Macros for writing arithmetic

user:function_expansion(ξ(X), Y, Y #= X).
