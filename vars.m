%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: vars.m.
% Main author: C4Cypher.
% Stability: low.
% 
% Dynamic lists of values.
%
% This type represents the Mercury equivalent to Lua's variable list
% espression, used to pass varadic function arguments and return
% values. 
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module vars.

:- interface.

:- import_module list.
:- import_module univ.

:- type vars.
	
:- pred vars(T, vars).
:- mode vars(in, out) is det.
:- mode vars(out, in) is semidet.

:- func vars(T) = vars.
:- mode vars(in) = out is det.
:- mode vars(out) = in is semidet.

:- func (T1 , T2) = vars.
:- mode (in, in) = out is det.
:- mode (out, out) = in is nondet.
:- mode (out, out) = in is cc_nondet.

:- pred vars_to_list(vars::in, list(univ)::out) is det.
:- pred list_to_vars(list(univ)::in, vars::out) is det.

:- func univ_list(list(univ)) = vars.
:- mode univ_list(in) = out is det.
:- mode univ_list(out) = in is det.


%:- type nil ---> nil.  % Declared in lua.m 

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 

:- import_module require.

:- type vars
	--->	var(univ)
	;	cons(univ, vars)
	;	nil_vars.
		
vars(T::in, V::out) :-
	( dynamic_cast(T, Tv:vars) ->
		V = Tv
	;
		V = var(univ(T))
	).
			
vars(T::out, V::in) :-
	( 
		V = var(U), 
		univ(T) = U
	;
		V = cons(_, _),
		dynamic_cast(V, T) 
	;
		V = nil_vars,
		dynamic_cast(nil, T)
	).
	
vars(T) = V :- vars(T, V).


:- pragma promise_pure(vars/2). 
		
	
	
(T1 , T2) = cons(univ(T1), vars(T2)).

vars_to_list(V, L) :-
	require_complete_switch [V]
	( V = var(U) ->
		L = [U]
	; V = cons(U, Vs) ->
		L = [U | Ls],
		vars_to_list(Vs, Ls)
	; 
		V = nil_vars,
		L = []
	;
		unexpected($module, $pred)
	).
	
list_to_vars(L, V) :-
	require_complete_switch [L]
	( L = [] ->
		V = nil_vars
	; nil_list(L) ->
		V = nil_vars
	; 
		L =  [U | Us],
		V = cons(U, Vs),
		list_to_vars(Us, Vs)
	; 
		unexpected($module, $pred)
	).
		
:- pred nil_list(list(univ)::in) is semidet.

nil_list([]).
nil_list([univ(nil) | X]) :- nil_list(X).


univ_list(L::in) = (V::out) :- list_to_vars(L, V).
univ_list(L::out) = (V::in) :- vars_to_list(V, L).
	 
:- pragma promise_pure(univ_list/1).

