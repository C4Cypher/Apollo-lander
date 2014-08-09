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

:- type vars.
	

:- func vars(T) = vars.
:- mode vars(in) = out is det.
:- mode vars(out) = in is semidet.

:- func T1 , T2 = vars.
:- mode in, in = out is det.
:- mode out, out = in is semidet.

:- func univ_list(list(univ)) = vars.
:- mode univ_list(in) = out is det.
:- mode univ_list(out) = in is det.

:

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation. 
:- type vars
	--->	var(univ)
	;	cons(univ, vars))
	where equality is unify_vars.	
	


	% Due to nil's special status as the abscence of value, a single
	% nil value is passed in place of the empty list and will unify
	% with lists of nil.
	%
:- pred unify_vars(vars::in, vars::in) is det.

unify_vars(var(U), var(U)).
 
unify_vars(cons(U, Us1), cons(U, Us2) :-
	unify_vars(Us1, Us2).
	
unify_vars(var(U), cons(U, Us)) :- 
	nil_vars(Us).
	
unify_vars(cons(U, Us)), var(U)) :- 
	nil_vars(Us).

	% A given set of vars is composed of nothing but nil values.
	%
:- pred nil_vars(vars::in) is semidet.

nil_vars(var(univ(nil))).
nil_vars(cons(univ(nil), Us)) :- nil_vars(Us).


vars(T) = Vars :- 
	( T:vars -> 
		Vars = T 
	; 
		Vars = var(univ(T))
	).
	
T1 , T2 = cons(vars(T1), vars(T2)).


univ_list([]) = var(univ(nil)).
univ_list([U]) = var(U) :- not(U = univ(nil))).
univ_list([U | Us]) = cons(U, Us).


