%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: varadic.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file presents typeclasses for varadic functions.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module varadic.

:- interface.
	
	% func(Index) = Arguments
	%
	% Higher order type used for passing varadic arguments and return 
	% values. Index represents the order that Arguments are passed to or 
	% from a varadic function.
:- type var_arg(T) == (func(int) = T).
:- inst var_arg(T) == (func(in) = out is cc_nondet).



	% The args typeclass represents a data type that can be constructed
	% from a higher-order var_arg.
	%
:- typeclass args(T) where [
	func args(var_arg(U)) = T
].


	
	% The to_args typeclass represents a data type that can be used to 
	% construct a higher-order var_arg.
	%
:- typeclass to_args(T) where [
	func to_args(T) = var_arg(U)
].


%-----------------------------------------------------------------------------%

:- pred var_call(pred(T), var_arg(U)) <= args(T).
:- mode var_call(in(pred(in) is det), in) is det.
:- mode var_call(in(pred(in) is semidet), in) is semidet.
:- mode var_call(in(pred(in) is multi), in) is multi.
:- mode var_call(in(pred(in) is nondet), in) is nondet.
:- mode var_call(in(pred(in) is cc_multi), in) is cc_multi.
:- mode var_call(in(pred(in) is cc_nondet), in) is cc_nondet.

:- func var_apply(func(T) = U, var_arg(V)) = U <= args(T).
:- mode var_apply(in(func(in) = out is det), in) = out is det.
:- mode var_apply(in(func(in) = out is semidet), in) = out is semidet.

:- func varadic(func(T) = U, var_arg(V)) = var_arg(W) 
	<= (args(T), to_args(U)).
:- mode varadic(in(func(in) = out is det), in) = out is det.
:- mode varadic(in(func(in) = out is semidet), in) = out is semidet.
	
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

var_call(Pred, Args) :- Pred(args(Args)).

var_apply(Func, Args) = Func(args(Args)).

varadic(Func, Args) = to_args(var_apply(Func, Args)).





