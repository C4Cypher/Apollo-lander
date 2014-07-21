%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: state.m
% Main author: C4Cypher.
% Stability: low.
% 
% This library defines types for handling mutable values in a pure matter,
% by allowing one to define semipure queries and reversible impure operations
% that may be performed on said value.  
%  
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module impure_stack.

:- interface.

:- import_module int.
:- import_module bool.
:- import_module maybe.


:- type state(T).

:- typeclass mutable_state(T) where [

