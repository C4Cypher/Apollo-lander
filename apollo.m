%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: apollo.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file presents a simple library interface meant to facilitate the 
% writing of compiled libraries in the Mercury programming language that can 
% be easily loaded and used from within the Lua programming language.
%
% The main predicate is intended to parse through the source code of a
% Mercury module in order to produce a Lua script, that when run, returns
% a Lua module that interfaces with the Mercury module compiled as a dynamic
% library, and exports the predicates, functions and types in the Mercury
% module's interface as Lua functions, values and userdata.
%
%-----------------------------------------------------------------------------%
%
% If you are reading this, you should no doubt be aware that Mercury is a
% strict language.  This is not merely about having a strict static type
% or mode system. Mercury was designed with a very specific set of semantics
% in mind.  
%
% Mercury is intended to be a purely declarative language, without 
% side-effects, and the Melbourne Mercury Compiler was designed specifically 
% to optimize code written with those strict, declarative semantics in mind.  
% While the language does allow you to deviate from those semantics, there 
% is a price to be paid in efficiency and ease-of use.  To do so is generally
% considered to be a 'bad idea'.
%
% In contrast, the Lua programming language is known for flexibility,
% extendability and for ease of code customization on the fly.  Using syntax
% influenced by Ada and Eiffel, and borrowing a few functional programming
% features from Lisp, Lua is simple, lightweight and easy to understand.
% 
% One of Lua's most notable features is it's extendability.  There are a
% multitude of ways to extend Lua's syntax and modify it's behavior,
% allowing one to define and use Lua with their own semantics, tailored
% to the programmer's needs.
%
% This, coupled with a similarly simple stack based C API, makes Lua a very
% popular choice for embedding in or binding to foreign languages and
% environments.
%
% For a Mercury programmer, to attempt to embed Lua in Mercury might seem
% counter-intuitive (if not outright insane).  Trying to work around
% Lua's dynamic, impure and unpredictable nature would run counter to
% Mercury's strengths and would be a nightmare to implement.
%
% At the same time, the way Lua's C API is implemented, if Lua were to load
% and invoke exported Mercury predicates and functions from a dynamic library,
% Mercury would be able to interact with the calling Lua state and it's
% instantiated variables as immutable values and data structures in a manner
% that preserves Mercury's pure declarative semantics.  Furthermore, 
% synergies with Lua's C API and language features would allow Mercury code 
% to define methods for Lua to interact with Mercury values passed to Lua as 
% if they were native Lua objects.  
%
% This would allow Lua programmers the ability to take advantage of the speed 
% and stability of compiled Mercury modules. It would also make it easier
% to embed Mercury in foreign code, and bind Mercury to foreign libraries and
% languages.
% 
% For these reasons, this module is intended to provide a simple way to export 
% Mercury predicates to loadable Lua modules that pass values between Mercury 
% and Lua in an efficient and seamless manner.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module apollo.

:- interface.

:- import_module io.

:- use_module lua.

:- type lua_state == lua.state.




	% Typeclass for values that can be passed from Lua as function
	% arguments.
	%
:- typeclass args(T) where [
	func args(lua_state) = T 
].

:- instance args(lua_state).

	% Typeclass for values that can be passed back to Lua as a 
	% function return value.
	%
:- typeclass return(T) where [
	func return(lua_state, T) = return
].

:- instance return(return).

	% Acceptable return values for a Lua function
	%
:- type return
	--->	nil	
	;	return_var(var)
	;	return_list(list(var))
	;	return_error(string).
	


:- implementation.







:- interface.

:- pred main(io::di, io::uo) is det.
