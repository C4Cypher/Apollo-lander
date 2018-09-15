%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: statement.m.
% Main author: c4cypher.
% Stability: low.
% 
% Describe the module.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module statement.

:- interface.


% In Lua, a statement performs an impure change upon the Lua state. Given the
% imperative nature of Lua, a Lua program consists of a series of statements to
% be executed in sequential order.

:- type statement == (impure func(lua_state) = lua_result).

:- type block == list(statement).

:- pred do(block::in, lua::in, lua_result::out, io::di, io::uo) is det.


:- type lua_result
	---> 	ok
	;	error(lua_error)
	;	return(list(expression)).
