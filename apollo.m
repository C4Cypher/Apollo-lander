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
%
% This file provides an external interface for the Apollo-Lander Library that
% is loadable by Lua.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- module apollo.
:- interface.

:- import_module lua.

:- implementation.

:- pragma foreign_import_module("C", lua).

:- pragma foreign_decl("C", "

/* Will this compile without the includes?
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
TODO Remove this entirely, or not. */ 

/* luaopen function as required by Lua package.load */
extern int luaopen_apollo(lua_State *);

").



:- pragma foreign_code("C", "


int luaopen_apollo(lua_State * L) {
	if(!luaAP_ready(L))
		return luaAP_init(L);
	else
		return 0;
").	


