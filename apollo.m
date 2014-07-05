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
%-----------------------------------------------------------------------------%
%
% External interface for the Apollo-Lander Library that is loadable by Lua
%
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
TODO Remove this entirely. */ 

/* luaopen function as required by Lua package.load */
extern int luaopen_apollo_lander(lua_State *);

/* check to see if Apollo has already been initialized. */
extern int luaAP_apollo_ready(lua_State *); 

/* lua_CFunction that prepares a lua_State for use with Apollo_lander */
extern int luaAP_init_apollo(lua_State *);

").



:- pragma foreign_code("C", "

/* check to see if Apollo has already been initialized. */
int luaAP_apollo_ready(lua_State * L) {
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
	int ready = lua_toboolean(L, 1);
	lua_remove(L, 1);
	return ready;
}


		
int luaAP_init_apollo(lua_State * L) {
	luaAP_init(L);
	return 0;
}


int luaopen_apollo_lander(lua_State * L) {
	if(!luaAP_apollo_ready(L))
		return luaAP_init_apollo(L);
	else
		return 0;
").	


