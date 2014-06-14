:- module apollo_lander.
:- interface.

:- import_module lua.

% TODO: Rewrite apollo_lander.m as to produce an appropriate C header file.

:- implementation.

:- pragma foreign_import_module("C", lua).

:- pragma foreign_decl("C", "int luaopen_apollo_lander(lua_State *);").

:- pragma foreign_code("C", "int luaopen_apollo_lander(lua_State * L) {
	if(!apollo_ready(L))
		return luaM_init_apollo(L);
	else
		return 0;
").
