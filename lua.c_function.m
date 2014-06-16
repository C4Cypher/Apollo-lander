:- module lua.c_function.

:- interface.

:- type c_function.

:- implementation.

:- pragma foreign_type("C", c_function, "lua_CFunction").
