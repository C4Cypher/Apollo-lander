:- module lua.

% Testing git

:- interface.

:- import_module list, string, bool, int, float.

:- type lua_type --->
    none;
    nil;
    number;
    boolean;
    string;
    table;
    function;
    userdata;
    thread;
    lightuserdata.
    
:- type lua_state.
:- type cfunction.

:- type mfunction == pred(


:- implementation.


:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", state, "lua_State *").
:- pragma foreign_type("C", cfunction, "lua_CFunction *").

:- pragma foreign_enum("C", lua_type, [
    none - "LUA_TNONE",
    nil - "LUA_TNIL",
    number - "LUA_TNUMBER",
    boolean - "LUA_TBOOLEAN",
    string - "LUA_TSTRING",
    table - "LUA_TTABLE",
    function - "LUA_TFUNCTION",
    userdata - "LUA_TUSERDATA",
    thread - "LUA_TTHREAD",
    lightuserdata - "LUA_TLIGHTUSERDATA" ]).
