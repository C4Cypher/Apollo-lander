:- module lua.

:- interface.

:- import_module list, string, bool, int, float, pair, assoc_list, map, io.

:- type lua_state.

:- type mercury_function.
:- type c_function.

:- type lua_refrence.



  
:- implementation.

:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", lua_state, "lua_State").
:- pragma foreign_type("C", c_function, "lua_CFunction").


:- type lua_refrence --->
    stack(int);
    global(string);
    registry(string);
    upvalue(int);
    key(lua_refrence,lua_refrence);
    key(lua_refrence,string).




