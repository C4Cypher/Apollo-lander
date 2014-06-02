:- module lua.

:- interface.

:- import_module list, string, bool, int, float, pair, assoc_list, map, io.

:- type lua_state.

:- type mercury_function == pred(lua_state::in, list(lua_value)::in, list(lua_value)::out) is det.
:- type c_function.

:- type lua_refrence.


:- type lua_value --->
    nil;
    boolean(bool);
    string(string);
    string(char);
    number(int);
    number(float);
    table(map(lua_value,lua_value));
    table(list(lua_value));
    function(mercury_function);
    function(c_function);
    userdata(c_pointer);
    thread(lua_state);
    refrence(lua_refrence);
    invalid.
    

  
:- implementation.

:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", lua_state, "lua_State").
:- pragma foreign_type("C", c_function, "lua_CFunction").


:- type lua_refrence --->
    stack(int);
    global(string);
    registry(string);
    upvalue(int).




