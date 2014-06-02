:- module lua.

:- interface.

:- import_module list, string, bool, int, float, pair, assoc_list, map, io.

:- type lua_state.


:- type c_function.

:- type lua_refrence.

:- typeclass lua_var(T).
:- instance lua_var(lua_refrence).

:- type mercury_function = pred(list(T)::in,list(lua_refrence)::out,lua_state::di,lua_state::out) is det.



  
:- implementation.


:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", lua_state, "lua_State").
:- pragma foreign_type("C", c_function, "lua_CFunction").

:- type lua_operation = pred(lua_state::di,lua_state::uo) is det.

:- type lua_refrence --->
    stack(int);
    global(string);
    registry(string);
    upvalue(int);
    key(lua_refrence,lua_refrence);
    key(lua_refrence,string).
    
:- pred pop(lua_state::di,lua_state::uo) is det. %Pop the top value off the stack.
:- pragma foreign_proc("C", pop(Lua,Out), [will_not_call_mercury], "lua_remove(Lua,-1); Lua = Out;").





