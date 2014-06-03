:- module lua.

:- interface.

:- import_module list, string, bool, int, float, pair, assoc_list, map.

:- type state.


:- type cfunction.

:- pred call(int::in, int::in, state::in, state::out) is det.

:- pred checkstack(int::in, state::in, state::out) is semidet.

:- pred close(state::in) is det.

:- pred cpcall(cfunction::in, c_pointer::in, state::in, state::out) is semidet.

:- pred createtable(int::in, int::in, state::in, state::out) is det.

:- pred equal(int::in, int::in, state::in, state::out) is semidet.

:- pred getfield(int:in, string::in, state::in, state::out) is det.

:- pred getglobal(string::in, state::in, state::out) is det.

:- pred getmetatable(int::in, state::in, state::out) is det.

:- pred gettable(int::in, state::in, state::out) is det.

:- pred gettop(int::out, state::in, state::out) is det.

:- pred insert(int::in, state::in, state::out) is det.

:- pred isboolean(int::in state::in, state::out) is semidet.
:- pred iscfunction(int::in state::in, state::out) is semidet.
:- pred isfunction(int::in state::in, state::out) is semidet.
:- pred islightuserdata(int::in state::in, state::out) is semidet.
:- pred isboolean(int::in state::in, state::out) is semidet.
:- pred isnil(int::in state::in, state::out) is semidet.
:- pred isnone(int::in state::in, state::out) is semidet.
:- pred isnoneornil(int::in state::in, state::out) is semidet.
:- pred isnumber(int::in state::in, state::out) is semidet.
:- pred isstring(int::in state::in, state::out) is semidet.
:- pred istable(int::in state::in, state::out) is semidet.
:- pred isthread(int::in state::in, state::out) is semidet.
:- pred isuserdata(int::in state::in, state::out) is semidet.

:- pred lessthan(int::in, int::in, state::in, state::out) is semidet.

:- pred newstate(state::out) is det.

:- pred newtable(state::in, state::out) is det.

:- pred newthread(state::out, state::in, state::out) is det.

:- pred next(int::in, state::in, state::out) is semidet.

:- pred pcall(int::in, int::in, int::in, state::in, state::out) is semidet.

:- pred pop(int::in, state::in, state::out) is semidet.


:- pred pushboolean(int::in, state::in, state::out) is det.
:- pred pushcclosure(cfunction::in, int::in, state::in, state::out) is det.
:- pred pushcfunction(cfunction::in, state::in, state::out) is det.
:- pred pushinteger(int::in, state::in, state::out) is det.
:- pred pushlightuserdata(c_pointer::in, state::in, state::out) is det.
:- pred pushnil(state::in, state::out) is det.
:- pred pushnumber(float::in, state::in, state::out) is det.
:- pred pushstring(string::in, state::in, state::out) is det.

:- pred pushthread(state::in, state::out) is det.
:- pred mainthread(state::in, state::out) is semidet.

:- pred pushvalue(int::in, state::in, state::out) is det.

:- pred rawequal(int::in, int::in, state::in, state::out) is semidet.
:- pred rawget(int::in, state::in, state::out) is det.
:- pred rawgeti(int::in, int::in, state::in, state::out) is det.
:- pred rawset(int::in, state::in, state::out) is det.
:- pred rawseti(int::in, int::in, state::in, state::out) is det.

:- pred remove(int::in, state::in, state::out) is det.

:- pred resume(int::in, state::in, state::out) is det.

:- pred setfield(int::in, string::in, state::in, state::out) is det.

:- pred setglobal(string::in, state::in, state::out) is det.

:- pred setmetatable(int::in, state::in, state::out) is det.

:- pred settable(int::in, state::in, state::out) is det.

:- pred settop(int::in, state::in, state::out) is det.

:- pred status(int::out, state::in, state::out) is det.

:- pred toboolean(int:: in, state::in, state::out) is semidet.
:- pred tocfunction(cfunction::out, state::in, state::out) is semidet.







:- implementation.


:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", state, "lua_State").
:- pragma foreign_type("C", cfunction, "lua_CFunction").

:- type lua_operation = pred(lua_state::in,lua_state::out) is det.

:- type lua_refrence --->
    stack(int);
    global(string);
    registry(string);
    upvalue(int);
    key(lua_refrence,lua_refrence);
    key(lua_refrence,string).
    
:- pred remove(int::in,lua_state::in,lua_state::out) is det. 
:- pragma foreign_proc("C", pop(Index,Lua,Lua), [will_not_call_mercury], "lua_remove(Lua,Index);").

:- pred remove(lua_state::in,lua_state::out) is det.
remove(!Lua) :- remove(-1, !Lua).
    
:- typeclass lua_var(T) where [
    pred push(T::in,lua_state::in,lua_state::out) is det,
    pred pop(T::out,lua_state::in,lua_state::out) is semidet,
    pred set(T::in,V::in





