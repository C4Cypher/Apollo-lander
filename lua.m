:- module lua.

:- interface.

:- import_module list, string, bool, int, float, pair, assoc_list, map.

:- type state.


:- type cfunction.

:- pred call(int::in, int::in, state::di, state::uo) is det.

:- pred checkstack(int::in, state::di, state::uo) is semidet.

:- pred close(state::di) is det.

:- pred cpcall(cfunction::in, c_pointer::in, state::di, state::uo) is semidet.

:- pred createtable(int::in, int::in, state::di, state::uo) is det.

:- pred equal(int::in, int::in, state::di, state::uo) is semidet.

:- pred getfield(int:in, string::in, state::di, state::uo) is det.

:- pred getglobal(string::in, state::di, state::uo) is det.

:- pred getmetatable(int::in, state::di, state::uo) is det.

:- pred gettable(int::in, state::di, state::uo) is det.

:- pred gettop(int::out, state::di, state::uo) is det.

:- pred insert(int::in, state::di, state::uo) is det.

:- pred isboolean(int::in state::di, state::uo) is semidet.
:- pred iscfunction(int::in state::di, state::uo) is semidet.
:- pred isfunction(int::in state::di, state::uo) is semidet.
:- pred islightuserdata(int::in state::di, state::uo) is semidet.
:- pred isboolean(int::in state::di, state::uo) is semidet.
:- pred isnil(int::in state::di, state::uo) is semidet.
:- pred isnone(int::in state::di, state::uo) is semidet.
:- pred isnoneornil(int::in state::di, state::uo) is semidet.
:- pred isnumber(int::in state::di, state::uo) is semidet.
:- pred isstring(int::in state::di, state::uo) is semidet.
:- pred istable(int::in state::di, state::uo) is semidet.
:- pred isthread(int::in state::di, state::uo) is semidet.
:- pred isuserdata(int::in state::di, state::uo) is semidet.

:- pred lessthan(int::in, int::in, state::di, state::uo) is semidet.

:- pred newstate(state::uo) is det.

:- pred newtable(state::di, state::uo) is det.

:- pred newthread(state::uo, state::di, state::uo) is det.

:- pred next(int::in, state::di, state::uo) is semidet.

:- pred pcall(int::in, int::in, int::in, state::di, state::uo) is semidet.

:- pred pop(int::in, state::di, state::uo) is semidet.


:- pred pushboolean(int::in, state::di, state::uo) is det.
:- pred pushcclosure(cfunction::in, int::in, state::di, state::uo) is det.
:- pred pushcfunction(cfunction::in, state::di, state::uo) is det.
:- pred pushinteger(int::in, state::di, state::uo) is det.
:- pred pushlightuserdata(c_pointer::in, state::di, state::uo) is det.
:- pred pushnil(state::di, state::uo) is det.
:- pred pushnumber(float::in, state::di, state::uo) is det.
:- pred pushstring(string::in, state::di, state::uo) is det.

:- pred pushthread(state::di, state::uo) is det.
:- pred mainthread(state::di, state::uo) is semidet.

:- pred pushvalue(int::in, state::di, state::uo) is det.

:- pred rawequal(int::in, int::in, state::di, state::uo) is semidet.
:- pred rawget(int::in, state::di, state::uo) is det.
:- pred rawgeti(int::in, int::in, state::di, state::uo) is det.
:- pred rawset(int::in, state::di, state::uo) is det.
:- pred rawseti(int::in, int::in, state::di, state::uo) is det.

:- pred remove(int::in, state::di, state::uo) is det.

:- pred resume(int::in, state::di, state::uo) is det.

:- pred setfield(int::in, string::in, state::di, state::uo) is det.

:- pred setglobal(string::in, state::di, state::uo) is det.

:- pred setmetatable(int::in, state::di, state::uo) is det.

:- pred settable(int::in, state::di, state::uo) is det.

:- pred settop(int::in, state::di, state::uo) is det.

:- pred status(int::out, state::di, state::uo) is det.

:- pred toboolean(int:: in, state::di, state::uo) is semidet.
:- pred tocfunction(cfunction::out, state::di, state::uo) is semidet.







:- implementation.


:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", state, "lua_State").
:- pragma foreign_type("C", cfunction, "lua_CFunction").

:- type lua_operation = pred(lua_state::di,lua_state::uo) is det.

:- type lua_refrence --->
    stack(int);
    global(string);
    registry(string);
    upvalue(int);
    key(lua_refrence,lua_refrence);
    key(lua_refrence,string).
    
:- pred remove(int::in,lua_state::di,lua_state::uo) is det. 
:- pragma foreign_proc("C", pop(Index,Lua,Lua), [will_not_call_mercury], "lua_remove(Lua,Index);").

:- pred remove(lua_state::di,lua_state::uo) is det.
remove(!Lua) :- remove(-1, !Lua).
    
:- typeclass lua_var(T) where [
    pred push(T::in,lua_state::di,lua_state::uo) is det,
    pred pop(T::out,lua_state::di,lua_state::uo) is semidet,
    pred set(T::in,V::in





