:- module lua.

:- interface.

:- import_module list, string, bool, int, float, enum.

:- type state.
:- type cfunction.

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

:- pred call(int::in, int::in, state::in, state::out) is det.
:- pred checkstack(int::in, state::in, state::out) is semidet.
:- pred close(state::in) is det.
:- pred createtable(int::in, int::in, state::in, state::out) is det.

:- pred equal(int::in, int::in, state::in, state::out) is semidet.

:- pred getfield(int:in, string::in, state::in, state::out) is det.
:- pred getglobal(string::in, state::in, state::out) is det.
:- pred getmetatable(int::in, state::in, state::out) is det.
:- pred gettable(int::in, state::in, state::out) is det.
:- pred gettop(int::out, state::in, state::out) is det.

:- pred insert(int::in, state::in, state::out) is det.

:- pred isboolean(int::in, state::in, state::out) is semidet.
:- pred iscfunction(int::in, state::in, state::out) is semidet.
:- pred isfunction(int::in, state::in, state::out) is semidet.
:- pred islightuserdata(int::in, state::in, state::out) is semidet.
:- pred isboolean(int::in, state::in, state::out) is semidet.
:- pred isnil(int::in, state::in, state::out) is semidet.
:- pred isnone(int::in, state::in, state::out) is semidet.
:- pred isnoneornil(int::in, state::in, state::out) is semidet.
:- pred isnumber(int::in, state::in, state::out) is semidet.
:- pred isstring(int::in, state::in, state::out) is semidet.
:- pred istable(int::in, state::in, state::out) is semidet.
:- pred isthread(int::in, state::in, state::out) is semidet.
:- pred isuserdata(int::in, state::in, state::out) is semidet.

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

:- pred toboolean(int::in, bool::out, state::in, state::out) is det.
:- pred tocfunction(int::in, cfunction::out, state::in, state::out) is semidet.
:- pred tointeger(int::in, int::out, state::in, state::out) is det.
:- pred tonumber(int::in, float::out, state::in, state::out) is det.
:- pred topointer(int::in, c_pointer::out, state::in, state::out) is semidet.
:- pred tostring(int::in, string::out, state::in, state::out) is semidet.
:- pred tothread(int::in, state::out, state::in, state::out) is semidet.

:- pred type(int::in, lua_type::out, state::in, state::out) is det.
:- func type(int::in, state::in, state::out) = lua_type::out is det.

:- pred typename(lua_type::in, string::out, state::in, state::out) is det.
:- func typename(lua_type::in, state::in, state::out) = string::out is det.

:- pred xmove(state::in, state::in, int::in) is det.

:- pred yield(int::in, state::in, state::out) is det.



%%% Functions from the auxillary library %%%

:- pred callmeta(int::in, string::in, state::in, state::out) is semidet.
:- pred checkstack(int::in,string::in, state::in, state::out) is semidet.
:- pred dofile(string::in, state::in, state::out) is semidet.
:- pred dostring(string::in, state::in, state::out) is semidet.
:- pred loadfile(string::in, state::in, state::out) is semidet.
:- pred loadstring(string::in, state::in, state::out) is semidet.

:- pred ref(int::in, int::uo, state::in, state::out) is semidet.
:- func ref(int::in, state::in, state::out) = int::uo is semidet.
:- pred unref(int::in, int::di, state::in, state::out) is semidet.

%% same as above, but implicitly uses the registry
:- pred ref(int::uo, state::in, state::out) is semidet.
:- func ref(state::in, state::out) = int::uo is semidet.
:- pred unref(int::di, state::in, state::out) is semidet.


:- implementation.


:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", state, "lua_State").
:- pragma foreign_type("C", cfunction, "lua_CFunction").

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
    
:- pragma foreign_proc("C", call(Args, Results, Lua, Lua), [may_call_mercury], "lua_call(Lua, Args, Results);")    
:- pragma foreign_proc("C", checkstack(Extra,Lua,Lua), [will_not_call_mercury], "lua_checkstack(Lua,Extra);").
:- pragma foreign_proc("C", close(Lua), [will_not_call_mercury], "lua_close(Lua);").
:- pragma foreign_proc("C", createtable(Narr,Nrec,Lua,Lua), [will_not_call_mercury], 
    "lua_createtable(Lua,Narr,Nrec);").
:- pragma foreign_proc("C", equal(A,B,Lua,Lua), [may_call_mercury], "SUCCSESS_INDICATOR = lua_equal(Lua,A,B);").

:- pragma foreign_proc("C", getfield(Table,Key,Lua,Lua), [may_call_mercury], "lua_getfield(Lua,Table,Key);").
:- pragma foreign_proc("C", getglobal(Key,Lua,Lua), [may_call_mercury], "lua_getfield(Lua,Key);").




%% old
:- pred remove(int::in,lua_state::in,lua_state::out) is det. 
:- pragma foreign_proc("C", pop(Index,Lua,Lua), [will_not_call_mercury], "lua_remove(Lua,Index);").

:- pred remove(lua_state::in,lua_state::out) is det.
remove(!Lua) :- remove(-1, !Lua).
    





