:- module lua.c.

% Direct bindings from the lua api to mercury predicates.
% Note: Abstraction with the mercury type system is to be kept to a minimum.

:- interface.

:- include_module int, float, string, io.

% typedef struct lua_State lua_State;
:- type lua.c.state.

% typedef int (*lua_CFunction) (lua_State *L);
:- type lua.c.cfunction.

% LUA_NUMBER and LUA_INTEGER should be cast to float and int respectively.


% state manipulation
:- pred lua.c.newstate(lua.c.state::uo) is det.
:- func lua.c.newstate = (lua.c.state::uo) is det.
:- pred lua.c.close(lua.c.state::di) is det.
:- pred lua.c.newthread(lua.c.state::uo, lua.c.state::di, lua.c.state::uo) is det.
:- func lua.c.newthread(lua.c.state::di, lua.c.state::uo) = (lua.c.state::uo) is det.

% basic stack manipulation.
:- pred lua.c.gettop(int::out, lua.c.state::di, lua.c.state::uo) is det.
:- func lua.c.gettop(lua.c.state::di, lua.c.state::uo) = (int::out) is det.










:- implementation.
