:- module lua.

:- interface.

:- include_module list, string, int, float, pair.


:- type luaversion == string.

:- typeclass lua(L).

:- typeclass value(T,L) <= lua(L).


:- func new_state = lua_state(L) <= lua(L).

:- pred close_state(lua_state(L):di) is det <= lua(L).

:- pred push(T, !lua_state(L)) <= value(T,L).
:- mode push(in, in, out) is det.
	
:- pred pop(T, !lua_state(L)) <= value(T,L).
:- mode pop(out, in, out) is semidet.
:- mode pop(in, in, out) is semidet.


:- typeclass callable(T,L) <= value(T,L).

:- pred pcall(T, !lua_state(L),)
:- typeclass index(T,L) <= value(T,L).
:- typeclass newindex(T,L) <= index(T,L).
:- typeclass unifiable(A,B,L) <= (value(A,L),value(B,L)).	
:- type lua_state(L). % Abstract lua state or Existential type constrained to the Lua typeclass

:- type lua_state == lua_state(univ).

% from lua.h
%	#define LUA_TNONE               (-1)
%	#define LUA_TNIL                0
%	#define LUA_TBOOLEAN            1
%	#define LUA_TLIGHTUSERDATA      2
%	#define LUA_TNUMBER             3
%	#define LUA_TSTRING             4
%	#define LUA_TTABLE              5
%	#define LUA_TFUNCTION           6
%	#define LUA_TUSERDATA           7
%	#define LUA_TTHREAD             8

:- type type --->
	tnone;
	tnil;
	tboolean;
	tlightuserdata;
	tnumber;
	tstring;
	ttable;
	tfunction;
	tuserdata;
	tthread.
	
:- type value --->
	nil;
	boolean;
	userdata;
	number;
	string;
	table;
	function;
	thread.
	
:- type value(T) --->
	some [T,L] ---> callable(L)

:- implementation.

:- type lua_state(L) --->
	lua_state(
		version	::string,
		stack	::list(luavalue(L)),
		registry::assoc_list(registry_index,luavalue(L)),
		global	::table)	;
		some [L] (lua_state(L) => lua(L)).
		
:- typeclass value(T,L) <= lua(L) where [
	
	pred push(T, !lua_state(L)),
	mode push(in, in, out),
	
	pred pop(T, !lua_state(L)),
	mode pop(out, in, out),
	
	
