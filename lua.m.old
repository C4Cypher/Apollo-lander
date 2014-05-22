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

:- func push(T, lua_state(L)) = lua_state(L) <= value(T,L) is det.
	
:- pred pop(T, !lua_state(L)) <= value(T,L).
:- mode pop(out, in, out) is semidet.
:- mode pop(in, in, out) is semidet.

:- func pop(T, lua_state(L)) = lua_state(L) <= value(T,L) is semidet.


:- typeclass callable(T,L) <= value(T,L).

:- pred call(T, !lua_state(L),)
:- typeclass index(T,L) <= value(T,L).
:- typeclass newindex(T,L) <= index(T,L).
:- typeclass unifiable(A,B,L) <= (value(A,L),value(B,L)).	
:- type lua_state(L). % Abstract lua state or Existential type constrained to the Lua typeclass

:- type lua_state == lua_state(univ).

:- type value(T) == some [T,L] (value(T) <= value(T,L) (T -> L)). %functional dependency yadda yadda

:- type ltype --->
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
	

	

:- implementation.

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

tnone		= -1.
tnil 		= 0.
tboolean 	= 1.
tlightuserdata = 2.
tnumber 	= 3.
tstring		= 4.
ttable		= 5.
tfunction	= 6.
tuserdata	= 7.
tthread		= 8.

%TODO :- pragma promise equiv type ltype = c int

:- type lua_state(L) --->
	lua_state(
		version	::string,
		stack	::list(luavalue(L)),
		registry::assoc_list(registry_index,luavalue(L)),
		global	::table)	;
		some [L] (lua_state(L) => lua(L)).
		
:- typeclass value(T,L) <= lua(L) where [
	
	pred push(T, !lua_state(L)),
	mode push(in, di, uo) is det,
	
	pred pop(T, !lua_state(L)),
	mode pop(out, di, uo) is semidet,
	mode pop(in, di, uo) is semidet 
].

push(T, !.Lua) = !:Lua :- push(T, !Lua).
pop(T, !.lua) = !:Lua :- pop(T, !Lua).


	
	
