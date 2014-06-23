:- module lua.

/*
%% lua.m - Apollo-lander

Mercury is a purely declarative logic language. It is related to both Prolog and
Haskell.[1] It features a strong, static, polymorphic type system, as well as a
strong mode and determinism system.

Apollo, the ancient Greek name for the planet Mercury, when observed just before
dawn as a morning star

The Apollo spacecraft was composed of three parts designed to accomplish the 
American Apollo program's goal of landing astronauts on the Moon by the end of 
the 1960s and returning them safely to Earth.

Lua - A lightweight programming language with dynamic typing.
From the Portuguese lua (“moon”). The inventors of the language were Brazilian.

%%----------------------------------------------------------------------------%%

This library is intended to provide a simple, easy to use interface between the
Mercury functional/logic compiled programming language and the Lua Virtual 
Machine in such a manner that it preserves the advantages of both environments.

Mercury and Lua are vastly different programming languages.  

Mercury is derived from Prolog, yet it is strongly-typed, functionally pure 
language that is compiled to C and then to native binary code. Any imperitive 
changes to the program state must be threaded through a state variable to 
preserve purity, or otherwise explicitly marked so that it can be handled safely
by the compiler.  The logical and functional purity of Mercury allow it's
compiler to perform exstensive optimization, and to catch potential problems at
compile-time that other languages would let through.

Lua is an imperitave dynamically-typed scripting language that is compiled to 
mid-level bytecode in realtime so that the interpreter (referred to as the Lua 
Virtual Machine) can execute Lua scripts on the fly.  The only native data 
structure is an efficiently implented hash-table.  Functions are treated as 
first-class variables, and Lua has an exstensive C-API that easily handles 
foreign data making it easy to bind to foreign code. This, coupled with the fact
that it is small and very fast makes it a superb choice for an embedded 
scripting language, or as glue-code between different environments.

Lua may not be the best choice for embedding in a Mercury program, it's
imperitive nature and the maleability of it's code would sacrifice much of the
advantage offered by Mercury's purity.  However, in order to interact with the
real world, Mercury provides language constructs for interacting with mutable
state that can be clunky and akward.

Apollo-lander is intended to allow Lua code to make calls on Mercury predicates
in a controlled manner, allowing Mercury to query the state of the Lua VM
without modifying it, and then returning values back to Lua in a manner that
capitalizes on Lua's capability to use varadic function calls and return values.

Lua functions may be called from Mercury, but only if the function in question
originated in a controlled manner from the Apollo interface.  Lua functions
passed as arguments to Mercury should be opaque, either passed back in return
values.

Planned features:
	-Allow outside coders to define additional types that can be easily
		passed to Lua as return values, using either userdata,
		metatables and/or Lua's existing primitive types, without
		compromising type-safety or having to resort to the Lua API.
	-Allow Lua to make calls on non-deterministic predicates via iterators
		and Mercury multithreading.
	-Initial support is for Lua 5.1, eventual support for Lua 5.2
	-Support for Lua coroutines

*/

:- interface.


%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%

:- type nil ---> nil.

:- import_module int.

:- import_module float.

:- import_module bool.

:- import_module string.

:- type table.

:- type function.

:- type c_function.

:- type lua_state.

:- type userdata.

%%%%%%%%%%%%%%%%%
% Lua variables %
%%%%%%%%%%%%%%%%%

:- type lua_var.

:- pred is_nil(lua_var::in) is semidet.
:- pred to_nil(lua_var::di) is det.
:- pred to_nil(lua_var::di, lua_var::out) is det.
:- func to_nil(lua_var::di) = (nil::in) is det.
:- func nil(lua_var) = nil is semidet.

:- pred is_int(lua_var::in) is semidet.
:- pred to_int(lua_var::di, int::in) is det.
:- pred to_int(lua_var::di, lua_var::out, int::in) is det.
:- func to_int(lua_var::di, int::in) = (lua_var::out) is det.
:- pred int(lua_var::in, int::out) is semidet.
:- func int(lua_var) = int is semidet.

:- pred is_float(lua_var::in) is semidet.
:- pred to_float(lua_var::di, float::in) is det.
:- pred to_float(lua_var::di, lua_var::out, float::in) is det.
:- func to_float(lua_var::di, float::in) = (lua_var::out) is det.
:- pred float(lua_var::in, float::out) is semidet.
:- func float(lua_var) = float is semidet.

:- pred is_bool(lua_var::in) is semidet.
:- pred to_bool(lua_var::di, bool::in) is det.
:- pred to_bool(lua_var::di, lua_var::out, bool::in) is det.
:- func to_bool(lua_var::di, bool::in) = (lua_var::out) is det.
:- pred bool(lua_var::in, bool::out) is semidet.
:- func bool(lua_var) = bool is semidet.

:- pred is_string(lua_var::in) is semidet.
:- pred to_string(lua_var::di, string::in) is det.
:- pred to_string(lua_var::di, lua_var::out, string::in) is det.
:- func to_string(lua_var::di, lua_string::in) = (lua_var::out) is det.
:- pred string(lua_var::in, string::out) is semidet.
:- func string(lua_var) = string is semidet.

:- pred is_table(lua_var::in) is semidet.
:- pred to_table(lua_var, table).
:- mode to_table(di, in) is det.
:- mode to_table(di, di) is det.
:- func to_table(lua_var) = table.
:- mode to_table(di) = in is det.
:- mode to_table(di) = di is det.
:- func table(lua_var) = table is semidet.


:- pred is_function(lua_var::in) is semidet.
:- pred to_function(lua_var::di, function::in) is det.
:- pred to_function(lua_var::di, lua_var::out, function::in) is det.
:- func to_function(lua_var::di) = (lua_var::out) is det.
:- pred function(lua_var::in, function::out) is semidet.
:- func function(lua_var) = function is semidet.

:- pred is_c_function(lua_var::in) is semidet.
:- pred to_c_function(lua_var::di, c_function::in) is det.
:- pred to_c_function(lua_var::di, c_function::in) is det.
:- func to_c_function(lua_var::di) = (lua_var::out) is det.
:- pred c_function(lua_var::in, c_function::out) is semidet.
:- func c_function(lua_var) = c_function is semidet.

:- pred is_lua_state(lua_var::in) is semidet.
:- pred to_lua_state(lua_var::di, lua_state::in) is det.
:- pred to_lua_state(lua_var::di, lua_state::in) is det.
:- func to_lua_state(lua_var::di) = (lua_var::out) is det.
:- pred lua_state(lua_var::in, lua_state::out) is semidet.
:- func lua_state(lua_var) = lua_state is semidet.

:- pred is_userdata(lua_var::in) is semidet.
:- pred to_userdata(lua_var::di, userdata::in) is det.
:- pred to_userdata(lua_var::di, userdata::in) is det.
:- func to_userdata(lua_var::di) = (lua_var::out) is det.
:- pred userdata(lua_var::in, userdata::out) is semidet.
:- func userdata(lua_var) = userdata is semidet.

:- pred is_c_pointer(lua_var::in) is semidet.
:- pred to_c_pointer(lua_var::di, c_pointer::in) is det.
:- pred to_c_pointer(lua_var::di, c_pointer::in) is det.
:- func to_c_pointer(lua_var::di) = (lua_var::out) is det.
:- pred c_pointer(lua_var::in, c_pointer::out) is semidet.
:- func c_pointer(lua_var) = c_pointer is semidet.

:- import_module type_desc.

:- pred is_a(lua_var::in, type_desc::in) is semidet.
:- pred to(lua_var::di, T::in) is det <= metatable(T).
:- pred to(lua_var::di, T::in) is det <= metatable(T).
:- func to(lua_var::di) = (lua_var::out) is det <= metatable(T).
:- pred from(lua_var::in, T::out) is semidet <= metatable(T).
:- func from(lua_var) = T is semidet <= metatable(T).

%%%%%%%%%%%%%%%%%
% Lua Functions *
%%%%%%%%%%%%%%%%%

:- import_module io.

:- type io_passing == pred(io, io).
:- inst io_passing == bound(pred(di, uo) is det).

:- type lua_call == pred(lua_state).
:- inst lua_call bound(pred(in) is det).

:- type lua_result
	--->	ok
	;	error(message::string, code::lua_error_code).

:- type lua_error_code
	--->	runtime
	;	memory
	;	error.


:- pred load_string(string::in, function::out, lua_result::out) is det.
:- pred load_file(string::in, function::out, lua_result::out) is det.

:- pred make_call(pred(lua::in) is det, function::out) is det.
:- pred make_function(
	pred(
		list(lua_var)::in,
		in(func = (lua_var::uo) is semidet),
		list(lua_var)::out
	) is det.

:- pred args(lua_state::in, list(lua_var)::out) is det.
:- func args(lua_state) = list(lua_var).

:- pred return(lua_state, lua_var).
:- mode return(in, in) is semidet.
:- mode return(in, uo) is semidet.
:- pred return(lua_state, lua_var, io, io).
:- mode return(in, in, di, uo) is det.
:- mode return(in, uo, di, uo) is det. 


%%%%%%%%%%%%%%
% Lua Tables %
%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%
% Lua Metamethods %
%%%%%%%%%%%%%%%%%%%

:- typeclass metatable(T) where [].

:- pred index(T::in, 
    
%%%%%%%%%%%%%%%%%%
:- implementation.
%%%%%%%%%%%%%%%%%%

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").


:- pragma foreign_type("C", lua_state, "lua_State *").

:- type lua_ref ---> ref(lua_state, int).

:- type lua_primitive
	--->	value(int)
	;	value(float)
	;	value(bool)	
	;	value(string).

:- type lua_type 
	--->	none
	;	nil
	;	boolean
	;	lightuserdata
	;	number
	;	string
	;	table
	;	function
	;	userdata
	;	lightuserdata.

:- pragma foreign_enum("C", lua_type, [
	none - LUA_TNONE,
	nil - "LUA_TNIL",
	boolean - "LUA_TBOOLEAN",
	lightuserdata - "LUA_TLIGHTUSERDATA",
	number - "LUA_TNUMBER",
	string - "LUA_TSTRING",
	table - "LUA_TTABLE",
	function - "LUA_TFUNCTION",
	userdata - "LUA_TUSERDATA",
	thread - "LUA_TTHREAD"
]).

:- type lua_function == lua_ref.
:- type lua_thread == lua_ref.
:- type lua_userdata == lua_ref.

:- func lua_call(T::in, lua_state::in, io.state::di, io.state::uo) = (int::out) 
	<= function(T) is det.

lua_call(MRcall, L, !IO) = Return :-
	call_function(MRcall, L, In, 

:- pred get_value(lua_state::in, int::in, lua_value::out, io.state::di, 
	io.state::uo) is semidet.

get_value(L, I, V, !IO) :-
	get_primitive(L, I, P, Type, !IO),
	  Type = nil -> V = nil
	; Type = number -> V = number(P)
	; Type = boolean -> V = boolean(P)
	; Type = string -> V = string(P)
	; Type = table ->
		push_value(L, nil, !IO) ,
		get_table(L, I, map.init, V)
	; Type = function ->
		V = function(ref(L, I))
	; Type = thread ->
		V = thread(ref(L, I))
	; Type = userdata ->
		V = userdata(ref(L, I))
	; Type = lightuserdata ->
		V = userdata(ref(L, I)).
:- pred push_value(lua_state::in, lua_value::out, io.state::di, 
	io.state::uo) is det.

push_value(L, nil, !IO) :- push_primitive(L, 0, nil, !IO).
push_value(L, integer(V), !IO) :- push_primitive(L, float(V), number, !IO).
push_value(L, number(V), !IO) :- push_primitive(L, V, number, !IO).
push_value(L, string(V), !IO) :- push_primitive(L, V, string, !IO).
push_value(L, boolean(V), !IO) :- push_primitive(L, V, boolean, !IO).

push_value(L, table(M), !IO) :- 
	push_primitive(L, 0, table, !IO)
	map.member(M, K, V),
	push_value(L,K, !IO),
	push_value(L,V, !IO),
	set_table(L, I, !IO).

push_value(L, function(ref(_, I)), !IO) :- push_primitive(L, I, function, !IO).
push_value(L, thread(ref(_, I)), !IO) :- push_primitive(L, I, thread, !IO).
push_value(L, userdata(ref(_, I)), !IO) :- push_primitive(L, I, userdata, !IO).
	


:- pred top(lua_state::in, int::out, io.state:di, io.state::uo).
:- pragma foreign_proc("C", top(L::in, Top::out, _I, _O), 
	"Top = lua_gettop(L);").

:- pred get_primitive(lua_state::in, int::in, T::out, lua_type::out, 
	io.state::di, io.state::uo) is det.


:- pragma foreign_proc("C", get_primitive(L::in, I::in, V::out, Type::out,
	 _, _),	[promise_pure, will_not_call_mercury], "
		
	Type = lua_type(L, I);
	switch(Type)
	{
		case LUA_TNUMBER:
			V = (MR_Float) lua_tonumber(L, I);
			break;
		case LUA_TBOOLEAN:
			if(lua_toboolean(L, I))
				V = MR_YES;
			else
				V = MR_NO;
			break;
		case LUA_TSTRING:
			V = (MR_String) lua_tostring(L, I);
			break;
		default:
			V = 0;
		break;
	}"
).

:- pred push_table(lua_state::in, map(lua_value, lua_value)::in, io_state::di,
	io_state::uo) is semidet.




:- pred get_table(lua_state::in, int::in, map(lua_value, lua_value)::in, 
	map(lua_value, lua_value)::out, io.state::di, io.state::uo) is det.

get_table(L, I, !M, !IO) :-
	get_next(L, I, !IO) ->
		rawget(L, I) ,
		get_value(L, -2, K, !IO) ,
		get_value(L, -1, V, !IO) , 
		!:M = map.insert(!.M, K, V) ,	
		push_value(L, K, !IO) ,
		get_table(L, I, !M, !IO)
	;
		!:M = !.M.

:- pred rawget(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawget(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawget(L, I);").

:- pred rawset(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawset(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawset(L, I);").

:- pred pop(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", pop(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_pop(L, I);").

:- pred push_primitive(lua_state::in, int::in, T::in,  lua_type::in, 
	io.state::di, io::state::uo) is det.

:- pragma foreign_proc("C", push_primitive(L::in, V::in, Type::in, _, _),
	[promise_pure, will_not_call_mercury], "
	assert(lua_checkstack(L, 1));	
	
	
	switch(Type)
	{
		case LUA_TNIL:
			lua_pushnil(L);
			break;
		case LUA_TNUMBER:
			lua_pushnumber(L, (lua_Number)V);
			break;
		case LUA_TBOOLEAN:
			if(V = MR_YES)
				lua_pushboolean(L, 1);
			else
				lua_pushboolean(L, 0);
			break;
		case LUA_TSTRING:
			lua_pushstring(L, V);
			break;
		case LUA_TTABLE:
			lua_checkstack(L, 2);
			lua_newtable(L);
			break;
		default:
			lua_pushvalue(L, I);
		break;
	}"
).
		
:- pred get_next(lua_state::in, int::in, io.state::di,
	io.state::uo) is semidet.

:- pred foreign_proc("C", get_next(L::in, I::in, _I, _O),
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = lua_next(L, I);"
).
			
		

:- pragma foreign_export("C", lua_call, "luaM_call






	


    
