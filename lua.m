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

:- import_module io.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module assoc_list.
:- import_module univ.

:- pred init_lua(lua_state::in, io::di, io::uo).

:- pred export_module(lua_state::in, string::in, lua_var::in, io::di, io::uo) is det.
:- pred export_module_loader(lua_state::in, string::in, function::in, io::di, io::uo) is det.


%%%%%%%%%%%%%%%%%
% Lua variables %
%%%%%%%%%%%%%%%%%

:- type lua_var
	--->	nil
	;	number(int)
	;	number(float)
	;	number(number)
	;	bool(bool)
	;	string(string)
	;	table(table)
	;	function(function)
	;	function(c_function)
	;	thread(lua_state)
	;	userdata(c_pointer)
	;	userdata(T)
	;	userdata(userdata)
	where equality is raw_equal.
	
:- pred raw_equal(lua_var::in, lua_var::in) is semidet.


:- type number.


%%%%%%%%%%%%%%%%%
% Lua Functions *
%%%%%%%%%%%%%%%%%

:- type lua_state.

:- type function.

:- type c_function.

:- type lua_result
	--->	ok
	;	error(message::string, code::lua_error_code).

:- type lua_error_code
	--->	runtime
	;	memory
	;	error.


:- pred load_string(string::in, function::out, lua_result::out) is det.
:- pred load_file(string::in, function::out, lua_result::out) is det.

:- func export_pred(pred(lua_state, list(lua_var)) = function.
:- mode export_pred(pred(in, out) is det) = out is det.
:- mode export_pred(pred(in, out) is semidet) = out is det.
% :- mode export_pred(pred(in, out) is multi) = out is det.
% :- mode export_pred(pred(in, out) is nondet) = out is det.

:- func export_io_pred(pred(lua_state, list(lua_var), io, io) = function.
:- mode export_io_pred(pred(in, out, di, uo) is det) = out is det.
:- mode export_io_pred(pred(in, out, di, uo) is semidet) = out is det.
% :- mode export_io_pred(pred(in, out) is multi) = out is det.
% :- mode export_io_pred(pred(in, out) is nondet) = out is det.


:- type args.

:- pred args(lua_state::in, args::out) is semidet.
:- func args(lua_state) = args is semidet.

:- pred next_arg(args::in, args::out, lua_var::out) is semidet.
:- pred next_arg(args::in, args::out) = (lua_var::out) is semidet.

:- pred list_args(lua_state::in, list(lua_var)::out) is det.
:- func list_args(lua_state) = list(lua_var).

%%%%%%%%%%%%%%
% Lua Tables %
%%%%%%%%%%%%%%

:- type table.

:- pred get(table, T, lua_var).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is det.
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.

:- func get(table, T) = lua_var.
:- mode get(in, in) = in is semidet.
:- mode get(in, out) = out is det.
:- mode get(out, in) = in is nondet.
:- mode get(out, out) = out is nondet.

:- pred get(table, table, T, lua_var).
:- mode get(di, uo, in, in) is semidet.
:- mode get(di, uo, in, out) is det.
:- mode get(di, uo, out, in) is nondet.
:- mode get(di, uo, out, out) is nondet.

:- func get(table, table, T) = lua_var.
:- mode get(di, uo, in) = in is semidet.
:- mode get(di, uo, in) = out is det.
:- mode get(di, uo, out) = in is nondet.
:- mode get(di, uo, out) = out is nondet.


:- func table ^ T = lua_var.
:- mode in ^ in = in is semidet.
:- mode in ^ in = out is det.
:- mode in ^ out = in is nondet.
:- mode in ^ out = out is nondet.


:- pred new_table(lua_state, table::uo) is det.
:- func new_table(lua_state) = (table::uo) is det.

:- pred set(table::di, table::uo, lua_var::in) is semidet.
:- func set(table::di, lua_var::in) = (table::uo) is semidet.

:- func (table::di ^ T::in := lua_var::in) = (table::uo) is semidet.

:- pred lock_table(table::di, table::out) is det.
:- func lock_table(table::di) = (table::out) is det.



%%%%%%%%%%%%%%%%
% Lua Userdata %
%%%%%%%%%%%%%%%%

:- typeclass userdata(T) where [
	pred ud_name(T, string),
	mode ud_name(in, in) is semidet,
	mode ud_name(in, out) is semidet,
	mode ud_name(out, in) is det,
	mode ud_name(unused, out) is nondet,
	
	pred metatable(string, table),
	mode metatable(in, out) is det,
	
].

	

:- type userdata.

:- func userdata(T) = userdata.
:- func some [T] some_value(userdata) = T.
:- func value_of(userdata) = T is semidet.

:- instance userdata(userdata).

:- instance userdata(univ).


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






	


    
