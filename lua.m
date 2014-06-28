%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.m.
% Main author: C4Cypher.
% Stability: low.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

:- import_module io.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module assoc_list.
:- import_module univ.

	% Represents the state of Lua, this is passed to and from C as 
	% a lua_State *
	%
:- type lua_state.

	% Prepares a lua_State for interaction with Mercury
	%
:- pred init_lua(lua_state::in, io::di, io::uo).

	% Passes a value to a lua_State's module loaders under the 
	% given string name
	%
:- pred export_module(lua_state::in, string::in, T::in, io::di, io::uo) is det.

	% Represents a refrence to a variable instantiated in Lua.
	%
:- type lua_var.

	% Through the Lua State, a mercury value can be passed as the value 
	% to a new lua_var.  int, float, bool, string and c_pointer are passed
	% by value, while other types are passed by refrence.
	% Example:  L ^ var(3) = Var
	%
:- pred var(lua_state, T, lua_var).
:- mode var(in, in, in) is semidet.
:- mode var(in, in, out) is det.
:- mode var(in, out, in) is semidet.

:- func var(lua_state, T) = lua_var.
:- mode var(in, in) = (in) is semidet.
:- mode var(in, in) = (out) is det.
:- mode var(in, out) = (in) is semidet. 	

	% In Lua, variables are not typed, values are.  Lua recognizes eight
	% types.
	%
	% 'nil' represents the abscence of value. 
	% 'number' is equivalent to float type.
	% 'boolean' is equivalent to the bool type.
	% 'string' is equivalent to the string type.
	%
	% 'table', a refrence to an associative array, used for lists and maps.
	%	Unlike an assoc_list, a Lua table may not associate more than
	%	one value with any given key, and thus, behaves more like the
	% 	Mercury map type.
	% 
	% 'function' is a refrence to a Lua function, which are impure, 
	%       may have a variable number of arguments, multiple return
	%	values and can be passed freely like any other variable.
	%
	% 'userdata' is lua's type for handling foreign data, unless otherwise
	%	noted, values stored as userdata are subject to collection by 		% 	Lua's Garbage Collector.
	%
	% 'lightuserdata' is seen in Lua as identical to userdata, but contains
	%	a C pointer which will not be collected by Lua's GC.
	%
	% 'thread' is a lua_state, usually a coroutine, note that the main
	% lua_state should not be treated like a coroutine.
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
	;	lightuserdata
	;	thread.

:- pred type(lua_var::in, lua_type::out)
:- func type(lua_var) = nil.

	% syntatic sugar for testing Lua variables against nil.
	%
:- func nil = (lua_var::in) is semidet.

	% A typedef containing the signature for a C function pointer that may
	% be passed to Lua as a Lua function
	%
:- type c_function.

	% Type to be thrown if Lua throws an error while interacting with
	% Mercury.
	%
:- type lua_error ---> error(message::string, code::lua_error_code).

	% 'runtime' represents a standard runtime error.
	% 'memory' represents a memory allocation error.
	% 'error' represents an error called while trying to handle an
	%	error.
	% 
:- type lua_error_code
	--->	runtime
	;	memory
	;	error.

	% Accepts a string of Lua source code that is dynamically compiled
	% into a Lua Function.  This function may not refrence any upvalues
	% or global variables.
	%
:- pred load_string(lua_state::in, string::in, lua_var::out) is semidet.
:- func load_string(lua_state, string) = lua_var is semidet.

%TODO: Implement a version of load_string that accepts a 
% string_builder or stream

	% Pass a higher order call to Lua that may be called as a function.
	%
:- func export_pred(pred(lua_state, list(lua_var))) = function.
:- mode export_pred(pred(in, out) is det) = (out) is det.
:- mode export_pred(pred(in, out) is semidet) = (out) is det.
% :- mode export_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_pred(pred(in, out) is nondet) = (out) is det.

	% The same as above, but with 
:- func export_io_pred(pred(lua_state, list(lua_var), io, io)) = function.
:- mode export_io_pred(pred(in, out, di, uo) is det) = (out) is det.
:- mode export_io_pred(pred(in, out, di, uo) is semidet) = (out) is det.
% :- mode export_io_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_io_pred(pred(in, out) is nondet) = (out) is det.

	% For collecting arguments passed from Lua to Mercury in a 
	% function call.
	%
:- type args.

:- pred args(lua_state::in, args::out) is semidet.
:- func args(lua_state) = args is semidet.

:- pred arg(args::in, args::out, T::out) is semidet.
:- func arg(args::in, args::out) = (T::out) is semidet.

	% An alternative method
	%
:- pred list_args(lua_state::in, list(lua_var)::out) is det.
:- func list_args(lua_state) = list(lua_var).


	% Extract values from tables (without calling metamethods), note that
	% for the det mode, the function will return nil if the key does not 
	% exist. If the variable is not a table, the call will always fail
	% or return nil.
:- pred get(lua_var, K, lua_var).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is det.
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.

:- func get(lua_var, K) = lua_var.
:- mode get(in, in) = in is semidet.
:- mode get(in, out) = out is det.
:- mode get(out, in) = in is nondet.
:- mode get(out, out) = out is nondet.

:- func lua_var ^ T = lua_var.
:- mode in ^ in = in is semidet.
:- mode in ^ in = out is det.
:- mode in ^ out = in is nondet.
:- mode in ^ out = out is nondet.



	% Lua table construction:
	% { value, value, value } or
	% { [key] = value, [key] = value, [key] = value } or
	% { value, value, value, [key] = value, [key] = value }
	% Note that any values added without the benefit of a key are indexed
	% by integral value starting at 1.
	%
:- type table_constructor
	---> 	{ }
	;	{ { _ } }	
	;	{ { table_array_constructor } }
	;	{ { table_hash_constructor } }.

:- type table_arracy_constructor
	---> 	_ , table_array_constructor
	;	_ , table_hash_constructor.

:- type table_key_constructor ---> [ _ ]

:- type table_hash_constructor
	--->	table_key_constructor = _
	; 	table_hash_constructor , table_hash_constructor.	


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

:- pragma foreign_decl("C", "//c
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").


:- pragma foreign_type("C", lua_state, "lua_State *").

:- type lua_ref ---> ref(lua_state, int).





:- pragma foreign_enum("C", lua_type, [
	none - "LUA_TNONE",
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






	



