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
% This file presents a simple library interface meant to facilitate the 
% writing of compiled libraries in the Mercury programming language that can 
% be easily loaded and used from within the Lua programming language.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

:- use_module io.





:- type io == io.state.



%-----------------------------------------------------------------------------%
%
% The Lua state
%

	% The state type is a refrence to the Lua state, also known as the
	% Lua Virtual Machine.  This type is defined in lua.h as
	% the C type "lua_State *". Note that as a convention borrowed from 
	% the C API, operations that query or manipulate the Lua state will
	% use the variable term 'L' to refer to the Lua state.
	%
:- type state
	---> lua_state
	;	lua(	
			global::global,
			registry::registry,
			upvalue::upvalue,
			stack::stack
		).	

	% init(L, !IO).
	% Prepares a lua_State for interaction with Mercury
	%
:- pred init(lua_state::in, io::di, io::uo) is det.

	%
	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred ready(lua_state::in) is semidet.



%-----------------------------------------------------------------------------%
%
% Variables
%

% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.
%
% Another thing to note is the fact that the lifetimes of Lua variables are
% manged by the Lua garbage collector, a mark and sweep collector not unlike
% the Bohem GC. 
%
% The current implementation of this library places reservations on and
% registers finalizer callbacks with variables passed by refrence between
% Mercury and Lua in such a manner that a Variable passed to it's non-native 
% environment will not be collected by it's own garbage collector until the
% non-native garbage collector calls the finalizer associated with it.

 
	% The var type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type lua.var.

%-----------------------------------------------------------------------------%
%
% Lua values and types
%

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
%	noted, values stored as userdata are subject to collection by 	
% 	Lua's Garbage Collector.
%
% 'lightuserdata' is seen in Lua as identical to userdata, but contains
%	a C pointer which will not be collected by Lua's GC.
%
% 'thread' is a lua_state, usually a coroutine, note that the main
% lua_state should not be treated like a coroutine.

	% The type lua.type represents the types that Lua recognizes.
	% 
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
	;	thread.

	% type/1 and type/2 query Lua for a Lua variable's type.  If 
	% provided a Mercury primitive value that can be implicity
	% cast to a Lua value, that type's equivalent value will be
	% returned.  For more complex Mercury types, 'none' will be
	% returned.
	% 
:- func lua_type(T) = type.

%-----------------------------------------------------------------------------%


	% Type lua.value represents the set of types that 
	% can be i 
:- type value.

:- module lua.value.
:- interface.

:- import_module float.
:- import_module bool.
:- import_module string.

:- type value
	--->	nil
	;	number(float)
	;	boolean(bool)
	;	string(string)
	;	lightuserdata(c_pointer)
	;	table(table)
	;	function(function)
	;	userdata(userdata)
	;	thread(thread)
	;	var(var)
	where equality is equal.


:- end_module lua.value.

:- pred equal(value::in, value::in) is semidet.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Lua objects/refrence types
%

% These types represent lua.vars of a specific Lua type.  In terms of Lua 
% semantics, these variables are passed by refrence, not by value. 
% 
% This is important to keep in mind when performing equality tests.  
% An equality test on two of these variables created seperately will fail, 
% even if they both have the same literal value. Equality will succeed when
% two refrence type variables refrence the same memory adress. 
%
% Note that the refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  As a result, (with the exception of 
% userdata) these values cannot be instantiated outside of Lua, only refrenced.
%
% Lua variables with assigned refrence types may be assigned metatables, 
% Lua tables that store metadata about 




%-----------------------------------------------------------------------------%
%
% Lua tables
%


% For the purposes of this library, values may only be assigned to unique 
% tables created in Mercury.  Any tables that have been exposed to Lua
% execution must be considered immutable to preserve Mercury's
% pure declarative semantics.


	% In Lua, the only native data-structure is the table. The Lua table
	% is implemented with a hybrid array/hash table data structure,
	% allowing tables to be efficiently used in the place of arrays, lists
	% maps and sets. 
	%
	% Tables may be assigned metatables, which can store metadata about
	% 
:- type lua.table.

% TODO: Write Table preds.

%-----------------------------------------------------------------------------%
	
% Lua functions are either compiled chunks of Lua source code, or are C
% function pointers as defined by the C type "lua_CFunction". Lua functions 
% are varadic, accepting variable numbers of arguments and
% return values.
% 
% Lua handles functions as first-class variables, they can be assigned
% to variables and passed as function arguments or return values in
% the same manner as any other value in Lua.
% 
% Lua functions are inherently impure.  Not only can they cause
% side effects to variables passed as arguments, but they are lexically 
% scoped, allowing side effects on any local variable declared in a scope
% outside of the function's scope.
%
% This fact effectively removes any way of determining a Lua function's purity
% outside of compiling a function directly with calls like loadstring and
% dostring, or passing a lua_CFunction function pointer.

	% A refrence to a 
	% This type is identical to the var type, but with a garuntee that
	% the value held by table is, in fact, a Lua table.
	% 
:- type function.

% TODO: Write function interface.

%-----------------------------------------------------------------------------%

% TODO: coroutines?, iterators?, multi/nondet functions?

%-----------------------------------------------------------------------------%
%
% Userdata
%
	% Lua handles all foreign values with the userdata value type.
	% Under normal circumstances, Lua can't actually do anything with
	% userdata. Attempts to use indexing, arithmetic or other operators
	% will result in a lua error.  However, as with tables and functions,
	% it is possible to assign metatables to userdata, allowing one to
	% extend the valid syntax with which Lua can interact with a given
	% userdata.
	

:- type userdata.

% TODO: Userdata calls




%-----------------------------------------------------------------------------%
%
% Thread
%

:- type thread.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.


:- import_module type_desc.

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define MR_LUA_MODULE ""MR_LUA_LANDER_MODULE""
#define MR_LUA_READY ""MR_LUA_LANDER_READY""
#define MR_LUA_LOCKED ""MR_LUA_LANDER_LOCKED""

#define MR_LUA_TYPE ""__mercury_type""



#define MR_LUA_FUNCTION_UPVALUE 1

").


:- pragma foreign_type("C", state, "lua_State *",
	[can_pass_as_mercury_type]).

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "extern void luaMR_init(lua_State *);").

:- pragma foreign_code("C", "void luaMR_init(lua_State * L) {

	/* Add tables to the registry. */

	/* univ_var(univ) metatable */
	lua_newtable(L);
	
	lua_pushboolean(L, 1);
	lua_setfield(L, -1, MR_LUA_IS_UNIV);

	/* TODO: load metamethods for univ vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_UNIV);

	/* function(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, MR_LUA_IS_FUNCTION);
	
	/* TODO: load metamethods for function vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_FUNCTION);

	/* univ_var(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, MR_LUA_IS_USERDATA);
	
	/* TODO: load metamethods for userdata vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_USERDATA);
	
	
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_MODULE);

	
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_LOCKED);

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const lua_Integer length = (lua_Integer)lua_objlen(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaMR_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, MR_LUA_READY);
}").

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").

:- pragma foreign_decl("C", "int luaMR_ready(lua_State *);").

:- pragma foreign_code("C", 
"
	/* check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
		lua_checkstack(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, MR_LUA_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").

:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_lua_ready(L);
").





%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", lua_type/0, 
[
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






%-----------------------------------------------------------------------------%


