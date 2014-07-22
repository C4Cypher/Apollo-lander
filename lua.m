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

:- import_module io.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module maybe.




%-----------------------------------------------------------------------------%
%
% The Lua state
%

	% The lua type is a refrence to the Lua state, also known as the
	% Lua Virtual Machine.  This type is defined in lua.h as
	% the C type "lua_State *". Note that as a convention borrowed from 
	% the C API, operations that query or manipulate the Lua state will
	% use the variable term 'L' to refer to the Lua state.
	%
:- type lua_state.
:- type lua == lua_state.

% WARNING! Refrences to Lua types (tables, functions, userdata) derived
% from one lua_state are NOT compatible with other seperately created
% lua_states. The only exception to this is lua_states created as threads.
% lua_threads may freely pass variables to or from their parent state and
% sibling threads.

	% Create a fresh, new , initialized lua_state.
	%
:- func new_state = lua_state.
:- mode new_state = out.
:- mode new_state = uo.

	% Create a new lua_state and assign values to it's
	% global environment.
	%
:- func new_state(table_ctor(string)) = lua_state.
:- mode new_state(in) = out.
:- mode new_state(in) = uo.

	% Create a new lua_state and assign values to both it's
	% environment and registry.
	%
:- func new_state(table_ctor(string), table_ctor(string)) = lua_state.
:- mode new_state(in) = out.
:- mode new_state(in) = uo.

	% init(L, !IO).
	% Prepares an existing lua_State for interaction with Mercury
	%
:- pred init(lua::in, io::di, io::uo) is det.

	% Verify that Lua is prepared for interaction with Mercury
	%
:- pred ready(state::in) is semidet.
	
%-----------------------------------------------------------------------------%
%
% Querying the Lua state.
%


% Each function call is provided with a local stack which function arguments
% are pushed onto before the call.  The function call returns an integer
% and Lua uses that number to determine the number of return values to take
% off the top of the stack (from the bottom up).  In both cases the first 
% argument is pushed first, with the last argument on the top of the stack.
% 
% Values on the stack can be refrenced by integer index values. Positive 
% integers refrence values from the bottom of the stack, starting at one,
% while negative integers refrences the stack from the top down (-1 referring
% to the value at the top of the stack).

% Due to the fact that different versions of Lua handle the global environment
% and the registry in different ways, for the sake of compatability, this
% library will not permit the explicit use of pseudo-indexes.  Instead, 
% seperate access predicates have been provided in the place of pseudo-indexes.

:- pred index(int, lua) = 

:- pred get(maybe(T)::out, lua::in, I::in) 
:- pred get(maybe(T)::out, lua::di, lua::uo, I::in)
:- func get(lua, I) = maybe(T) <= index(I) is det.
:- func get(lua::di, lua::uo, I::in) = (maybe(T)::out) 



%-----------------------------------------------------------------------------%
%
% Modifying the Lua State
%

	% Set assigns the provided value in Lua, aborts if it cannot do so.
	%
	% Index ^ set(Value, Result, !L),
	% Index ^ set(Value, L) ,
	% (Maybe = yes(Var) ; Maybe = no).
	%
:- pred set(T::in, lua::in, I::in) <= index(I) is det.
:- pred set(maybe(T)::out, lua::di, lua::uo, I::in) <= index(I) is det.
:- func set(lua, I) = maybe(T) <= index(I) is det.
:- func set(lua::di, lua::uo, I::in) = (maybe(T)::out) <= index(I) is det.

	% Call a function or closure on a unique lua_state.
	% The return values will be pushed onto the end of the stack.
	%
:- pred lua_call(function::in, lua_result::out, lua::di, lua::uo) is det. 

:- type lua_result
	--->	ok		% Successful, with no return values.
	;	ok(int)		% Successful, with the number of return values.
	;	error(string)	% Lua experienced an error

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
:- func lua_type(T) = lua_type.


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
	where equality is equal_value.

:- pred equal_value(lua.value::in, lua.value::in) is semidet.






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
%
% Lua functions
%

	
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
% Lua userdata
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
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module require.

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

%-----------------------------------------------------------------------------%


:- type lua_state.

	% The lua_state type is a literal refrence to the Lua runtime, but the
	% lua type is an abstraction of the Lua state.
	%
:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

:- type lua
	---> 	lua(lua)
	;	state(abstract_state)
	;	state(state, args).
	;	state - int
	where equality is equal_states.

:- pred equal_states(state::in, state::in) is semidet.





:- type abstract_state 
	--->	abstract_state(	
			global::global,
			registry::registry,
			upvalues::upvalues,
			stack::list(var).
		).

:- type global == var.

:- type registry == var.

:- type upvalue == var.

:- type stack ---> undefined_stack.



%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "extern void luaMR_init(lua_State *);").

:- pragma foreign_code("C", "void luaMR_init(lua_State * L) {

	/* Add tables to the registry. */
	
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
	SUCCESS_INDICATOR = luaMR_ready(L);
").


:- pragma foreign_code("C", 
"

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaMR_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, MR_LUA_MODULE);
		lua_getfield(L, 2, module_name);
		return 1;
	}
	return 0;
}

").

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

 
:- type index == int.
:- type id == int.
:- type key == var.


	% The var type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type var 
	--->	index(state, index) 
	;	ref(state, id)
	;	upvalue(state, id)
	;	key(table, key).


%-----------------------------------------------------------------------------%
%
% Lua Types
%
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

lua_type(_) = _ :- sorry($module, $pred).

:- use_module lua.value.

:- type value == lua.value.value.


equal_value(_, _) :- sorry($module, $pred).


%-----------------------------------------------------------------------------%
%
% Lua tables
%



:- type table == var.


%-----------------------------------------------------------------------------%
%
% Lua functions
%

:- type function == var.


%-----------------------------------------------------------------------------%
%
% Lua userdata
%

:- type userdata == var.


%-----------------------------------------------------------------------------%
%
% Lua thread
%

:- type thread == state.






