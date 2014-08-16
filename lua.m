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
% This file presents an interface for handling and passing values between
% compiled Mercury modules and the Lua runtime VM.
%
% Lua is known for being a lightweight, easy to write, (comparatively) fast 
% dynamically typed scripting language.  With first-class functions, lexical 
% closures, varadic argument-passing/variable-assignment, it offers a set of
% language features one might expect of a functional language, rather than an 
% imperative scripting language.  With the usage of metatables and a stack
% based C interface, Lua is extremely exstensible and easily embedded or bound
% with other languages.  This flexibility allows Lua programmers to define
% and use their own semantics, be it functional, object-oriented or otherwise.
%
%
% The Semantic gap.
%
% a Lua program can be considered a set of instructions on what to do to.
% These instructions impose changes oand when.In Lua, statements represent 
% imperative changes to the Lua state by producing side effects. In sequential 
% order, Lua evaluates each statement and modifies the Lua state to reflect the 
% truth-value intended by the statement, within the context of the local scope. 
% As such,  Instead of requiring the declaration and deletion of variables, Lua 
% uses 'nil' to represent unnasigned values. 
% 
% In Lua, 'Foo = 3' can be read as 'Foo is now the number 3'.
%
% In contrast to Lua's imperative semantics, Mercury is a purely declarative
% language.  A Mercury program can be considered a set of predicates that
% describe whether or not things are true. Mercury variables aren't containers
% for values that can change, they represent values that Mercury may not know.
% A 'free' variable is one whose value has not yet been determined.
%
% in Mercury, 'Foo = 3' can be read as 'Foo is 3', a statement that can either
% be true or false.
% 
% These are two very different ways of looking at things, and in order to 
% bridge that gap, this Library defines a means of expressing Lua in a given
% context at a specific moment in time.  That context may be the entire Lua
% state of execution, or it could only be the local scope inside a function
% call.  
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

%:- include_module state.

:- import_module io.
:- import_module float.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module char.
:- import_module list.
:- import_module map.
:- import_module univ.
:- import_module require.

%-----------------------------------------------------------------------------%
%
% The Lua State
%

	% A refrence to the Lua VM as defined by the lua_State type in lua.h
	%
:- type lua_state.

:- type lua == lua_state.

	% Typeclass defining all of the values retreivable from a Lua state.
	%
:- typeclass lua(L) where [

	% Retreive a value from Lua without invoking metamethods
	pred rawget(var, value, L),		
	mode rawget(in, out, in) is det, 	 	
	mode rawget(out, out, in) is multi,
	
	
	% Index at the top of the stack
	func top(L) = int,
	
	% Minimum allocated stack size
	func minstack(L) = int,
	
	% Retreive the version number of the Lua runtime.
	% 
	func version(L) = string
].

:- typeclass pure_lua(L) <= lua(L) where [

	% Modify variables in Lua, will not invoke metamethods
	pred rawset(var::in, value::in, L::in, L::out) is det,
	
	% Push a value onto the stack
	pred push(value::in, L::in, L::out) is det,
	
	% Pop a number of values off the stack
	pred pop(int::in, L::in, L::out) is det
	
	% Dump a function as a compiled chunk, 
	% fails if not a Lua function			
	func dump(var, L) = string is semidet,
	
	% Call the first value in the list with the rest as arguments fail if 
	% the function tries to do something impure.	
	func pure_call(values, L) = values.
	
	% Compile a chunk as a function, optionally name the chunk
	func load(string, L) = var is det,
	func load(string, string, L) = var is det,
	
	% Dynamic cast a value
	func value_of(value, L) = T is semidet,
].




%%% WARNING %%%
% be very careful when invoking these typeclass methods, doing so in a safe
:- typeclass imperative_lua(L) <= lua(L) where [
	impure pred get(var, value, L),	
	mode get(in, out, in) is semidet, 	 	
	mode get(out, out, in) is nondet
	
	impure pred set(var::in, value::in, L::in) is det,
	impure pred push(value::in, L::in) is det,
	impure pred pop(int::in, L::in) is det
].

% TODO: Abstract representation implementing imperative_lua in pure Mercury



%-----------------------------------------------------------------------------%
%
% Lua values
%

	% A union of all of the types that can be natively passed to and from
	% Lua.
	%
:- type value
	--->	nil(nil)	% the abscence of value
	;	literal(literal_value)
	;	var(var)	% Instantiated in the Lua state
	;	c_var(c_var)	% Represented by a C type
	;	m_var(m_var)	% Represented by a Mercury type
	;	error(lua_error). 
	
:- type values == list(value).

:- func value(T) = value.
:- func value_of(value, L) = T is semidet <= lua(L).
:- some [T] det_value_of(value, L) = T is det <= lua(L).
	
:- type literal_value.
	--->	number(float)	% double prescision, casts to float
	;	integer(int)	% int cast to Lua number
	;	boolean(bool)	% boolean truth values, casts to bool
	;	string(string)	% string value, casts to string
	;	char(char)	% Passed as string.
	;	chunk(string).	% A chunk of Lua code
	
:- type literals == list(literal_value).
	
:- type c_var
	--->	lightuserdata(c_pointer)	% naked C pointer
	;	userdata(userdata)	% fully allocated userdata
	;	thread(lua_state)	% A coroutine
	;	c_function(c_function). % A Lua callable function pointer

:- type c_function.	% A Lua callable function defined in C

:- type m_var
	--->	ground(univ).
	;	m_func(func(values) = values)
	;	m_function(func(lua_state) = int)
	;	free.	% Unbound variable, think promiscuous nil
		


% The nil value
%
% In Lua, nil represents the abscence of value.  Looking up a key in a Lua table 
% that is not assigned a value with produce a nil result.
%
% Furthermore, assigning a key value to nil will unassign that value. Note that 
% this does not neccicarily delete the value, if Lua holds a refrence to that
% value elsewhere, it will not be garbage collected.
%
% In normal Lua semantics, using nil as a key value produces an error, however
% due to the Mercury semantics used in this library, doing so will either fail
% or return another nil value.  This is both for the sake of safer runtime
% integration of Mercury's strict type system with Lua's dynamic type system,
% and also as a practical consideration of Mercury's potentially
% nondeterministic nature, as testing for a paticular type wil result in a
% backtracking failure.
%
% It is to be noted that Lua's nil value is not to be confused with C's NULL
% value.  While used in similar ways, Lua will interpret C's NULL as the number
% zero, wheras C has no direct representation for Lua's nil value.
%
% As a result of this, Lua's semantics on conditional tests are slightly
% different than C's.   C interprets any numeric value other than 0 as true.
% In contrast, Lua interprets ANY value other than boolean false or nil as true.

:- type nil ---> nil.

%-----------------------------------------------------------------------------%
%
% Lua variables
%

% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.

:- type var.

:- type vars == list(var).

:- type index. 		% stack index
:- type upvalue. 	% upvalue index

% The ref type represents a strong refrence to a Lua variable instantiated in
% Lua, as a result, a refrenced variable will not be garbage collected by Lua
% until said refrence is unregistered or re-assigned.
%
% Note that these refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  

	
	% The ref type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type ref.


%-----------------------------------------------------------------------------%
%
% Lua environment
%

:- type scope(L)
	---> 	global(
			L, 	% The state that this scope represents
			int,	% Count of arguments on bottom of stack
		),
	;	local(
			scope,	% parent scope
			int	% number of values allocated for this scope
		),
	;	top(index).	% index to start triggering automatic use of
				% lua_checkstack. 
	
:- type scope == scope(lua_state).
	
:- instance lua(scope(L)) <= lua(L).
:- instance pure_lua(scope(L)) <= pure_lua(L).
:- instance pure_lua(scope(L)) <= imperative_lua(L). 

	% Retreive the number of arguments passed to a Lua state.
	%
:- func arg_count(L) = int <= lua(L).

	% Declaratively access the arugments passed to a Lua state with dynamic
	% casting.
	%
:- pred args(int, T, L) <= lua(L).
:- mode args(in, out, in) is semidet.
:- mode args(out, out, in) is nondet.

	% Declaratively access the arugments passed to a Lua state with static
	% casting. If no arguments are passed, det_args will behave as if a
	% single nil value was passed.
	%
:- some [T] pred det_args(int, T, L) <= lua(L).
:- mode det_args(in, out, in) is det.
:- mode det_args(out, out, in) is multi.

	% Dynamic cast a specific argument.
	%
:- func arg(int, lua_state) = T is semidet <= lua(L).

	% Static cast a specific argument.
	%
:- some [T] func det_arg(int, L) = T <= lua(L).





%-----------------------------------------------------------------------------%
%
% Lua types
%	

:- type lua_type
	--->	none			% rarely used, represents invalid type
	;	nil_type		
	;	number_type		
	;	boolean_type		
	;	string_type		
	;	lightuserdata_type	
	;	function_type		
	;	table_type		
	;	thread_type		
	;	userdata_type.		
	
	% Look up the Lua type of a given variable. 
	% 
:- func lua_type(T) = lua_type.



%-----------------------------------------------------------------------------%
%
% Lua errors
%
	% Thrown when Lua experiences an error.
	%
:- type lua_error
	---> 	lua_error(error_type, string).

:- type error_type
	--->	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lua.state.

:- import_module type_desc.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module require.

:- pragma require_feature_set([conservative_gc, double_prec_float]). 

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define MR_LUA_MODULE ""MR_LUA_MODULE""
#define MR_LUA_UDATA ""MR_LUA_UDATA_METATABLE""
#define MR_LUA_READY ""MR_LUA_IS_READY""
#define MR_LUA_STATE ""MR_LUA_STATE_ID""

#define MR_LUA_TYPE ""__mercury_type""

").

	% Succeed if the given value is NULL.
	%
:- pred null(T::in) is semidet.

:- pragma foreign_proc("C", null(T::in),
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = (T == NULL);").

%-----------------------------------------------------------------------------%
%
% The Lua state
%



% The lua type represents the state of a running Lua Virtual Machine. (Lua VM
% for short) Note that as a convention borrowed from the C API, procedures 
% that query or manipulate the Lua state will use the variable term 'L' to refer 
% to the Lua state.

:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

	% For the sake of safety and sanity checking, produce a lua_state_ptr
	% instantiated as a NULL pointer.
	%
:- func null_state = lua_state.

:- pragma foreign_proc("C", null_state = (Null::out), 
	[promise_pure, will_not_call_mercury], 
	"Null = NULL;").

% WARNING! Refrences to Lua types (tables, functions, userdata) derived
% from one global lua_state are NOT compatible with other se+perately created
% lua_states. The only exception to this is lua_states created as threads.
% lua_threads may freely pass variables to or from their parent state and
% sibling threads.


%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "extern void luaMR_init(lua_State *);").

:- pragma foreign_code("C", 
"
void luaMR_getregistry(lua_State * L, const char * k) {
	lua_getfield(L, LUA_REGISTRYINDEX, k);
}

void luaMR_setregistry(lua_State * L, const char * k) {
	lua_setfield(L, LUA_REGISTRYINDEX, k);
}

void luaMR_getupvalue(lua_State * L, const int id) {
	lua_pushvalue(L, lua_upvalueindex(id));
}

void luaMR_setupvalue(lua_State * L, const int id) {
	lua_replace(L, lua_upvalueindex(id));
}

void luaMR_init(lua_State * L) {
	
	/* set the given Lua state as the current Lua state */
	
	luaMR_current_lua_state = L;
	
	/* Add tables to the registry. */
	
	lua_newtable(L);
	luaMR_setregistry(L, MR_LUA_MODULE);

	/* TODO: Define and export luaMR_userdata_metatable. */
	luaMR_userdata_metatable(L);
	luaMR_setregistry(L, MR_LUA_UDATA);
	
	

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
	luaMR_setregistry(L, MR_LUA_READY);
} 

").

:- pred init(lua_state::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").

:- pragma foreign_decl("C", "int luaMR_ready(lua_State *);").

:- pragma foreign_code("C", 
"
	/* Check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
		lua_checkstack(L, 1);
		luaMR_getregistry(L, MR_LUA_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}

").

:- pred ready(lua_state::in) is semidet.

:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").

:- pred ready(lua_state::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", ready(L::in, Answer::out, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	if(luaMR_ready(L))
		Answer = MR_YES;
	else
		Answer = MR_NO;

").



:- pragma foreign_code("C", 
"

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaMR_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		luaMR_getregistry(L, MR_LUA_MODULE);
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

:- type var
	--->	local(index)	% An index on the the local stack
	;	ref(ref)	% A strong refrence (like a pointer)
	;	up(upvalue)	% An upvalue pushed onto a C closure
	;	index(var, value)	% Value stored in a table or metamethod
	;	metatable(var)	% A variable's metatable
	;	env(var)	% a variable's environment
	;	global(string)	% A global variable
	;	global		% The global environment
	;	registry(string) % A registry entry
	;	registry	% The registry
	;	invalid(string).

:- type index == int.
:- type upvalue ---> upvalue(int).

%-----------------------------------------------------------------------------%
%
% Refrences
%

:- pragma foreign_type("C", ref, "luaMR_Ref", [can_pass_as_mercury_type]).

:- pragma foreign_code("C",
"
typedef int * luaMR_Ref;


/* Creates a new refrence from the stack */
luaMR_Ref luaMR_new_ref(lua_State * L, int index) {
	lua_pushvalue(L, index);
	luaMR_Ref new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, luaMR_finalize_ref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void luaMR_push_ref(lua_State * L, luaMR_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaMR_finalize_ref(luaMR_Ref ref, lua_State * L) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}

"). 

%-----------------------------------------------------------------------------%
%
% Lua Types
%
:- pragma foreign_enum("C", lua_type/0, 
[
	none 		- 	"LUA_TNONE",
	nil_type 	- 	"LUA_TNIL",
	boolean_type 	- 	"LUA_TBOOLEAN",
	lightuserdata_type - 	"LUA_TLIGHTUSERDATA",
	number_type 	- 	"LUA_TNUMBER",
	string_type 	- 	"LUA_TSTRING",
	table_type 	- 	"LUA_TTABLE",
	function_type 	- 	"LUA_TFUNCTION",
	userdata_type 	- 	"LUA_TUSERDATA",
	thread_type 	- 	"LUA_TTHREAD"
]).

%-----------------------------------------------------------------------------%
%
% Lua errors
%

:- pragma foreign_enum("C", error_type,
[
	runtime_error 	-	"LUA_ERRRUN",
	syntax_error 	-	"LUA_ERRSYNTAX",
	memory_error	-	"LUA_ERRMEM",
	unhandled_error	-	"LUA_ERRERR"
]). 


