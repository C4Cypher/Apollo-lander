%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: apollo.m
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

:- module apollo.


:- interface.

:- include_module state.
:- include_module api.

% Note: The impure operations defined in the api and modules are used to 
% implement this library, however they do not fully conform to the semantics of 
% the prodedures in this library.  Semipure procedures should be safe to be 
% called without special consideration. However; the impure prodedures in the
% api module WILL produce side effects that will produce undefined behavior in
% this library if they are not properly implemented. Use the api module at your
% own risk.

:- import_module io.
:- import_module float.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module univ.


%-----------------------------------------------------------------------------%
%
% The Lua State in an impure context
%

	% A refrence to the Lua VM as defined by the lua_State type in lua.h
	%
:- type lua.

:- mode li == in(bound(lua)).
:- mode lo == out(bound(lua)).



%-----------------------------------------------------------------------------%
%
% The Lua State as a mutable state variable
%

	% A refrence to the Lua state meant to be passed in a safe, 
	% declarative manner. 
	%
:- type lua_state. 	

	% Abbriviations for lua_state.
	%
:- type ls == lua_state.

	% Create a new Lua state.
	%
:- pred new_state(ls::uo) is det.

:- func new_state = lua_state.
:- mode new_state = uo is det.







%-----------------------------------------------------------------------------%
%
% Initializing the Lua state
%

	% Set up the Lua state so that it has all of the assigned values
	% Mercury needs to interact with it.
	%
:- pred init_lua(lua::in, io::di, io::uo) is det.
:- pred init_lua(lua_state::di, lua_state::uo) is det.

	% Check to see if lua_init has been called on a Lua state.
	%
:- semipure pred ready(lua::in) is semidet.

:- pred ready(bool, ls, ls).
:- mode ready(out, di, uo) is det.
:- mode ready(out, mdi, muo) is det.


:- pred ready(ls::mdi, ls::muo) is semidet.

%-----------------------------------------------------------------------------%
%
% Lua variables and values
%




% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.

:- type var
	--->	local(index)	% An index on the the local stack
	;	index(value, var)	% Value stored in a table
	;	meta(var)	% A variable's metatable
	
	% The following are meant for internal use
	;	ref(ref)	% A strong refrence (like a pointer)
	;	global(string) 	% A global variable
	
	% Returned on invalid request.
	;	invalid(string).




	% Var ^ T = index(value(T), Var).
	% Syntactic sugar for accessing the elements of a table, 
	% assuming Var is a Table.  If Var is NOT a table, Lua may respond with a 
	% runtime error when it gets passed a variable constructed in this manner.
	%
:- func var ^ T = var. 

:- type vars == list(var).

	% A given var is valid.
:- pred valid_var(var, ls, ls).
:- mode valid_var(in, mdi, muo) is semidet.

:- pred valid_var(var, bool, ls, ls).
:- mode valid_var(in, out, di, uo) is det.
:- mode valid_var(in, out, mdi, muo) is det.


	% Test equality on vars (no metamethods)
:- pred var_equal(var::in, var::in, ls::mdi, ls::muo) is semidet.


% The ref type represents a strong refrence to a Lua variable instantiated in
% Lua, as a result, a refrenced variable will not be garbage collected by Lua
% until said refrence is unregistered or re-assigned.
%
% Note that these refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  

:- type ref.

% The index type is used to directly refrence variables on the Lua stack

:- type index.

%:- func ref(var) = ref.


	% A union of all of the types that can be natively passed to and from
	% Lua.
	%
:- type value
	--->	nil(nil)	% the abscence of value
	;	number(float)	% double prescision, casts to float
	;	integer(int)	% int cast to Lua number
	;	boolean(bool)	% boolean truth values, casts to bool
	;	string(string)	% string value, casts to string
	;	lightuserdata(c_pointer)	% naked C pointer
	;	thread(lua)	% A coroutine
	;	c_function(c_function) % A Lua callable function pointer
	;	var(var)	% A Lua variable
	;	userdata(univ)	% opaque type in Lua for handling foreign data
	;	lua_error(lua_error). % May be returned from a Lua function on error.
		
:- type values == list(value).

	% values are ground data types that can be cast back and forth from mercury 
	% types without help from the Lua runtime. The cc_nondet modes should
	% properly cast ints and floats via backtracking.
	%
:- func value(T) = value.
:- mode value(in) = out is det.
:- mode value(out) = in is cc_nondet.

:- pred value(value, T).
:- mode value(out, in) is det.
:- mode value(in, out) is cc_nondet.

	% A Lua callable function defined in C and refrenced via pointer
:- type c_function.	

	% Test equality on values (no metamethods)
:- pred value_equal(value::in, value::in, ls::mdi, ls::muo) is semidet.

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

	% Retreive the value of a var in Lua without triggering metatables.  
	%
:- pred get(var, value, ls, ls).
:- mode get(in, out, di, uo) is det.
:- mode get(in, out, mdi, muo) is det.

:- func get(var, ls, ls) = value.
:- mode get(in, di, uo) = out is det.
:- mode get(in, mdi, muo) = out is det.

	% Change the value of a var in Lua
	% These calls are now backtrackable due to the trailing module
	%
:- pred set(var, value, ls, ls).
:- mode set(in, in, di, uo) is det.
:- mode set(in, in, mdi, muo) is det.
 

	% Change the value of a var in Lua, making sure to not trigger metatables.
	%

	% Create a new variable local to the environment initial value will be nil
	% if the lua state is mostly unique the variable is trailed and will be 
	% undone on backtrack.
	%
:- pred local(var, ls, ls).
:- mode local(out, di, uo) is det.
:- mode local(out, mdi, muo) is det.

:- func local(ls, ls) = var.
:- mode local(di, uo) = out is det.
:- mode local(mdi, muo) = out is det.


%-----------------------------------------------------------------------------%
%
% Lua tables
%	

	% Create new Lua table and pass it as a local.
	%
:- func local_table(ls, ls) = var.
:- mode local_table(di, uo) = out is det.
:- mode local_table(mdi, muo) = out is det.

:- pred local_table(var, ls, ls).
:- mode local_table(out, di, uo) is det.
:- mode local_table(out, mdi, muo) is det. 


	% Create new Lua table and pass it to Mercury as a refrence
	%
:- func ref_table(ls, ls) = var.
:- mode ref_table(di, uo) = out is det.
:- mode ref_table(mdi, muo) = out is det.

:- pred ref_table(var, ls, ls).
:- mode ref_table(out, di, uo) is det.
:- mode ref_table(out, mdi, muo) is det.

	% The first key-value pair from a table, fails if var is not a table or
	% if the table is empty.
	%
:- pred first(var, value, value, ls, ls).
:- mode first(in, out, out, mdi, muo) is semidet.
 
	% Det version, returns nil values if table is empty or var is not a table
:- pred det_first(var, value, value, ls, ls).
:- mode det_first(in, out, out, di, uo) is det.
:- mode det_first(in, out, out, mdi, muo) is det.

	% Accepts a key value and returns the next key value pair when iterating
	% over a table. Fails if there is no next pair.
	%
:- pred next(var, value, value, value, ls, ls).
:- mode next(in, in, out, out, mdi, muo) is semidet.

	% Det version of next, nil values returned if there is no next pair
	%
:- pred det_next(var, value, value, value, ls, ls).
:- mode det_next(in, in, out, out, di, uo) is det.
:- mode det_next(in, in, out, out, mdi,  muo) is det.


%-----------------------------------------------------------------------------%
%
% Lua functions
%	



	% This is the type signature for mercury predicates that can be called as
	% Lua functions.  Unless you're familiar with the calls in apollo.api, please
	% use the provided constructor functions to create mr_pred values.
	%
:- type mr_func == (impure func(lua)= int).

:- inst mr_func == (func(in) = out is det).

:- mode mri == in(mr_func).
:- mode mro == out(mr_func).


	% Accepts a Mercury function that takes a list of lua variables and 
	% returns a list of lua variables. If a semidet function fails, then
	% the pred will return nil to lua.
	%
:- func make_lua_func(func(vars, ls, ls) = vars) = mr_func.
:- mode make_lua_func(in(func(in, di, uo) = out is det)) = mro is det.
:- mode make_lua_func(in(func(in, di, uo) = out is semidet)) = mro is det.

	% Acceps a Mercury function that takes a list of variables and returns
	% one Lua variable. In Lua, the function will return nil on failure, or
	% if the function finds multiple solutions, they will all be returned
	%
:- func make_nondet_lua_func(func(vars, ls, ls) = var) = mr_func.
:- mode make_nondet_lua_func(in(func(in, di, uo) = out is det)) = mro is det.
:- mode make_nondet_lua_func(in(func(in, di, uo) = out is semidet)) 
  = mro is det.
:- mode make_nondet_lua_func(in(func(in, di, uo) = out is multi)) = mro is det.
:- mode make_nondet_lua_func(in(func(in, di, uo) = out is nondet)) 
  = mro is det.


	% Accepts a string chunk of lua code and compiles it to a lua function, 
	% passing it by refrence. If the compile fails, a refrecnce to a lua_error    
	% userdata object will be returned instead.
	%  
:- pred string_to_func(string, var, ls, ls).
:- mode string_to_func(in, out, di, uo) is det.

:- func string_to_func(string, ls, ls) = var.
:- mode string_to_func(in, di, uo) = out is det.

	% Calls a lua variable as if it were a function, this may be unsafe if the
	% variable is not a function or does not have a __call metamethod defined
	%
:- pred call_lua_func(var, values, values, ls, ls).
:- mode call_lua_func(in, in, out, di, uo) is det.

:- func call_lua_func(var, values, ls, ls) = values.
:- mode call_lua_func(in, in, di, uo) = out is det.







%-----------------------------------------------------------------------------%
%
% Lua modules
%	

	% register_module(Module, L, !IO).
	%
	% Register a module in Lua.
	%
%:- pred register_module(string::in, lua_func::in, lua::in,
%	io::di, io::uo) is det.


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
:- pred var_type(var, lua_type, ls, ls).
:- mode var_type(in, out, di, uo) is det.
:- mode var_type(in, out, mdi, muo) is det.

:- func var_type(var, ls, ls) = lua_type.
:- mode var_type(in, di, uo) = out is det.
:- mode var_type(in, mdi, muo) = out is det.

	

%-----------------------------------------------------------------------------%
%
% Lua errors
%
	% Thrown when Lua experiences an error.
	%
:- type lua_error
	---> 	lua_error(error_type, string).

:- type error_type
	--->	no_error
	;	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.
	

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module apollo.api.
:- import_module apollo.state.

:- pragma foreign_import_module("C", apollo.api).
:- pragma foreign_import_module("C", apollo.state).

:- import_module char.
:- import_module solutions.
:- import_module exception.
:- import_module type_desc.
:- import_module require.


:- pragma require_feature_set([conservative_gc, trailing]). 

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <mercury_memory.h>

/* Checking for Lua language features introduced with 5.2 */
#if LUA_VERSION_NUM >= 502

#define AFTER_502

#else /*  LUA_VERSION_NUM < 502 */ 

#define BEFORE_502

#endif /* LUA_VERSION_NUM < 502 */ 

#ifdef BEFORE_502
#define LUA_RIDX_MAINTHREAD     1
#define LUA_RIDX_GLOBALS        2
#define LUA_RIDX_LAST           LUA_RIDX_GLOBALS
#endif /* BEFORE_502 */

#define LUA_MR_MODULES ""LUA_MR_MODULES""
#define LUA_MR_READY ""LUA_MR_READY""

/* metatable values*/
#define LUA_MR_TYPE ""__mercury_type""
#define LUA_MR_USERDATA ""__mercury_userdata""



").



%-----------------------------------------------------------------------------%
%
% The Lua State 
%



% The lua type represents the state of a running Lua Virtual Machine. (Lua VM
% for short) Note that as a convention borrowed from the C API, procedures 
% that query or manipulate the Lua state will use the variable term 'L' to refer 
% to the Lua state.

:- pragma foreign_type("C", lua, "lua_State *",
	[can_pass_as_mercury_type]).

:- pragma foreign_type("C", lua_state, "apollo_lua_state *").



new_state = lua_state(lua_new, null_id, empty_trail).

new_state(new_state).

%-----------------------------------------------------------------------------%
%
% Initializing the Lua state
%

:- pragma foreign_proc("C", init_lua(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "apollo_init(L);").



init_lua(ls(L, I, T), ls(L, I, T)) :- promise_pure( impure init_lua(L)).

:- impure pred init_lua(lua::in) is det.

:- pragma foreign_proc("C", init_lua(L::in), 
	[may_call_mercury], "apollo_init(L);").


:- pragma foreign_proc("C", ready(L::in), 
	[promise_semipure, will_not_call_mercury], "
	SUCCESS_INDICATOR = apollo_ready(L);
").


ready(lua_state(L, I, T), lua_state(L, I, T)) :- 
  promise_pure (semipure ready(L) ).

ready(B, ls(L, I, T), ls(L, I, T)) :- 
	promise_pure 
	( semipure ready(L) -> B = yes
	; B = no).

:- pragma foreign_decl("C", "void apollo_init(lua_State *);").

:- pragma foreign_decl("C", "int apollo_ready(lua_State *);").

:- pragma foreign_code("C", "
void apollo_init(lua_State * L) 
{
	
	int length;
	
#ifdef BEFORE_502
	
	/* Set the Main thread in the registry */
	lua_pushvalue(L, LUA_REGISTRYINDEX);
	lua_pushinteger(L, LUA_RIDX_MAINTHREAD); 
	if(!lua_pushthread(L))
		MR_fatal_error(""Must init main thread."");
	lua_settable(L, -3);
	
	lua_pushinteger(L, LUA_RIDX_GLOBALS);
	lua_pushvalue(L, LUA_GLOBALSINDEX);
	lua_settable(L, -3);
 
 	
#endif /* BEFORE_502 */

	/* Add tables to the registry. */
	
	lua_newtable(L);
	apollo_setregistry(L, LUA_MR_MODULES);
	
	/* Add loader to package.loaders */
	lua_getglobal(L, ""package""); 
  lua_getfield(L, -1, ""loaders""); 
  length = apollo_len(L, 1);
	lua_pushinteger(L, length + 1); 
	lua_pushcfunction(L, apollo_loader); 
	lua_settable(L, -3);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	apollo_setregistry(L, LUA_MR_READY);
} 

").



:- pragma foreign_code("C", "
	/* Check to see if Lua has already been initialized. */
	int apollo_ready(lua_State * L) {
    int ready;
	
		lua_checkstack(L, 1);
		lua_pushvalue(L, LUA_REGISTRYINDEX);
		lua_pushstring(L, LUA_MR_READY);
		lua_gettable(L, -2);
		ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").


%-----------------------------------------------------------------------------%
%
% Variables
%



:- type index == int.
	
Var ^ T = index(value(T), Var). 

valid_var(V, ls(L, I, T), ls(L, I, T)) :- 
	semipure valid_var(V, L). 
		
:- pragma promise_pure(valid_var/3).

valid_var(Var, Valid, ls(L, I, T), ls(L, I, T)) :- 
			if semipure valid_var(Var, L) 
				then Valid = yes 
				else Valid = no.
				
:- pragma promise_pure(valid_var/4).


/* Yeah no, I'll have to revisit this later

table(Table, Acc, !AccVar, ls(L, I, T), ls(L, I, T)) :- 
	Pred = (pred(A::in, Lua::in, { B, C }::out) is nondet :- 
		promise_pure 
		semipure table(A, B, C, Lua)
		),
	AccPred = (pred({A, B}::in, C::in, D::out) is det :- Acc(A, B, C, D) ),
	unsorted_aggregate(Pred(Table, L), AccPred, !AccVar).
	
:- pragma promise_pure(table/6).

*/
	 	
	

var_type(V, T, ls(L, I, Tr), ls(L, I, Tr)) :-
	semipure var_type(V, T, L).

:- pragma promise_pure(var_type/4).

var_type(V, !L) = T :- var_type(V, T, !L).
	

var_equal(V1, V2, ls(L, I, T), ls(L, I, T)) :-
	semipure var_equal(V1, V2, L).
		
:- pragma promise_pure(var_equal/4).


%-----------------------------------------------------------------------------%
%
% Refrences
%



:- pragma foreign_type("C", ref, "apollo_Ref", [can_pass_as_mercury_type]).

:- pragma foreign_decl("C", "

	typedef int * apollo_Ref;
	
	apollo_Ref apollo_newref(lua_State *, int);
	void apollo_pushref(lua_State *, apollo_Ref);
	void apollo_finalizeref(lua_State *, apollo_Ref);
").

:- pragma foreign_code("C",
"

/* Creates a new refrence from the stack */
apollo_Ref apollo_newref(lua_State * L, int index) {
  apollo_Ref new_ref;
	lua_pushvalue(L, index);
	new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, 
		(GC_finalization_proc)apollo_finalizeref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void apollo_pushref(lua_State * L, apollo_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, *ref);
	}
}

/* Remove Lua's refrence to the var in the registry */
void apollo_finalizeref(lua_State * L, apollo_Ref ref) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}

"). 

:- impure pred finalizeref(ref::in, lua::in) is det.

:- pragma foreign_proc("C", finalizeref(R::in, L::in), 
	[will_not_call_mercury], "apollo_finalizeref(L, R);").




%-----------------------------------------------------------------------------%
%
% Lua values
%


value(T::in) = (
	( dynamic_cast(T, U:nil) -> nil(U)
	; dynamic_cast(T, U:float) -> number(U)
	; dynamic_cast(T, U:int) -> number(float(U))
	; dynamic_cast(T, U:int) -> integer(U)
	; dynamic_cast(T, U:bool) -> boolean(U)
	; dynamic_cast(T, U:string) -> string(U)
	; dynamic_cast(T, U:char) -> string(string.from_char(U))
	; dynamic_cast(T, U:c_pointer) -> lightuserdata(U)
	; dynamic_cast(T, U:lua) -> thread(U)
	; dynamic_cast(T, U:c_function) -> c_function(U)
	; dynamic_cast(T, U:var) -> var(U)
	; dynamic_cast(T, U:univ) -> userdata(U)
	; userdata(univ(T))
	)::out).
		

value(T::out) = (V::in) :-
	require_complete_switch [V]
	( V = nil(N) ,  dynamic_cast(N, T)
	; V = number(F) , dynamic_cast(F, T)
	; V = integer(I) , dynamic_cast(I, T)
	; V = boolean(B) , dynamic_cast(B, T)
	; V = string(S) , dynamic_cast(S, T)
	; V = lightuserdata(P) , dynamic_cast(P, T)
	; V = thread(L) , dynamic_cast(L, T)
	; V = c_function(F) , dynamic_cast(F, T)
	; V = var(Var) , dynamic_cast(Var, T)
	; V = userdata(U) , dynamic_cast(U, T)
	; V = lua_error(E) , dynamic_cast(E, T)
	; V = userdata(univ(U)) , dynamic_cast(U, T)
	).
	



:- pragma promise_pure(value/1).

value(V, T) :- value(T) = V.




:- pragma foreign_type("C", c_function, "lua_CFunction").



value_equal(V1, V2, ls(L, I, T), ls(L, I, T)) :-
	semipure value_equal(V1, V2, L).

:- pragma promise_pure(value_equal/4).


%-----------------------------------------------------------------------------%
%
% Get and Set
%


	% Retreive the value of a var in Lua
	%
%:- pred get(var, value, ls, ls).
%:- mode get(in, out, di, uo) is det.
%:- mode get(in, out, mdi, muo) is det.

get(Var,Value,ls(L, I, T), ls(L, I, T)) :-
  impure push_var(Var, L),
  semipure Value = to_value(-1, L),
  impure lua_pop(1, L).

:- pragma promise_pure(get/4).

get(Var, !L) = Value :- get(Var, Value, !L).

	% Change the value of a var in Lua
	% Although these calls are considered pure due to the passing of the lua_state
	% Mercury can not backtrack through these or other calls that modify the 
	% Lua state.
	%
%:- pred set(var, value, ls, ls).
%:- mode set(in, in, di, uo) is det.
%:- mode set(in, in, mdi, uo) is det.

set(V::in, Value::in, ls(L, Ix, T)::di, ls(L, Ix, T)::uo) :-
  require_complete_switch [V] (
	  V = local(I), 
      impure push_value(Value, L),
      impure lua_replace(I, L)
    ; V = index(Key, Table),
      impure push_var(Table,L),
      impure push_value(Key, L),
      impure push_value(Value, L),
      impure lua_rawset(-3, L),
      impure lua_pop(1, L)
    ; V = meta(Table),
      impure push_var(Table, L), 
      impure push_value(Value, L),
      impure lua_setmetatable(-2, L),
      impure lua_pop(1, L)
    ; V = ref(R),
      (dynamic_cast(R, I:int) -> 
        impure lua_pushinteger(I, L),
        impure push_value(Value, L),
        impure lua_rawset(registryindex, L)
      ;
        throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
        " attempted to set invalid ref."))
      )       
    ; V = global(S),
      impure lua_pushstring(S, L),
      impure push_value(Value, L),
      impure lua_rawset(globalindex, L)
    ; V = invalid(S),
      throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
      " attempted to set invalid var: " ++ S))
    ).



%:- mode set(in, in, mdi, muo) is det.

set(V::in, Value::in, LS0::mdi, LS1::muo) :-

  LS0 = ls(L, I0, T0),
  
  
  require_complete_switch [V] (
    V = local(I),
    
      semipure OldValue = to_value(I, L),
      trail_lua_closure(revert_local(I, OldValue) , ls(L, I0, T0), ls(L1, I1, T1) ),
     
      impure push_value(Value, L),
      impure lua_replace(I, L),
    
      LS1 = ls(L1, I1, T1)
    
    ; V = index(Key, Table),
      impure push_var(Table,L),
      impure push_value(Key, L),
      impure lua_rawget(-2, L),
      semipure OldValue = to_value(-1, L),
      impure lua_pop(1, L),
    
      trail_lua_closure(revert_table(Key, Table, OldValue), ls(L, I0, T0), ls(L1, I1, T1) ),
    
      impure push_value(Key, L), %?
      impure push_value(Value, L),
      impure lua_rawset(-3, L),
      impure lua_pop(1, L),
    
      LS1 = ls(L1, I1, T1)
    
    ; V = meta(Table),
      impure push_var(Table, L), 
    
      ( impure lua_getmetatable(-1, L) -> true ; impure lua_pushnil(L)), 
      semipure OldTable = to_value(-1, L),
      impure lua_pop(1, L),
    
      trail_lua_closure(revert_metatable(Table, OldTable) , ls(L, I0, T0), ls(L1, I1, T1) ),
    
      impure push_value(Value, L),
      impure lua_setmetatable(-2, L),
      impure lua_pop(1, L),
    
      LS1 = ls(L1, I1, T1)
    
    ; V = ref(R),
      ( dynamic_cast(R, I:int) -> 
        impure lua_pushinteger(I, L),
      
        impure lua_rawget(registryindex, L),
        semipure OldValue = to_value(-1, L),
        impure lua_pop(1, L),
      
        trail_lua_closure(revert_ref(I, OldValue), ls(L, I0, T0), ls(L1, I1, T1) ),
      
        impure push_value(Value, L),
        impure lua_rawset(registryindex, L)
      ;
        throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
        " attempted to set invalid ref."))
      ),
      LS1 = ls(L1, I1, T1)
    
    ; V = global(S),
      impure lua_pushstring(S, L),
    
      impure lua_rawget(globalindex, L),
      semipure OldValue = to_value(-1, L),
      impure lua_pop(1, L),
    
      trail_lua_closure(revert_global(S, OldValue), ls(L, I0, T0), ls(L1, I1, T1) ),
    
      impure push_value(Value, L),
      impure lua_rawset(globalindex, L),
    
      LS1 = ls(L1, I1, T1)
    
    ; V = invalid(S),
      throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
      " attempted to set invalid var: " ++ S)),
      I0 = I1, T0 = T1, LS1 = ls(L, I1, T1)
    ).
  
:- impure func revert_local(index, value, lua) = int.
 
revert_local(I, OldValue, L) = 0 :- 
  impure push_value(OldValue, L),
  impure lua_replace(I, L).
  
:- impure func revert_table(value, var, value, lua) = int.
 
revert_table(Key, Table, OldValue, L) = 0 :- 
      impure push_var(Table,  L),
      impure push_value(Key, L),
      impure push_value(OldValue, L),
      impure lua_rawset(-3, L),
		  impure lua_pop(1, L).
      
      
:- impure func revert_metatable(var, value, lua) = int.

revert_metatable(Table, OldTable, L) = 0 :-
      impure push_var(Table, L),
      impure push_value(OldTable, L),
      impure lua_setmetatable(-2, L),
      impure lua_pop(1, L).

:- impure func revert_ref(int, value, lua) = int.
      
revert_ref(I, OldValue, L) = 0 :-
        impure lua_pushinteger(I, L),
        impure push_value(OldValue, L),
        impure lua_rawset(registryindex, L).
        
:- impure func revert_global(string, value, lua) = int.
        
revert_global(S, OldValue, L) = 0 :-
      impure lua_pushstring(S, L),
      impure push_value(OldValue, L),
      impure lua_rawset(globalindex, L).

:- pragma promise_pure(set/4).

	% Create a new variable local to the environment initial value will be nil
	%
%:- pred local(var, ls, ls).
%:- mode local(out, di, uo).

local(local(I)::out, ls(L, Ix, T)::di, ls(L, Ix, T)::uo) :- 
  semipure Top = lua_gettop(L),
  I = Top + 1,
  impure lua_settop(I, L).
  
local(local(I)::out, ls(L, Ix, T)::mdi, LSout::muo) :- 
  semipure Top = lua_gettop(L),
  I = Top + 1,
  impure lua_settop(I, L),
  trail_lua_closure(pop_one, ls(L, Ix, T), LSout).
      
:- impure func pop_one(lua) = int is det.

pop_one(L) = 0 :- impure lua_pop(1, L).   

:- pragma promise_pure(local/3).

%:- func local(ls, ls) = var.
%:- mode local(di, uo) = out is det.

local(L1, L2) = V :- local(V, L1, L2).


%-----------------------------------------------------------------------------%
%
% Lua tables
%	

	% Create new Lua table and pass it as a local.
	%
%:- func local_table(ls, ls) = var.
%:= mode local_table(di, uo) = out is det.

%:- pred local_table(var, ls, ls).
%:- pred local_table(out, di, uo) is det.

local_table(local(I)::out, ls(L, Ix, T)::di, ls(L, Ix, T)::uo) :-
  impure lua_newtable(L), 
  semipure I = absolute(-1, L).
  
local_table(local(I)::out, ls(L, I0, T0)::mdi, L1::muo) :-
  impure lua_newtable(L), 
  semipure I = absolute(-1, L),
  trail_lua_closure(pop_one, ls(L, I0, T0), L1).
  
:- pragma promise_pure(local_table/3).

local_table(!L) = V :- local_table(V, !L). 


	% Create new Lua table and pass it to Mercury as a refrence
	%
%:- func ref_table(ls, ls) = var.
%:- mode ref_table(di, uo) = out is det.
%:- mode ref_table(mdi, muo) = out is det.

%:- pred ref_table(var, ls, ls).
%:- mode ref_table(out, di, uo) is det.
%:- mode ref_table(out, mdi, muo) is det.

ref_table(ref(Ref)::out, ls(L, I, T)::di, ls(L, I, T)::uo):-
  impure lua_newtable(L),
  semipure Ref = lua_toref(-1, L),
  impure lua_pop(1, L).
  
ref_table(ref(Ref)::out, ls(L, I, T)::mdi, L1::muo):-
  impure lua_newtable(L),
  semipure Ref = lua_toref(-1, L),
  trail_lua_closure((impure func(Lu) = 0 :- impure finalizeref(Ref, Lu)), ls(L, I, T), L1).
  
:- pragma promise_pure(ref_table/3).
          
ref_table(!L) = V :- ref_table(V, !L).

:- pred pure_next(var::in, value::in, value::out, value::out, lua::in) 
	is semidet.

pure_next(Table, Last, Next, Value, L) :-
	impure push_var(Table, L),
	impure push_value(Last, L),
	impure lua_next(-2, L) ->
		semipure Next = to_value(-2, L),
		semipure Value = to_value(-1, L),
		impure lua_pop(3, L)
	; impure lua_pop(1, L), fail.

:- pragma promise_pure(pure_next/5).

	% The first key-value pair from a table, fails if the table is empty.
	%
%:- pred first(var, value, value, ls, ls).
%:- mode first(in, out, out, mdi, muo) is semidet.
first(Table, Key, Value, !L) :- next(Table, nil(nil), Key, Value, !L).

	% Det version, returns nil values if table is empty
%:- pred det_first(var, value, value, ls, ls).
%:- mode det_first(in, out, out, di, uo) is det.
%:- mode det_first(in, out, out, mdi, muo) is det.
det_first(Table, Key, Value, !L) :- det_next(Table, nil(nil), Key, Value, !L).

	% Accepts a key value and returns the next key value pair when iterating
	% over a table. Fails if there is no next pair. If a nil value is passed
	% as the key value, the first pair is passed instead.
	%
%:- pred next(var, value, value, value, ls, ls).
%:- mode next(in, in, out, out, mdi, muo) is semidet.
next(Table, Last, Next, Value, ls(L, I, T), ls(L, I, T)) :-
	pure_next(Table, Last, Next, Value, L).


	% Det version of next, nil values returned if there is no next pair
	%
%:- pred det_next(var, value, value, value, ls, ls).
%:- mode det_next(in, in, out, out, di, uo) is det.
%:- mode det_next(in, in, out, out, mdi,  muo) is det.
det_next(Table, Last, Next, Value, ls(L, I, T), ls(L, I, T)) :-
	if pure_next(Table, Last, K, V, L)
	then Next = K, Value = V
	else Next = nil(nil), Value = nil(nil).
	
%-----------------------------------------------------------------------------%
%
% Functions
%

:- semipure func get_args(index, vars) = vars.

 get_args(I, Old) = New :-
    I = 1 -> New = [local(1) | Old ]         
  ;
    semipure get_args(I - 1, [local(I) | Old ]) = New. 
     
                                                            
:- impure pred return_args(vars::in, int::out, lua::in) is det.
    
 return_args(List, Count, L) :-
  List = [] ->
    Count = 0
  ; List = [Var | Rest] ->
    impure push_var(Var, L),
    impure return_args(Rest, Old, L),                
    Count = Old + 1
  ; throw(lua_error(runtime_error, $module ++ "." ++ $pred ++
		  " Invalid list of vars.")).

	% Accepts a Mercury function that takes a list of lua variables and 
	% returns a list of lua variables. If a semidet function fails, then
	% the pred will return nil to lua.
	%
%:- func make_lua_func(func(vars, ls, ls) = vars) = mr_func.
%:- mode make_lua_func(in(func(in, di, uo) = out is det)) = mpo.
%:- mode make_lua_func(in(func(in, di, uo) = out is semidet)) = mpo.
%:- mode make_lua_func(in(func(in, di, uo) = out is cc_multi)) = mpo.
%:- mode make_lua_func(in(func(in, di, uo) = out is cc_nondet)) = mpo.


make_lua_func(Func) = (impure func(L) = Returned is det :-  
    semipure Top = lua_gettop(L),
    semipure Args = get_args(Top, []),
    LS = ls(L, current_id, empty_trail),
    Return = Func(Args, LS, _) ->        
      impure return_args(Return, Returned, L)
    ; Returned = 0
  ). 

    
 
%:- func make_nondet_lua_func(pred(vars, ls, ls, var)) = mr_func.
%:- mode make_lua_func(in(pred(in, di, uo, out) is det)) = mpo.
%:- mode make_lua_func(in(pred(in, di, uo, out) is semidet)) = mpo.
%:- mode make_lua_func(in(pred(in, di, uo, out) is multi)) = mpo.
%:- mode make_lua_func(in(pred(in, di, uo, out) is nondet)) = mpo.

make_nondet_lua_func(Func) = (impure func(L) = Returned is det :-
  semipure Top = lua_gettop(L),  
  semipure Args = get_args(Top, []),    
  Pred = (pred(Out::out) is nondet :- 
    Out = Func(Args, ls(L, current_id, empty_trail), _)),
  Return:vars = solutions(Pred),        
  impure return_args(Return, Returned, L)).

	% Accepts a string chunk of lua code and compiles it to a lua function, 
	% passing it by refrence. If the compile fails, a refrecnce to a lua_error    
	% userdata object will be returned instead.
	%  
%:- pred string_to_func(string, var, ls, ls) is det.
%:- mode string_to_func(in, out, di, uo) is det.
%
%:- func string_to_func(string, ls, ls) = var is det.
%:- mode string_to_func(in, di, uo) = out is det.

string_to_func(Chunk, Var, ls(L, Ix, T), ls(L, Ix, T)) :-
  impure lua_loadstring(Chunk, L) = _,
  semipure I = lua_gettop(L),
  semipure Ref = lua_toref(I, L),
  Var = ref(Ref),
  impure lua_pop(1, L).
   

:- pragma promise_pure(string_to_func/4).

string_to_func(Chunk, L1, L2) = Var :- string_to_func(Chunk, Var, L1, L2).


	% Calls a lua variable as if it were a function, this may be unsafe if the
	% variable is not a function or does not have a __call metamethod defined
	%
%:- pred call_lua_func(var, vars, vars, ls, ls) is det.
%:- mode call_lua_func(in, in, out, di, uo).
%
%:- func call_lua_func(var, vars, ls, ls) = vars is det.
%:- mode call_lua_func(in, di, uo) = out is det.

call_lua_func(Var, Arg_List, Ret_List, ls(L, Ix, T), ls(L, Ix, T)) :-
  semipure TopBefore = lua_gettop(L),
  impure push_var(Var, L), % push the function onto the stack 
  impure push_values(Arg_List, Args, L), % push arguments onto the stack
  impure lua_call(Args, multret, L),
  semipure TopAfter = lua_gettop(L),
  Returned = TopAfter - TopBefore,
  ( Returned = 0 -> Ret_List = [] ; semipure to_values(Returned, Ret_List, L)), 
  impure lua_settop(TopBefore, L).

:- pragma promise_pure(call_lua_func/5).
  
call_lua_func(Var, Arg_list, !L) = Ret_List :- 
  call_lua_func(Var, Arg_list, Ret_List, !L).  



%-----------------------------------------------------------------------------%
%
% Length
%

:- pragma foreign_decl("C", "
	size_t apollo_len(lua_State *, int);
").

:- pragma foreign_code("C", "

size_t apollo_len(lua_State * L, int index) {
	
#ifdef BEFORE_502
	return lua_objlen(L, index);
#else 
	return lua_rawlen(L, index);
#endif /* END BEFORE_502 */
}

").




%-----------------------------------------------------------------------------%
%
% The registry, and upvalues.
%


:- pragma foreign_decl("C", "
	void apollo_getregistry(lua_State *, const char *);
	void apollo_setregistry(lua_State *, const char *);
	int  apollo_getupvalue(lua_State *, const int);
	void apollo_setupvalue(lua_State *, const int);
").


:- pragma foreign_code("C", "
	void apollo_getregistry(lua_State * L, const char * k) {
		lua_getfield(L, LUA_REGISTRYINDEX, k);
	}

	void apollo_setregistry(lua_State * L, const char * k) {
		lua_setfield(L, LUA_REGISTRYINDEX, k);
	}

	int apollo_getupvalue(lua_State * L, const int id) {
		lua_pushvalue(L, lua_upvalueindex(id));
		if (lua_type(L, -1) == LUA_TNONE) {
			lua_pop(L, 1);
			return 0;
		} else {
			return 1;
		}
	}

	void apollo_setupvalue(lua_State * L, const int id) {
		lua_replace(L, lua_upvalueindex(id));
	}

").



%-----------------------------------------------------------------------------%
%
% Lua modules
%

/*
register_module(Name, Func, L, !IO) :-

	( semipure ready(L) ; impure init_lua(L, !IO) ),
	 
	( 	impure lua_pushfunc(L, Func),
		impure lua
	; 
		unexpected($module, $pred, 
		"function/2 did not return a ref.")
	),
	impure lua_getregistry(L, LUA_RIDX_MR_MODULE), /* table -3 /
	impure lua_pushstring(L, Name), /* key -2 /
	impure apollo_pushref(L, R), /* value -1 /
	impure lua_settable(L, -3), /* table -1 /
	impure lua_pop(L, 1). /* empty stack /

:- pragma promise_pure(register_module/5).
*/

:- pragma foreign_decl("C", "
	int apollo_loader(lua_State *);
").

:- pragma foreign_code("C", "

	/* take the provided module name and attempt to load an apollo module
	passes any additional arguments. */
	int apollo_loader(lua_State * L) {
		if (lua_isstring(L, 1)) {
			const char * module_name = lua_tostring(L, 1);
			apollo_getregistry(L, LUA_MR_MODULES);
			lua_getfield(L, 2, module_name);
			return 1;
		}
		return 0;
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
% Mercury userdata
%

:- pragma foreign_decl("C", "
	MR_Word * apollo_new(MR_Word);
	int apollo_free(lua_State *);
").

:- pragma foreign_code("C", "

	MR_Word * apollo_new(MR_Word word) {
		MR_Word * newptr = MR_GC_malloc_uncollectable(sizeof newptr);
		*newptr = word;
		return newptr; 
	}
	
	int apollo_free(lua_State * L) {
		MR_Word ** ptr = lua_touserdata(L, 1);
		MR_GC_free(*ptr);
		return 0;
	}
		
").
		
:- impure func to_string(lua) = int.

to_string(L) = 1 :- 
	semipure lua_touserdata(1, L) = U,
	impure lua_pushstring(string.string(univ_value(U)), L).

		
	
:- pragma foreign_export("C", to_string(in) = out, "apollo_tostring").



%-----------------------------------------------------------------------------%
%
% Lua errors
%

:- pragma foreign_enum("C", error_type/0,
[
	no_error	-	"0",
	runtime_error 	-	"LUA_ERRRUN",
	syntax_error 	-	"LUA_ERRSYNTAX",
	memory_error	-	"LUA_ERRMEM",
	unhandled_error	-	"LUA_ERRERR"
]). 


