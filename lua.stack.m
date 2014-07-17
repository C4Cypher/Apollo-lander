%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.state.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file provides lower level access to the Lua state itself, allowing for
% direct manipulation of the Lua stack.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.stack.

:- interface.




%-----------------------------------------------------------------------------%
%
% The Lua stack
%
	
% Lua facilitates interaction with foreign code by providing a stack based
% interface to interact with.  When Lua makes a call to a foreign function, a 
% brand new stack is created.  Arguments are pushed onto the stack, and then 
% Lua calls the foreign function, passing the stack to it and receiving an
% integer return value for the number of values to pop off the stack for
% the return value of the Lua call, then the stack is discarded.
% 
% This stack based interface for calling foreign code from Lua is simple,
% flexible and fast, but Lua trusts that anyone writing Lua functions in C
% knows what they are doing.  By default, Lua allocates space for 20 variables
% on the stack, but it trusts that the C function will ask for more space if
% it needs it.  BAD THINGS happen when you try to push Lua variables onto
% unallocated stack space, trust me on this.
%
% ANY IMPURE CALLS IN THIS MODULE ARE UNSAFE!  Please! Use get_top/1 and 
% check_stack/2 to ensure that you have enough space for your variables if you
% insist on using the impure variants of these calls. 
%
% I have deliberately omitted any impure calls that remove values from the
% stack in order to treat the stack as an immutable data structure,
% to preserve the declarative semantics of calls from Mercury to Lua. Calls
% to  do_impure/2 and do_stack_impure/2 will pop off any values from the stack
% that are added by the impure higher-order calls they are provided.
%
% Eventually, if enough values are pushed onto the stack without removing any
% Lua will run out of RAM it is allowed to allocate, and you'll end up with
% a memory allocation error.  The semipure calls are designed to prevent this
% by ensuring any values they add to the stack are removed before they finish.
%
% I cannot prevent you from popping variables off the stack from within
% foreign_procs, and I cannot guarantee the stability of your code if you do
% so.  REMOVE VALUES FROM THE STACK AT YOUR OWN RISK.
% 
% The semipure calls in this module either do not modify the stack at all, or
% remove any values placed on the stack once they are finished.  Their use is
% strongly recommended over impure calls.

	% The stack type is an abstraction of the lua.state type, carrying
	% additional information to allow safe interactions with the 
	% Lua stack.
	%
:- type stack.
	
	% Used for readability, indexes are used to refrence values on the
	% Stack or at pseduo-indexes
:- type index == int.

	% Convert a state into a stack for stack operations.
:- func stack(state) = stack is det.

	% Perform an operation with Lua state, throwing an exception if the
	% Lua state is not in the condition it was found in.
	%
:- semipure pred do(state, semipure pred(stack)).
:- mode do(in, (pred(in) is det)) is det.
:- mode do(in, (pred(in) is semidet)) is semidet.

	% Perform an operation with a Lua stack that will leave it in the same
	% state that it was found.
	%
:- semipure pred do_impure(stack, impure pred(stack)).
:- mode do(in, (pred(in) is det)) is det.
:- mode do(in, (pred(in) is semidet)) is semidet.

	% Perform an operation with a Lua stack, throwing an exception if the
	% Lua state is not in the condition it was found in.
	%
:- semipure pred do_stack(stack, semipure pred(stack)).
:- mode do_stack(in, (pred(in) is det)) is det.
:- mode do_stack(in, (pred(in) is semidet)) is semidet.

	% Perform an operation with a Lua stack that will leave it in the same
	% state that it was found.
	%
:- semipure pred do_stack_impure(stack, semipure pred(stack)).
:- mode do_stack(in, (pred(in) is det)) is det.
:- mode do_stack(in, (pred(in) is semidet)) is semidet.


	% Find the top of the stack
	%
:- semipure func get_top(state) = index is det.

	% Set the top of the stack.
	%
:- impure pred set_top(state, int) is det.


	% checkstack(L, Number).
	% 
	% Ensure there are a certain Number of free values on the stack.
	% Fails if Lua cannot allocate memory for the free values.
:- semipure pred check_stack(state::in, int::in) is semidet.


%-----------------------------------------------------------------------------%
%
% Stack queries
%

	% There is a nil value at the provided index.
	%
:- semipure pred is_nil(stack::in, index::in) is semidet.

	% is_int will fail if a number has a non-zero fraction part. 
	%
:- semipure pred is_int(stack::in, index::in) is semidet.

	% is_number will succeed if the value at the top of the stack is a
	% Lua number. 
	%
:- semipure pred is_number(stack::in, index::in) is semidet.

	% is_bool will succeed if the value at the top of the stack is a
	% Lua boolean. 
	%
:- semipure pred is_bool(stack::in, index::in) is semidet.

	% is_string will succeed if the value at the top of the stack is a
	% Lua string. 
	%
:- semipure pred is_string(stack::in, index::in) is semidet.


	% is_lightuserdata will succeed if the value at the top of the stack
	% is a C void * pointer. 
	%
:- semipure pred is_lightuserdata(stack::in, index::in) is semidet.


	% is_table will succeed if the value at the top of the stack is a
	% Lua table. 
	%
:- semipure pred is_table(stack::in, index::in) is semidet.

	% is_function will succeed if the value at the top of the stack is a
	% Lua function. 
	%
:- semipure pred is_function(stack::in, index::in) is semidet.

	% is_userdata will succeed if the value at the top of the stack is a
	% Lua userdata. 
	%
:- semipure pred is_userdata(stack::in, index::in) is semidet.


	% is_thread will succeed if the value at the top of the stack is a
	% Lua thread. 
	%
:- semipure pred is_thread(stack::in, index::in) is semidet.




%-----------------------------------------------------------------------------%
%
% Passing values from Lua
%

% :- semipure func to_Type(stack, index) = Type is semidet 
%
% This will return the value stored at the given index, failing if the
% value is not the appropriate type.

	% to_int will fail when attempting to pass a number that has a 
	% non-zero fraction part. 
	%
:- semipure func to_int(stack, index) = int is semidet.


:- semipure func to_number(stack, index) = float is semidet.


:- semipure func to_bool(stack, index) = bool is semidet.


:- semipure func to_string(stack, index) = string semidet.


:- semipure func to_lightuserdata(stack, index) = c_pointer is semidet.


:- semipure func to_table(stack, index, table) = table is semidet.


:- semipure func to_function(stack, index) = function is semidet.


:- semipure func to_userdata(stack, index) = userdata is semidet.


:- semipure func to_thread(stack, index) = thread is semidet.


:- semipure func to_state(stack, index) = state is semidet.


:- semipure func to_var(stack, index) = var is semidet.


:- semipure func to_value(stack, index) = value is semidet.

%-----------------------------------------------------------------------------%
%
% Stack push operations
%

% Push operations push values onto the stack.  They come in two forms, pure
% and semipure.
%
% :- impure func push_Type(stack, Type) = stack is det.
%
% This simply pushes a value onto the top of the stack, returning a modified
% stack containing the required information to reverse the change. 
%
% :- semipure pred push_Type(stack::in, Type::in, 
%	semipure pred(stack::in) is Det) is Det.
%
% This form pushes a value onto the stack, performs a semipure operation on
% the stack, and then pushes the value off the stack.


	% Push a copy of the value at index onto the top of the stack. 
	% Aborts if the Index is not valid.
	%
:- semipure pred push_stack(stack, index, pred(state)).
:- mode push_stack(in, in, (pred(in) is det)) is det.
:- mode push_stack(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_stack(stack, index) = state is det.


	% upvalue(L, Id, Pred).
	% upvalue(L1, Id) = L2. 
	%
	% Push a copy of an upvalue associated with the given ID onto the
	% stack. Fails if Id is not a valid upvalue for the current context.
	%
:- semipure pred push_upvalue(stack, int, pred(state)).
:- mode push_upvalue(in, in, (pred(in) is det)) is semidet.
:- mode push_upvalue(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_upvalue(stack, int) = state is det.

	% global(L, Key, Pred).
	% global(L1, Key) = L2.
	%
	% Push a copy of the global variable indexed by Key onto the stack.
	%
:- semipure pred push_global(stack, string, pred(state)).
:- mode push_global(in, in, (pred(in) is det)) is det.
:- mode push_global(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_global(stack, string) = state is det.

	% ref(L, Id, Pred).
	%
	% Push a copy of a refrence onto stack.
	% Aborts if the refrence is not valid.
	%
:- semipure pred push_ref(stack, int, pred(state)).
:- mode push_ref(in, in, (pred(in) is det)) is det.
:- mode push_ref(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_ref(stack, int) = state is det.

	% regis(L, Index, Pred).
	%
	% Push a copy of a thing onto the top of the stack and do something
	% with it, then pop it off the stack.
:- semipure pred push_registry(stack, int, pred(state)).
:- mode push_registry(in, in, (pred(in) is det)) is det.
:- mode push_registry(in, in, (pred(in) is semidet)) is semidet.

	% Push a nil value onto the stack and do something with it.
	%
:- semipure pred push_nil(stack, pred(state)).
:- mode push_nil(in, (pred(in) is det)) is det.
:- mode push_nil(in, (pred(in) is semidet)) is semidet.

:- impure func push_nil(stack) = state is det.

	% Mercury int values will be converted to floats before being stored
	% as Lua numbers.
	%
:- semipure pred push_int(stack, int, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_int(stack, int) = state is det.


:- semipure pred push_number(stack, float, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_number(stack, float) = state is det.


:- semipure pred push_bool(stack, bool, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_bool(stack, bool) = state is det.


:- semipure pred push_string(stack, string, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_string(stack, string) = state is det.


:- semipure pred push_lightuserdata(stack, c_pointer, pred(state) is det) 
	is det.
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_lightuserdata(stack, c_pointer) = state is det.


:- semipure pred push_table(stack, table, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_table(stack, table) = state is det.


:- semipure pred push_function(stack, function, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_function(stack, function) = state is det.


:- semipure pred push_userdata(stack, userdata, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_userdata(stack, userdata) = state is det.


:- semipure pred push_thread(stack, state, pred(state)).
:- mode push_(in, in, (pred(in) is det)) is det.
:- mode push_(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_thread(stack, state) = state is det.


	% var(L, Var, Pred).
	% var(L1, Var) = L2
	%
	% Push a copy of Var the stack.
	%
:- semipure pred push_var(stack, var, pred(state)).
:- mode push_var(in, in, (pred(in) is det)) is det.
:- mode push_var(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_var(stack, var) = state is det.

	% value(L, Value, Pred).
	% value(L1, Value) = L2
	%
	% Push a copy of the given Value onto stack. 
	% 
:- semipure pred push_value(stack, value, pred(state)).
:- mode push_value(in, in, (pred(in) is det)) is det.
:- mode push_value(in, in, (pred(in) is semidet)) is semidet.

:- impure func push_value(stack, value) = state is det.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require.

:- type stack ---> stack(state::state. bottom::int, top::int).
	

% :- pred stack(state::in, stack::out) is det.

stack(L, ) :- 


% :- func stack(state) = stack is det.

stack(L) = stack(L, Top, Top) :- Top = get_top(L).

:- pred validate_stack(stack::in) is det.

validate_stack(L) :- (
		Top = get_top(L ^ state),
		L ^ bottom =< Top
	;
		error("lua.stack: Invalid stack operation: Lua stack started "
		++ " with " ++ L ^ bottom ++ " values and ended with " ++
		Top ++ " values.  Lua may not remove more values from the " ++
		"stack than it added.")
	).

% :- semipure pred do(state, semipure pred(stack)).

do(L, Pred) :- 
	Stack = stack(L),
	( semipure Pred(Stack) -> (
			validate_stack(L)
		;
			validate_stack(L), fail
	).
		  

% :- semipure pred do_impure(stack, impure pred(stack)).

do_impure(L, Pred) :- 
	Stack = stack(L),
	( impure Pred(Stack) -> (
			validate_stack(L),
			impure set_top(L, L ^ bottom)
		;
			validate_stack(L),
			impure set_top(L, L ^ bottom),
			fail
	).

% :- semipure pred do_stack(stack, semipure pred(stack)).

do(L, Pred) :- 
	( semipure Pred(L) -> (
			validate_stack(L)
		;
			validate_stack(L), fail
	).

% :- semipure pred do_stack_impure(stack, semipure pred(stack)).

do_impure(L, Pred) :- 
	Stack = stack(L),
	( impure Pred(Stack) -> (
			validate_stack(L),
			impure set_top(L, L ^ bottom)
		;
			validate_stack(L),
			impure set_top(L, L ^ bottom),
			fail
	).

% :- semipure func top(state) = index.

% :- impure pred set_top(state, int) is det.

% :- semipure pred checkstack(state::in, int::in) is semidet.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module map.
:- import_module type_desc.

:- pragma foreign_decl("C", 
"
#include <h>
#include <lauxlib.h>
#include <lualib.h>

#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""

#define AP_TYPE ""__mercury_type""

#define AP_UNIV ""luaAPOLLO_LANDER_MERCURY_UNIV""
#define AP_FUNCTION ""luaAPOLLO_LANDER_MERCURY_FUNCTION""
#define AP_USERDATA ""luaAPOLLO_LANDER_MERCURY_USERDATA""


#define AP_FUNCTION_UPVALUE 1

").

:- pragma foreign_code("C", 
"

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaAP_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_MODULE);
		lua_getfield(L, 2, module_name);
		return 1;
	}
	return 0;
}

").

:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

:- pragma foreign_type("C", c_function, "lua_CFunction",
	[can_pass_as_mercury_type, stable]).

:- pragma foreign_decl("C", "extern void luaAP_init(lua_State *);").

:- pragma foreign_code("C", "void luaAP_init(lua_State * L) {

	/* Add tables to the registry. */

	/* univ_var(univ) metatable */
	lua_newtable(L);
	
	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_UNIV);

	/* TODO: load metamethods for univ vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_UNIV);

	/* function(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_FUNCTION);
	
	/* TODO: load metamethods for function vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_FUNCTION);

	/* univ_var(univ) metatable */
	lua_newtable(L);

	lua_pushboolean(L, 1);
	lua_setfield(L, -1, AP_IS_USERDATA);
	
	/* TODO: load metamethods for userdata vars */
	
	lua_setfield(L, LUA_REGISTRYINDEX, AP_USERDATA);
	
	
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_MODULE);

	
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const lua_Integer length = (lua_Integer)lua_objlen(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaAP_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_READY);
}").

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaAP_init(L);").

:- pragma foreign_decl("C", "int luaAP_ready(lua_State *);").

:- pragma foreign_code("C", 
"
	/* check to see if Lua has already been initialized. */
	int luaAP_ready(lua_State * L) {
		lua_checkstack(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}
").

:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaAP_lua_ready(L);
").






%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

% The following procedures all carry the same type and mode signatures, and
% all have the same purpose, but for different types.
%
% to_Type(Var, Type) <=> to_Type(Var) = Type.
% These will accept a Lua variable and return the associated Mercury variable 
% of the corresponding Type, failing if the type of the Lua variable is not
% convertable to Type.
%
% push_Type(Type, Var) <=> push_Type(Type) = Var.
% These will accept the corresponding Mercury Type and return a Lua variable
% storing that Type.  If Lua cannot allocate the memory for a new variable,
% it will abort.
%
% Note that when passing numeric values, Lua will implicitly cast strings
% to numbers and back if the string represents a numeric value. 
% For example, in Lua, "3" == 3 will evaluate to true.
%-----------------------------------------------------------------------------%


% Mercury has no equivalent to the Lua nil value.  In Lua, nil does represent
% the empty list, but the abscence of value, similar to the SQL NULL value.

	
	% Check to see if Var is a nil value. 
	%
:- pred is_nil(var::in) is semidet.


	% Create a new Lua variable with a value of nil.
	%
:- pred push_nil(state::in, var::out) is det.
:- func push_nil(state) = var is det.

push_nil(S) = V :- push_nil(S, V).

:- pragma foreign_proc("C", is_nil(Var), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	SUCCESS_INDICATOR = lua_isnil(L, -1);
	lua_pop(L, 1);
").



:- pragma foreign_proc("C", push_nil(L::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnil(L);
	Ref = luaAP_new_ref(L);
").


%-----------------------------------------------------------------------------%




	% Mercury int values will be converted to floats before being stored
	% as Lua numbers.
	%
	% push_int will fail when attempting to pass a number that has a 
	% non-zero fraction part. 
	%
:- pred to_int(var::in, int::out) is semidet.
:- func to_int(var) = int is semidet.

:- pred push_int(state::in, int::in, var::out) is det.
:- func push_int(state, int) = var is det.

:- pragma foreign_proc("C", to_int(L::out, Int::out, Ref::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	if(lua_isnumber(L, -1) {
		double number = (double)lua_tonumber(L, -1);
		I = (MR_Integer)number;
		SUCCESS_INDICATOR = !(Int - number);
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").

:- pragma foreign_proc("C", int_to_ref(L::in, Int::in, Ref::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushinteger(L, (lua_Integer)Int);
	Ref = luaAP_new_ref(L);
").



	% Both Mercury floats and Lua numbers are represented in C by a
	% double precision floating point value, as a result, to_float
	% will never fail if the Lua variable is garunteed to be a number. 
	%
:- pred to_float(var::in, float::out) is semidet.
:- func to_float(var) = float is semidet.

:- pred push_float(state::in, float::in, var::out) is det.
:- func push_float(state, float) = var is det.


% Lua boolean values differently than C does. In Lua, any value that is not
% false or nil will evaluate to 'true' when Lua expects a boolean value. 

:- pred to_bool(var::in, bool::out) is semidet.
:- func to_bool(var) = bool is semidet.

	% explicit_bool will test the type of the Lua variable, failing if
	% it is any Lua type other than boolean.
	%
:- pred explicit_bool(var::in, bool::out) is semidet.
:- func explicit_bool(var) = bool is semidet.

:- pred push_bool(state::in, bool::in, var::out) is det.
:- func push_bool(state, bool) = var is det.


% Lua strings behave similarly to Mercury strings and Char * pointers.
% The only notable difference is the fact that Lua internalizes any new
% string that isn't already instantiated in   If two strings are equal in
% Lua, then behind the scenes, they are refrencing the same address.  This 
% makes table lookups and equality tests very efficient, at the cost of making
% string concatenation VERY expensive.
%
% Because of this, a string value is not garbage collected by Lua until all
% string variables with that value are no longer in use.  This allows some
% interesting tricks for managing the lifetime of variables (especially for
% memoizing) through the use of string keys in weak tables.
 
:- pred to_string(var::in, string::out) is semidet.
:- func to_string(var) = string is semidet.

:- pred push_string(state::in, string::in, var::out) is det.
:- func push_string(state, string) = var is det.


% C pointer types can be passed to Lua using a special variation of the
% Lua userdata type called lightuserdata.  Unlike full userdata, pointers
% passed as lightuserdata cannot be assigned metatables, and are not 
% managed by the Lua garbage collector.  As a result, be careful when passing
% pointers as lightuserdata that might be garbage collected by Mercury.

:- pred to_pointer(var::in, c_pointer::out) is semidet.
:- func to_pointer(var) = c_pointer is semidet.

:- pred push_pointer(state::in, c_pointer::in, var::out) is det.
:- func push_pointer(state, c_pointer) = var is det.












:- pred float_to_ref(lua_state, float, ref).
:- mode float_to_ref(in, in, out).
:- mode float_to_ref(out, out, in).


:- pragma foreign_proc("C", float_to_ref(L::in, Float::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushnumber(L, (lua_Number)Float);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", float_to_ref(L::out, Float::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isnumber(L, -1) {
		Float = (MR_Float)lua_tonumber(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);
		
").

:- pred bool_to_ref(lua_state, bool, ref).
:- mode bool_to_ref(in, in, out).
:- mode bool_to_ref(out, out, in).


:- pragma foreign_proc("C", bool_to_ref(L::in, Bool::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	if (Bool == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", bool_to_ref(L::out, Bool::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isboolean(L, -1) {
		int boolean = lua_toboolean(L, -1);
		if(boolean) 
			Bool = MR_YES;
		else
			Bool = MR_NO;
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

:- pred string_to_ref(lua_state, string, ref).
:- mode string_to_ref(in, in, out).
:- mode string_to_ref(out, out, in).

:- pragma foreign_proc("C", string_to_ref(L::in, String::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushstring(L, String);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", string_to_ref(L::out, String::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_isstring(L, -1) {
		String = (MR_String)lua_tostring(L, -1);
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
	
	lua_pop(L, 1);
").

:- pred c_pointer_to_ref(lua_state, c_pointer, ref).
:- mode c_pointer_to_ref(in, in, out).
:- mode c_pointer_to_ref(out, out, in).


:- pragma foreign_proc("C", c_pointer_to_ref(L::in, Pointer::in, Var::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_pushlightuserdata(L, Pointer);
	Var = luaAP_new_ref(L);
").

:- pragma foreign_proc("C", c_pointer_to_ref(L::out, Pointer::out, Var::in), 
	[promise_pure, will_not_call_mercury], 
"
	L = luaAP_ref_state(Var);
	luaAP_push_ref(L, Var);
	if(lua_islightuserdata(L, -1) {
		Pointer = lua_tolightuserdata(L, -1);
		
		SUCCESS_INDICATOR = 1;
	} else 
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);	
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


:- pragma foreign_type("C", ref, "luaAP_Ref *", [can_pass_as_mercury_type])
	where equality is ref_equals.

:- pragma foreign_proc("C", ref_equals(A::in, B::in),
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(A);
	lua_checkstack(L, 2);
	luaAP_push_ref(L, A);	
	luaAP_push_ref(L, B);
	SUCCESS_INDICATOR = lua_rawequal(L, -2, -1);
	lua_pop(2);
").



:- pragma foreign_proc("C", ref_state(Ref::in, L::out) ,
	[promise_pure, will_not_call_mercury],
	"L = luaAP_ref_state(Ref);").

ref_state(R) = L :- ref_state(R, L).
	
:- pragma foreign_code("C",
"

typedef struct luaAP_Ref {
	lua_State * state;
	int id;
}


/* Creates a new refrence from the stack */
luaAP_Ref * luaAP_new_ref(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Ref * new_ref = MR_GC_NEW(luaAP_Ref);
	new_ref->state = L;
	new_ref->id = id;
	MR_GC_register_finalizer(new_ref, luaAP_finalize_ref);
	return new_var;
}

/* Retreives the refrenced Lua state */
lua_State * luaAP_ref_state(luaAP_Ref * ref) {
	return ref->state;
}

/* Push a refrence onto the provided stack */
void luaAP_push_ref(lua_State * L, luaAP_Ref * ref) {
	lua_State * L0 = ref-> state;	
	int id = ref->id;
	if (id == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else if (L == L0) {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
	else {
		lua_checkstack(L0, 1);
		lua_rawgeti(L0, LUA_REGISTRYINDEX, id);
		lua_xmove(L0, L, 1);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaAP_finalize_ref(luaAP_Ref * ref, void * dummy) {
	luaL_unref(ref->state, LUA_REGISTRYINDEX, ref->id);
}

"). 


:- pragma foreign_proc("C", ref_type(Ref::in, Type::out), 
	[promise_pure, will_not_call_mercury], 
"
	lua_State * L = luaAP_ref_state(Ref);
	luaAP_push_ref(Ref);
	Type = lua_type(L, -1);
	lua_pop(L, 1);
").

ref_type(R) = T :- ref_type(R, T).

%-----------------------------------------------------------------------------%


%-----------------------------------------------------------------------------%

:- pred univ_to_ref(lua_state, var, ref).
:- mode univ_to_ref(in, in, out) is semidet.
:- mode univ_to_ref(out, out, in) is semidet.

univ_to_ref(L, Var1, Ref) :-
	Var1 = lua_univ(U1),
	Var2 = lua_univ(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	univ_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred univ_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", univ_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_UNIV);").	

:- pragma foreign_proc("C", univ_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%

:- pred table_to_ref(lua_state, var, ref).
:- mode table_to_ref(in, in, out) is semidet.
:- mode table_to_ref(out, out, in) is semidet.

table_to_ref(L, Var1, Ref) :-
	Var1 = table(U1),
	Var2 = table(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	userdata_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred userdata_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_USERDATA);").	

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%

:- pred userdata_to_ref(lua_state, var, ref).
:- mode userdata_to_ref(in, in, out) is semidet.
:- mode userdata_to_ref(out, out, in) is semidet.

userdata_to_ref(L, Var1, Ref) :-
	Var1 = userdata(U1),
	Var2 = userdata(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	userdata_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred userdata_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_USERDATA);").	

:- pragma foreign_proc("C", userdata_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").

%-----------------------------------------------------------------------------%


:- pred function_to_ref(lua_state, var, ref).
:- mode function_to_ref(in, in, out) is semidet.
:- mode function_to_ref(out, out, in) is semidet.

univ_to_ref(L::in, Var1::in, Ref::out) :-
	Var1 = function(U1),
	Var2 = function(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	function_to_ref_unsafe(L, Var2, Ref0),
	make_closure(L, Ref0, Ref).

univ_to_ref(L::out, Var1::out, Ref::in) :-
	Var1 = function(U1),
	Var2 = function(U2),
	univ(U1) = univ(U2),
	univ(Var1) = univ(Var2),
	function_to_ref_unsafe(L, Var2, Ref).
	
		
:- pred function_to_ref_unsafe(lua_state::in, var::in, ref::out) is det.

:- pragma foreign_proc("C", function_to_ref_unsafe(L::in, Var::in, Ref::out), 
	[promise_pure, may_call_mercury], 
	"Ref = *luaAP_get_ref(L, Var, AP_UNIV);").	

:- pragma foreign_proc("C", function_to_ref_unsafe(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L, AP_UNIV);
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").




	

:- pred make_closure(lua_state::in, ref::in, ref::out).
:- pragma foreign_proc("C", make_closure(L::in, Function::in, Ref::out), 
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_ref(L, Function);
	lua_pushclosure(L, luaAP_call, 1);
	Ref = luaAP_new_ref(L);
").

:- func lua_call_function(lua_state::in, io::di, io::uo) = (int::out) is det.

lua_call_function(L, !IO) = Num :-
	NewFunction = function(Function),	
	(
		 get_function_upvalue(L, univ(NewFunction), !IO) 
	; 
		lua_error(L, "lua_call_function: Failed to load function") 
	),
	Args = args(L),
	call_function(Function, Args, Return, !IO),
	ReturnValue = return(L, Return),
	require_complete_switch [ReturnValue]
	( ReturnValue = nil ->
		Num = 0
	; ReturnValue = return_var(Var) ->
		push_var(L, Var, !IO),
		Num = 1
	; 
		ReturnValue = return_list(VarList),
		push_list(L, VarList, Num, !IO)
	).

:- pragma foreign_export("C", 
	lua_call_function(in, di, uo) = out, "luaAP_call").
		

:- pred get_function_upvalue(lua_state::in, univ::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", 
	get_function_upvalue(L::in, Univ::out, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	lua_pushvalue(L, lua_upvalueindex(AP_FUNCTION_UPVALUE));
	
	MR_Word * new_univ = luaAP_get_var(L);
	lua_pop(L, 1);
	
	assert(new_univ);
	
	Univ = new_univ*;
").
	

:- pred push_var(lua_state::in, var::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", push_var(L::in, Var::in, _I::di, _O::uo),
	[promise_pure, will_not_call_mercury], 
"
	luaAP_push_ref(L, Var);").
	
:- pred push_list(lua_state::in, list(var)::in, int::out, io::di, io::uo)
	is det.

push_list(_, [], 0, !IO).

push_list(L, [Var | Rest], Num + 1, !IO) :- 
	push_var(L, Var, !IO), 
	push_list(L, Rest, Num, !IO).

:- instance args(lua_state) where [ args(L) = L ].

:- instance return(return) where [ return(_, Return) = Return ].

%-----------------------------------------------------------------------------%


%:- pred var_to_ref(lua_state, var, ref).
%:- mode var_to_ref(in, in, out).
%:- mode var_to_ref(out, out, in).




:- pragma foreign_code("C", 
" 
luaAP_Ref luaAP_get_ref(lua_State * L, MR_Word var, const char * var_type) {

	/* Force Mercury to hold a refrence to the var so
	that it won't be garbage collected. */
	luaAP_intern(Var);
	
	/* Create a new Lua userdata and point it to the var */
	MR_Word * udata = lua_newuserdata(L, sizeof(MR_Word));
	*udata = var;
	
	/* Assign our new userdata a metatable */
	lua_getfield(L, LUA_REGISTRYINDEX, var_type);
	lua_setmetatable(L, -2);
	
	return luaAP_new_ref(L);
}


MR_Word * luaAP_get_var(lua_State * L, const char * var_type) {


	/* Userdata will be a valid univ value only if it's
	metatable contains a value at AP_MERCURY_UNIV. */
	
	if (luaL_getmetafield(L, -1, AP_TYPE)) {

		if(lua_tostring(L, -1) = var_type) {
		
			/* Remove the metafeild pushed by luaL_getmetafield */
			lua_pop(L, 1);
		
			/* Extract the MR_Word pointer */
			MR_Word * var = (MR_Word *) lua_touserdata(L, -1);
		
			return var;
		}
		else
			lua_pop(L, 1);
	}

	return 0;
} ").
		
%-----------------------------------------------------------------------------%

	% This mutvar keeps a refrence to any Mercury variable passed to Lua
	% to ensure that Mercury does not garbage collect said variable before
	% Lua is finished with it.
	%
:- mutable(reserved, map(var, int), map.init, ground, [untrailed, attach_to_io_state]).

	% TODO: Redesign reserved system so that refrences to univs are passed
	% by integer index instead of by pointer?  Only if current implementation
	% proves to be unstable/unworkable.

:- pred intern(var::in, io::di, io::uo) is det.

intern(V, !IO) :- 
	get_reserved(R, !IO),
	( search(R, V, I) ->
		set_reserved(det_update(R, V, I + 1), !IO) 
	;
		set_reserved(det_insert(R, V, 1), !IO)
	).

:- pragma foreign_export("C", intern(in, di, uo), "luaAP_intern").

:- pred release(lua_state::in, var::in, io::di, io::uo) is det.


release(L, V, !IO) :- get_reserved(R, !IO),
	( search(R, V, I) -> 
		( I > 1 -> 
			set_reserved(det_update(R, V, I - 1), !IO)
		;
			set_reserved(delete(R, V), !IO)
		)
	;
		lua_error(L, 
			string.append_list(
			["Attempted to release a Mercury type: ",
			type_name(type_of(V)),
			" that was not interned."])
		)
	).



:- pragma foreign_export("C", release(in, in, di, uo), "luaAP_release").

%-----------------------------------------------------------------------------%


