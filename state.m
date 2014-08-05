%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: state.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file provides typeclass for the Lua state.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module state.

:- interface.




	% Create a fresh, new , initialized lua_state.
	%
:- pred new_state(lua::out).

:- func state_ptr(lua_state) = lua_state_ptr.

	% Return the Lua state's current status.
	%
:- semipure pred get_status(lua::in, status::out). 


:- type status
	---> 	ready
	;	yield
	;	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.

%-----------------------------------------------------------------------------%
%
% Stack operations.
%

:- type index == int.

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

% Warning: Lua employs minimal error checking when performing low level
% stack operations. It trusts that code directly manipulating the stack
% will avoid using invalid stack refrences or stack overflows through the use
% of top, get_top, set_top and check_stack.  For more information, refer
% to the Lua Refrence Manual, and the examples provided at the Lua User's Wiki.

	% Retreive the index for the top value on the stack.
	% Also represents the number of values on the stack.
	%
:- semipure pred get_top(lua::in, index::out) is det.

	% Set the size of the stack. Any values indexed above the new stack
	% size will be removed from the stack, and any unassigned values at
	% or below the new stack size will be assigned nil values. Aborts
	% if the new size is less than zero. 
	%
:- impure pred set_top(lua::in, index::in) is det.


	% Ensure that there is space allocated to allow pushing the specified
	% number of variables onto the stack.
	%
:- impure pred check_stack(lua::in, int::in) is det.
	
	
	% Look up the type of a value indexed on the stack.
	%
:- semipure pred get_type(lua::in, index::in, lua_type::out) is det.
 


	% Pop the specified number of values off of the stack.
	%
:- impure pred pop(lua::in, int::in) is det.


	% Call a function, specifying the number of arguments pushed onto the 
	% stack above it. The function and argumetns are removed from the stack.
	% If an error is encountered, the provided pred is called with the
	% string error message. The function's return values will be pushed onto
	% the stack if no error is encountered.
	%
:- impure pred pcall(lua, int, pred(string).
:- mode pcall(in, in, pred(in) is det) is det.
:- mode pcall(in, in, pred(in) is erroneous) is det. 

%-----------------------------------------------------------------------------%
%
% Value Passing.
%

	% Push any value onto the Lua stack.
	%
:- impure pred push(lua::in, T::in) is det.

	% Retreive the value at the given index, fail if the value cannot be
	% cast to the desired type. Nondet modes can be used to look up the 
	% indexes where a specific value may be found, or retreive all of
	% the values on the stack.
	%
:- semipure pred pull(lua, index, T).
:- mode pull(in, in, out) is semidet.
:- mode pull(in, out, in) is nondet.
:- mode pull(in, out, out) is nondet.

	% The value typeclass facilitates methods for pushing variables onto 
	% and off of the lua stack.
	%
:- typeclass value(T) where [

	% Look up a value indexed on the stack. Fail if 
	%
	semipure pred pull_value(lua, index, T),
	mode pull_value(in, in, out) is semidet,

	% Push a value onto the stack. Shouldn't need to check for free space
	% on the stack.
	%
	impure pred push_value(lua, T),
	mode push_value(in, in) is det
	
].

% Primitives

:- instance value(nil).
:- instance value(int).
:- instance value(float).
:- instance value(bool).
:- instance value(string).
:- instance value(char).

% C Types.

:- instance value(c_pointer).
:- instance value(c_function).
:- instance value(lua_state).
:- instance value(ref)

% Lua Refrence types.

:- instance value(table).
:- instance value(function).
:- instance value(thread).
:- instance value(userdata).

% Generic Mercury type

:- instance value(


%-----------------------------------------------------------------------------%
%
% Global variables.
%

	% Push a global variable onto the stack.
	%
:- semipure pred push_global(lua::in, string::in) is det.

	% Retreive a global variable.
	%
:- semipure some [T] pred get_global(lua::in, string::in, T::out) is det.

	% Set a global variable from the top of the stack.
	%
:- impure pred pop_global(lua::in, string::in) is det.

	% Set a global variable.
	%
:- impure pred set_global(lua::in, string::in, T::in) is det.

%-----------------------------------------------------------------------------%
%
% Registry Values.
%

	% Push a registry value onto the stack.
	%
:- semipure pred push_registry(lua::in, string::in, T::out) is det.

	% Retreive a registry value.
	%
:- semipure some [T] pred get_registry(lua::in, string::in, T::out) is det.


	% Set a registry valua from the top of the stack.
	%
:- impure pred pop_registry(lua::in, string::in) is det.

	% Set a registry value.
	% 
:- impure pred set_registry(lua::in, string::in) is det.

%-----------------------------------------------------------------------------%
%
% Upvalues.
%

	% Push a function upvalue onto the stack. 
	%
:- semipure pred push_upvalue(lua::in, int::in) is det.

	% Retreive a function upvalue.
	%
:- semipure some [T] pred get_upvalue(lua::in, int::in, T::out) is det.


	% Set a function upvalue from the top of the stack. 
	%
:- impure pred pop_upvalue(lua::in, int::in) is det.

	% Set a function upvalue. 
	%
:- impure pred set_upvalue(lua::in, int::in, T::in) is det.


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Impure API calls
%

% WARINING: Handle with care, contents volitile.

:- semipure pred is_number(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_nil(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_userdata(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_integer(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_lightuserdata(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_string(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_boolean(lua_state_ptr::in, int::in) is semidet.
:- semipure pred is_thread(lua_state_ptr::in, int::in) is semidet.


:- semipure pred pull_number(lua_state_ptr::in, float::out) is det.
:- semipure pred pull_userdata(lua_state_ptr::in, T::out) is det.
:- semipure pred pull_integer(lua_state_ptr::in, int::out) is det.
:- semipure pred pull_lightuserdata(lua_state_ptr::in, c_pointer::out) is det.
:- semipure pred pull_string(lua_state_ptr::in, string::out) is det.
:- semipure pred pull_boolean(lua_state_ptr::in, bool::out) is det.
:- semipure pred pull_thread(lua_state_ptr::in, lua_state_ptr::out) is det.


:- impure pred push_number(lua_state_ptr::in, float::in) is det.
:- impure pred push_nil(lua_state_ptr::in) is det.
:- impure pred push_userdata(lua_state_ptr::in, T::in) is det.
:- impure pred push_integer(lua_state_ptr::in, int::in) is det.
:- impure pred push_lightuserdata(lua_state_ptr::in, c_pointer::in) is det.
:- impure pred push_string(lua_state_ptr::in, string::in) is det.
:- impure pred push_boolean(lua_state_ptr::in, bool::in) is det.
:- impure pred push_thread(lua_state_ptr::in, lua_state_ptr::in) is det.




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- implementation.

:- import_module require.
:- import_module type_desc.

:- func return_nil = nil.

return_nil = nil.

:- pragma foreign_export("C", return_nil = out, "luaMR_nil").


:- pragma foreign_proc("C", new_state = L::out, 
	[promise_pure, will_not_call_mercury],
"
	L = luaL_newstate();
").

:- pragma foreign_proc("C", get_status(L::in, Status::out), 
	[promise_semipure, will_not_call_mercury],
"
	Status = lua_status(L);
").

:- pragma foreign_enum("C", lua_status/0, 
	[
	ready - 0,
	yield - "LUA_YIELD",
	runtime_error - "LUA_ERRRUN",
	syntax_error - "LUA_ERRSYNTAX",
	memory_error - "LUA_ERRMEM",
	unhandled_error - "LUA_ERRERR"
] ).


:- pragma foreign_proc("C", get_top(L::in, Index::out),
	[promise_semipure, will_not_call_mercury],
"
	Index = lua_gettop(L);
").

	
	%
:- pragma foreign_proc("C",  set_top(L::in, Index::in),
	[will_not_call_mercury],
"
	lua_settop(L, Index);
").

:- pragma foreign_proc("C",  check_stack(L::in, Free::in),
	[will_not_call_mercury],
"
	lua_checkstack(L, Free);
").

:- pragma foreign_proc("C",  get_type(L::in, Index::in, Type::out),
	[promise_semipure, will_not_call_mercury],
"
	Type = lua_type(L, Index);
").


pull(L, Index, T) :- 
	( T:nil, 
		semipure is_nil(L, Index) -> T = nil
	; T:int, 
		semipure is_integer(L, Index) ->
		semipure pull_integer(L, Index, T)
	; T:float, 
		semipure is_number(L, Index) ->
		semipure pull_number(L, Index, T)
	; T:bool, 
		semipure is_boolean(L, Index) ->
		semipure pull_boolean(L, Index, T)
	; T:string, 
		semipure is_string(L, Index) ->
		semipure pull_string(L, Index, T)
	; T:c_pointer, 
		semipure is_lightuserdata(L, Index) ->
		semipure pull_lightuserdata(L, Index, T)
	; T:lua_state_pointer, 
		semipure is_thread(L, Index) ->
		semipure pull_thread(L, Index, T)
	; pull_userdata(L, Index, T).
	

push(L, T) :- 
	( T:nil ->
		impure push_nil(L)
	; T:int ->
		impure push_integer(L, int)
	; T:float ->
		impure push_number(L, float)
	; T:bool ->
		impure push_boolean(L, bool)
	; T:string ->
		impure push_string(L, string)
	; T:char ->
		impure push_string(L, char)
	; T:c_pointer ->
		impure push_lightuserdata(L, T)
	; T:c_function ->
		sorry($module, $pred)
	; T:lua_state_ptr ->
		impure push_thread(L, T)
	; impure push_userdata(L, T).



:- pragma foreign_proc("C",  pop(L::in, Num::in),
	[will_not_call_mercury],
"
	lua_pop(L, Num);
").


:- pragma foreign_proc("C",  call(L::in, Args::in, Err::in) = Status,
	[will_not_call_mercury],
"
	/* TODO */
MR_fatal_error("lua.state.pcall/3 has not yet been implemented. Sorry.");
").




:- pragma foreign_proc("C",  push_global(L::in, Name::in),
	[promise_semipure, will_not_call_mercury],
"
	lua_getglobal(L, Name);	
").

get_global(L, Name, T) :- 
	promise_semipure (
		impure push_global(L, Name),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).
	

:- pragma foreign_proc("C",  pop_global(L::in, Name::in),
	[will_not_call_mercury],
"
	lua_setglobal(L, Name)
").

set_global(L, Name, T) :-
	impure push(L, T),
	impure pop_global(L, Name).

:- pragma foreign_proc("C",  push_registry(L::in, Key::in),
	[promise_semipure, will_not_call_mercury],
"
	luaMR_getregistry(L, Key);
").

get_registry(L, Key, T) :-
	promise_semipure (
		impure push_registry(L, Name),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).


:- pragma foreign_proc("C",  pop_registry(L::in, Key::in),
	[will_not_call_mercury],
"
	luaMR_setregistry(L, Key);
").

set_registry(L, Name, T) :-
	impure push(L, T),
	impure pop_registry(L, Name).
	

:- pragma foreign_proc("C",  push_upvalue(L::in, Id::in),
	[promise_semipure, will_not_call_mercury],
"
		luaMR_getupvalue(L, Id);
").

get_upvalue(L, Id, T) :-
	promise_semipure (
		impure push_upvalue(L, Id),
		semipure get_stack(L, -1),
		impure pop(L, 1)
	).


:- pragma foreign_proc("C",  pop_upvalue(L::in, Key::in),
	[will_not_call_mercury],
"
	luaMR_setupvalue(L, Id);
").

set_upvalue(L, Name, T) :-
	impure push(L, T),
	impure pop_upvalue(L, Name).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%


:- pragma foreign_proc("C", is_number(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isnumber(L, Index);
").

:- pragma foreign_proc("C", is_nil(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isnil(L, Index);
").

:- pragma foreign_proc("C", is_userdata(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isuserdata(L, Index);
").

:- pragma foreign_proc("C", is_integer(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	if(lua_isnumber(L, Index));
	 SUCCESS_INDICATOR = 
	 	!(lua_tonumber(L, Index) - lua_tointeger(L, Index));
").

:- pragma foreign_proc("C", is_lightuserdata(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_islightuserdata(L, Index);
").

:- pragma foreign_proc("C", is_string(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isstring(L, Index);
").

:- pragma foreign_proc("C", is_boolean(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isboolean(L, Index);
").

:- pragma foreign_proc("C", is_thread(L::in, Index::in),
	[promise_semipure, will_not_call_mercury],
"
	 SUCCESS_INDICATOR = lua_isthread(L, Index);
").


:- pragma foreign_proc("C", pull_number(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tonumber(L, Index);
").

:- pragma foreign_proc("C", pull_userdata(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_touserdata(L, Index);
").

:- pragma foreign_proc("C", pull_integer(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tointeger(L, Index);
").
:- pragma foreign_proc("C", pull_lightuserdata(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tolightuserdata(L, Index);
").

:- pragma foreign_proc("C", pull_string(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tostring(L, Index);
").

:- pragma foreign_proc("C", pull_boolean(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 if(lua_toboolean(L, Index))
	 	V = MR_YES;
	 else
	 	V = MR_NO;
").

:- pragma foreign_proc("C", pull_thread(L::in, Index::in, V::out),
	[promise_semipure, will_not_call_mercury],
"
	 V = lua_tothread(L, Index);
").

:- pragma foreign_proc("C", push_number(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushnumber(L, V);
").

:- pragma foreign_proc("C", push_nil(L::in),
	[will_not_call_mercury],
"
	 lua_pushnil(L);
").

:- pragma foreign_proc("C", push_userdata(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushuserdata(L, V);
").

:- pragma foreign_proc("C", push_integer(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushinteger(L, V);
").

:- pragma foreign_proc("C", push_lightuserdata(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushlightuserdata(L, V);
").

:- pragma foreign_proc("C", push_string(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushstring(L, V);
").

:- pragma foreign_proc("C", push_boolean(L::in, V::in),
	[will_not_call_mercury],
"
	 if(V == MR_YES)
	 	lua_pushboolean(L, 1);
	 else
	 	lua_pushboolean(L, 0);
").

:- pragma foreign_proc("C", push_thread(L::in, V::in),
	[will_not_call_mercury],
"
	 lua_pushthread(L, V);
").


