:- module lua.value.

:- interface.

% push a value onto the stack

:- typeclass lua_value(T) where [
	% push a value onto the stack
	impure pred push_value(T, lua_state, lua_state),
	mode push_value(in, in, out) is semidet,
	
	% extract value from a lua_var, or, if it's a refrence, push it onto the stack and extract it from lua.
	some [T] func local_value(lua_state, int) = T,
	mode local_value(in, in) = out is semidet,
	
	% determine equivalent Lua type
	func type_of(T) = lua_type
].

	
:- instance lua_value(nil).
:- instance lua_value(bool).
:- instance lua_value(string).
:- instance lua_value(int).
:- instance lua_value(float).
:- instance lua_value(lua_state).
:- instance lua_value(c_function).
:- instance lua_value(c_pointer).
:- instance lua_value(map(lua_var,lua_var)).
:- instance lua_value(list(lua_var)).


:- implementation.
	
