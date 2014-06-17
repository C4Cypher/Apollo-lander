:- module lua.value.

:- interface.

:- import_module lua.state.


:- typeclass lua_value(T) where [
	% Push a value onto the stack
	pred push(lua_state, T, io.state, io.state),
	mode push(in, in, di, uo) is det,
    
	% Get a copy of the value at the supplied index
	% Fail if wrong type
	% Leave the stack in the same state it was found
	func pull(lua_state, lua_index, io.state, io.state) = T,
	mode pull(in, in, di, uo) = out is semidet ].


:- instance lua_value(int).
:- instance lua_value(float).
:- instance lua_value(bool).
:- instance lua_value(string).
    
:- implementation.

:- instance lua_value(int) where [
	( push(L, T, !IO) :- push_int(L, T, !IO) ),
	( pull(L, I, !IO) = V :- is_int(L, I, !IO), to_int(L, I, V, !IO) ) ].
	
:- instance lua_value(float) where [
	( push(L, T, !IO) :- push_float(L, T, !IO) ),
	( pull(L, I, !IO) = V :- is_float(L, I, !IO), to_float(L, I, V, !IO) ) ].
	
:- instance lua_value(bool) where [
	( push(L, T, !IO) :- push_bool(L, T, !IO) ),
	( pull(L, I, !IO) = V :- is_bool(L, I, !IO), to_bool(L, I, V, !IO) ) ].
	
:- instance lua_value(string) where [
	( push(L, T, !IO) :- push_string(L, T, !IO) ),
	( pull(L, I, !IO) = V :- is_string(L, I, !IO), to_string(L, I, V, !IO) ) ].






