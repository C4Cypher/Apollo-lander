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
	func pull(lua_state, int, io.state, io.state) = T,
	mode pull(in, in, di, uo) = out is semidet ].
    
    
    
:- implementation.
