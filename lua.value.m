:- module lua.value.

:- interface.

% push a value onto the stack
:- pred push(T::in, lua_context::in) is cc_nondet.
:- pred push(T::in, lua_context::mdi, lua_context::muo) is cc_nondet.

:- pred look(int::in, int::out, T::out, lua_context::in) is cc_nondet.
:- pred look(int::in, int::out, T::out lua_context::mdi, lua_context::muo) 
	is cc_nondet.


:- typeclass lua_value(T) where [
	% push a value onto the stack
	pred push(T, lua_context),
	mode push(in, in) is semidet,
	pred push(T, lua_context, lua_context),
	mode push(in, mdi, muo) is semidet,
	
	% unify with a value on the stack, starting at a given index
	% return the number of values matched
	pred look(int, int, T, lua_context),
	mode look(in, out, out, in),
	pred look(int, int, T, lua_context, lua context),
	mode look(in, out, out,  mdi, muo) is semidet].


:- implementation.
	
