:- module lua.api.m

:- interface.
:- pred check_stack(int::in, lua_state::in, lua_state::out) is semidet.

:- implementation.
:- pragma foreign_proc("C", check_stack(N::in, L:in, O::out), [promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_checkstack(L, N);
	O = L; ").

:- interface.	
:- func gettop(lua_state) = int is det.

:- implementation.
:- pragma foreign_proc("C", gettop(L::in) = (O::out), [promise_pure, will_not_call_mercury],
	"O = lua_gettop(L);").
