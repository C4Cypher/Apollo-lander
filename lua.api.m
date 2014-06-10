:- module lua.api.m

:- interface.
:- pred check_stack(int::in, lua_state::in, lua_state::out) is semidet.

:- implementation.
:- pragma foreign_proc("C", check_stack(N::in, L:in, O::out), [promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_checkstack(L, N);
	O = L; ").

%%%%-------------------------------------------------------------------------------------------------------------------%%%%

:- interface.	
:- func gettop(lua_state) = int is det.

:- implementation.
:- pragma foreign_proc("C", gettop(L::in) = (O::out), [promise_pure, will_not_call_mercury],
	"O = lua_gettop(L);").

%%%%-------------------------------------------------------------------------------------------------------------------%%%%
:- interface.	
:- pred pop(int, lua_state, lua_state) is det.
:- pred pop(lua_state::in, lua_state::out) is det.

:- implementation.
:- pragma foreign_proc("C", pop(N::in, L::in, O::out), [promise_pure, will_not_call_mercury],
	"lua_pop(L, N); O = L;").

pop(In, Out) :- pop(1, In, Out).	

%%%%-------------------------------------------------------------------------------------------------------------------%%%%	
:- interface.	
:- pred push_global(string, lua_state, int) is det.

:- implementation.
:- pragma foreign_proc("C", push_global(S::in, L::in, O::out), [promise_pure, will_not_call_mercury], "
	lua_pushstring(L, S);
	lua_rawget(L, LUA_GLOBALSINDEX);
	O = lua_gettop(L);").

%%%%-------------------------------------------------------------------------------------------------------------------%%%%	
:- interface.	
:- pred push_local(int, lua_state, lua_state) is det.

:- implementation.
:- pragma foreign_proc("C", push_local(I::in, L::in, O::out), [promise_pure, will_not_call_mercury], "
	lua_pushvalue(L, I);
	O = L;").
	
%%%%-------------------------------------------------------------------------------------------------------------------%%%%	
:- interface.	
:- pred push_registry(string, lua_state, int) is det.

:- implementation.
:- pragma foreign_proc("C", push_registry(S::in, L::in, O::out), [promise_pure, will_not_call_mercury], "
	lua_pushstring(L, S);
	lua_rawget(L, LUA_REGISTRYINDEX);
	O = lua_gettop(L);").

%%%%-------------------------------------------------------------------------------------------------------------------%%%%	
:- interface.	
:- pred push_reference(int, lua_state, int) is det.

:- implementation.
:- pragma foreign_proc("C", push_registry(S::in, L::in, O::out), [promise_pure, will_not_call_mercury], "
	lua_pushstring(L, S);
	lua_rawget(L, LUA_REGISTRYINDEX);
	O = lua_gettop(L);").	

