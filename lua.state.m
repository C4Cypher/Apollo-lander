:- module lua.state.

% TODO: Implement pushing userdata.

:- interface.

:- import_module exception.


/* This type represents a refrence to the Lua VM, in Mercury it should be 
treated as a unique value, frozen in time, to preserve both Mercury's 
declarative semantics and Lua's imperative semantics. */
:- type lua_state.


% State variable to be passed when manipulating the stack.
:- type lua_io.

:- type io == lua_io.

:- mode li == mdi.

:- mode lo == muo.

:- pred do(lua_state::in, io::lo) is det.
:- func do(lua_state::in) = io::lo.

:- pred safe_do(lua_state::in, io::lo) is semidet.


:- pred end(io::li).
:- func end = io::li.

:- pred locked(lua_state) is semidet.


:- implementation.

:- pragma foreign_type("C", lua_state, "lua_State *").


:- pragma foreign_decl("C", "
	typedef struct luaAP_IO {
		lua_State * L;
		int top;
		int locked;
	} luaAP_IO; 
").

:- pragma foreign_type("C", lua_io, "luaAP_IO").


:- func new_io(lua_state::in) = lua_io::lo.

pragma foreign_proc("C", new_io(L::in) = (IO::lo),
	[will_not_call_mercury, promise_pure], "
	luaAP_IO new_io;
	new_io.L = L;
	new_io.top = lua_gettop(L);
	new_io.locked = io.top;
	IO = new_io;
").

do(L, IO) :- safe_do(L, IO) ; throw("Apollo/Mercury attempted to perform an operation on a locked Lua state").

safe_do(L, new_io(L)) :- not locked(L). 




:- pragma foreign_proc("C", locked(L::in), 
	[will_not_call_mercury, promise_pure], "
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	SUCCESS_INDICATOR = lua_toboolean(L, -1);
	lua_pop(L, 1);").

:- func lock(lua_state::in) = lua_state::out is det.

:- pragma foreign_proc("C", lock(L::in) = (R::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	R = L;").

:- func unlock(lua_state::in) = lua_state::out is det.

:- pragma foreign_proc("C", unlock(L::in) = (R::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	R = L;").
	




:- interface.

:- pred get_type(index::in, lua_type::out, io::li, io::lo)
	is det.

:- func get_type(index::in, io::li, io::lo) = lua_type::out is det.

:- pred get_top(lua_state::in, index::out, io::di, io::uo) is det.
:- func get_top(lua_state::in, io::in, io::uo) = int::out is det.

:- pred check_stack_semidet(lua_state::in, index::in, io::di, io::uo) 
	is semidet.

:- pred check_stack(lua_state::in, index::in, io::di, io::uo) is det.

:- pred raw_equal(lua_state::in, index::in, index::in, io::di, io::uo)
	is semidet.

:- implementation.

:- pragma foreign_proc("C", get_type(L::in, Index::in, Type::out, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"Type = lua_type(L, I);").
	
get_type(L, I, !IO) = Type :- get_type(L, I, Type, !IO).

:- pragma foreign_proc("C", get_top(L::in, Top::out, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"Top = lua_gettop(L);").
	
get_top(L, !IO) = Top :- get_top(L, Top, !IO).

:- pragma foreign_proc("C", check_stack_semidet(L::in, N::in, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"SUCCESS_INDICATOR = lua_checkstack(L, N);").
	
:- check_stack(L, N, !IO) :- check_stack_semidet(L, N, !IO) ;
	throw("Failed to allocate new elements on the Lua stack.").

:- pragma foreign_proc("C", raw_equal(L::in, I1::in, I2::in, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"SUCCESS_INDICATOR = lua_rawequal(L, I1, I2);").

:- interface.




:- pred push_value(lua_state::in, index::in, io::di, io::uo) is det.
:- pred push_nil(lua_state::in, io::di, io::uo) is det.
:- pred push_int(lua_state::in, int::in, io::di, io::uo) is det.
:- pred push_float(lua_state::in, float::in, io::di, io::uo) is det.
:- pred push_string(lua_state::in, string::in, io::di, io::uo) is det.
:- pred push_bool(lua_state::in, bool::in, io::di, io::uo) is det.
:- pred push_thread(lua_state::in, io::di, io::uo) is det.
:- pred push_thread(lua_state::in, bool::out, io::di, io::uo) is det.
:- pred push_c_function(lua_state::in, c_function::in, io::di, io::uo) is det.
:- pred push_c_pointer(lua_state::in, c_pointer::in, io::di, io::uo) is det.
:- pred push_lightuserdata(lua_state::in, lightuserdata::in, io::di, io::uo) 
	is det.

:- implementation.

:- pragma foreign_type("C", lua_state, "lua_State *").

:- pragma foreign_type("C", c_function, "lua_CFunction").

:- pragma foreign_proc("C", push_value(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushvalue(L, (lua_Integer)T);").

:- pragma foreign_proc("C", push_nil(L::in, I::di, O::uo), 
	[promise_pure, will_not_call_mercury], "lua_pushnil(L);").

:- pragma foreign_proc("C", push_int(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushinteger(L, (lua_Integer)T);").

:- pragma foreign_proc("C", push_int(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushinteger(L, (lua_Integer)T);").

:- pragma foreign_proc("C", push_float(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushnumber(L, (lua_Number)T);").

:- pragma foreign_proc("C", push_string(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_push(L, T);").

:- pragma foreign_proc("C", push_bool(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	if (T == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
").

:- pragma foreign_proc("C", push_thread(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "lua_pushthread(L);").

:- pragma foreign_proc("C", push_thread(L::in, B::out, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	if(lua_pushthread(L)
		B = MR_YES;
	else
		B = MR_NO;
;").

:- pragma foreign_proc("C", push_c_function(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushcfunction(L, T);").

push_lightuserdata(L, T, !IO) :- push_c_function(L, T, !IO).

:- interface.

:- pred xmove(lua_state::in, lua_state::in, int::in, io::di, io::uo) is det.

:- implementation.

:- pragma foreign_proc("C", xmove(L1::in, L2::in, N::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"lua_xmove(L1, L2, N);").

:- interface.


:- pred is_nil(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_number(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_string(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_boolean(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_table(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_function(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_c_function(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_thread(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_userdata(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_c_pointer(lua_state::in, int::in, io::di, io::uo) is semidet.
:- pred is_lightuserdata(lua_state::in, int::in, io::di, io::uo) is semidet.

:- implementation.

:- pragma foreign_proc("C", is_nil(L::in, T::in, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isnil(L, T);").

:- pragma foreign_proc("C", is_number(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isnumber(L, T);").

:- pragma foreign_proc("C", is_string(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isstring(L, T);").

:- pragma foreign_proc("C", is_boolean(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isboolean(L, T);").

:- pragma foreign_proc("C", is_function(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isfunction(L, T);").

:- pragma foreign_proc("C", is_c_function(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_iscfunction(L, T);").

:- pragma foreign_proc("C", is_thread(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isthread(L, T);").

:- pragma foreign_proc("C", is_userdata(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isuserdata(L, T);").

:- pragma foreign_proc("C", is_c_pointer(L::in, T::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_is(L, T);").

is_lightuserdata(L, T, !IO) :- is_c_pointer(L, T, !IO);



:- interface.

:- pred to_int(lua_state::in, index::in, int::out, io::di, io::uo) is semiddet.
:- pred to_float(lua_state::in, index::in, float::out, io::di, io::uo) 
	is semiddet.
:- pred to_string(lua_state::in, index::in, string::out, io::di, io::uo) 
	is semiddet.
:- pred to_bool(lua_state::in, index::in, bool::out, io::di, io::uo) 
	is semiddet.
:- pred to_thread(lua_state::in, index::in, lua_state::out, io::di, io::uo) 
	is semiddet.
:- pred to_c_function(lua_state::in, index::in, c_function::out, io::di, io::uo) 
	is semiddet.
:- pred to_c_pointer(lua_state::in, index::in, c_pointer::out, io::di, io::uo) 
	is semiddet.
:- pred to_lightuserdata(lua_state::in, index::in, lightuserdata::out, io::di, 
	io::uo) is semiddet.

:- implementation.

to_int(L, Index, T, !IO) :- is_number(L, Index, !IO), 
	to_int_unsafe(L, Index, T, !IO).

:- pred to_int_unsafe(lua_state::in, int::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", to_int_unsafe(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], 
	"T = (MR_Integer)lua_tointeger(L, N);").

to_float(L, Index, T, !IO) :- is_number(L, Index, !IO), 
	to_float_unsafe(L, Index, T, !IO).

:- pred to_float_unsafe(lua_state::in, int::in int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", to_float_unsafe(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], 
	"T = (MR_Float)lua_tonumber(L, N);").

:- pragma foreign_proc("C", to_string(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], "
	T = (MR_String)lua_tostring(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;").

:- pragma foreign_proc("C", to_bool(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], "
	T = lua_toboolean(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;").

:- pragma foreign_proc("C", to_thread(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], "
	T = lua_tothread(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;").

:- pragma foreign_proc("C", to_c_function(L::in, N::in, T::out, _I::di, _O::uo),, 
	[promise_pure, will_not_call_mercury], "
	T = lua_tocfunction(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;").

to_c_pointer(L, Index, T, !IO) :- is_lightuserdata(L, Index, !IO), 
	to_lightuserdata_unsafe(L, Index, T, !IO).

:- pred to_c_pointer_unsafe(lua_state::in, int::in, c_pointer::out, io::di, 
	io::uo) is det.

:- pragma foreign_proc("C", to_c_pointer_unsafe(L::in, N::in, T::out, 
	_I::di, _O::uo), [promise_pure, will_not_call_mercury], 
	"T = lua_touserdata(L, N);").

to_lightuserdata(L, N, T, !IO) :- to_c_pointer_unsafe(L, N, T, !IO).





