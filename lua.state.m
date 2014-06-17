:- module lua.state.

% TODO: Implement pushing userdata.

:- interface.

:- import_module io, int, float, string, bool.

:- type io == io.state

/* This type represents a refrence to the Lua VM, in Mercury it should be 
treated as a unique value, frozen in time, to preserve both Mercury's 
declarative semantics and Lua's imperative semantics. */
:- type lua_state.

% Lua can refrence C pointers as lightuserdata
:- type lightuserdata == c_pointer.

:- implementation.

:- pragma foreign_type("C", lua_state, "lua_State *").

:- interface.

:- pred get_type(lua_state::in, int::in, lua_type::out, io::di, io::uo) is det.
:- func get_type(lua_state::in, int::in, io::di, io::uo) = lua_type::out is det.

:- pred get_top(lua_state::in, int::out, io::di, io::uo) is det.
:- func get_top(lua_state::in, state.io::in, state.io::uo) = int::out is det.

:- pred check_stack(lua_state::in, int::in, io::di, io::uo) is semidet.

:- implementation.

:- pragma foreign_proc("C", get_type(L::in, Index::in, Type::out, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"Type = lua_type(L, I);").
	
get_type(L, I, !IO) = Type :- get_type(L, I, Type, !IO).

:- pragma foreign_proc("C", get_top(L::in, Top::out, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"Top = lua_gettop(L);").
	
get_top(L, !IO) = Top :- get_top(L, Top, !IO).

:- pragma foreign_proc("C", check_stack(L::in, N::in, _I::di, _O::uo), 
	[will_not_call_mercury, promise_pure],
	"SUCCESS_INDICATOR = lua_checkstack(L, N);").


:- interface.

:- type io == io.state.

:- pred push_value(lua_state::in, int::in, io::di, io::uo) is det.
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


:- pred is_nil(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_number(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_string(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_boolean(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_table(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_function(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_c_function(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_thread(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_userdata(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_c_pointer(lua_state::in, int::in io::di, io::uo) is semidet.
:- pred is_lightuserdata(lua_state::in, int::in io::di, io::uo) is semidet.

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

:- pred to_int(lua_state::in, int::in, int::out, io::di, io::uo) is semiddet.
:- pred to_float(lua_state::in, int::in, float::out, io::di, io::uo) 
	is semiddet.
:- pred to_string(lua_state::in, int::in, string::out, io::di, io::uo) 
	is semiddet.
:- pred to_bool(lua_state::in, int::in, bool::out, io::di, io::uo) is semiddet.
:- pred to_thread(lua_state::in, int::in, lua_state::out, io::di, io::uo) 
	is semiddet.
:- pred to_c_function(lua_state::in, int::in, c_function::out, io::di, io::uo) 
	is semiddet.
:- pred to_c_pointer(lua_state::in, int::in, c_pointer::out, io::di, io::uo) 
	is semiddet.
:- pred to_lightuserdata(lua_state::in, int::in, lightuserdata::out, io::di, 
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





