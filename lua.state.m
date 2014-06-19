:- module lua.state.

% TODO: Implement pushing userdata.

:- interface.

:- import_module io, exception.


/* This type represents a refrence to the Lua VM, in Mercury it should be 
treated as a unique value, frozen in time, to preserve both Mercury's 
declarative semantics and Lua's imperative semantics. */
:- type lua_state.

:- pred do(lua_state, pred(lua_state, lua_state)).
:- mode do(in, pred(mdi, muo) is det) is det.
:- mode do(in, pred(mdi, muo) is semidet) is semidet.


:- pred locked(lua_state::in) is semidet.






:- type lua_op == pred(lua_state, lua_state).
:- inst op_det ---> (pred(mdi, muo) is det).
:- inst op_sem ---> (pred(mdi, muo) is semidet).

:- inst lua_op ---> op_det ; op_sem.

:- mode opi == in(lua_op).
:- mode opo == out(lua_op).

:- mode ldi == in(op_det).
:- mode ldo == out(op_det).
:- mode lsi == in(op_sem).
:- mode lso == out(op_sem).


:- func lua_op , lua_op = lua_op.
:- mode ldi , ldi = ldo.
:- mode opi , lsi = lso.
:- mode lsi, opi = lso. 

:- func lua_op ; lua_op = lua_op.
:- mode ldi ; opi = ldo.
:- mode lsi ; opi = lso.

:- type op_else ---> { lua_op ; lua_op }.

:- inst then_det ---> { ldi ; opi }.

:- inst then_sem ---> { lsi ; opi }.

:- inst else_sem ---> { lsi ; opi } ; { ldi ; lsi }.

:- inst else_det --->  { ldi ; ldi }.

:- func lua_op -> op_else = lua_op.
:- mode ldi -> in(then_det) = ldo.
:- mode ldi -> in(then_sem) = lso.
:- mode lsi -> in(else_det) = ldo.
:- mode lsi -> in(else_sem) = lso.

:- func not lua_op = lua_op.
:- mode not lsi = lsi.







:- implementation.

:- pragma require_feature_set([trailing]).

:- type lua == lua_state.

:- pragma foreign_type("C", lua_state, "lua_State *").



do(L0, Op) :-
	lock(L0, L1) , 
	call(Op, L1, L2) -> 
		unlock(L2) ;
		unlock(L1) , fail.
		

:- pragma foreign_proc("C", locked(L::di, L1::muo), 
	[will_not_call_mercury, promise_pure], "
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	SUCCESS_INDICATOR = lua_toboolean(L, -1);
	lua_pop(L, 1);
	L1 = L; ").

:- pred lock(lua::in, lua::muo) is det.

:- pragma foreign_proc("C", lock(L::di, L1::muo, _), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
").

:- func unlock(lua::mdi) is det.

:- pragma foreign_proc("C", unlock(L::mdi), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
").

:- pred choicepoint(lua::mdi, lua::muo) is det.

:- pragma foreign_proc("C", choicepoint(L::mdi, L1::muo), 
	[will_not_call_mercury, promise_pure], "
	luaAP_choicepoint(L);
	L1 = L;
").

:- pragma foreign_decl("C", "

	typedef struct lua_Trail {
		lua_State * L;
		int top;
		} lua_Trail;

	void luaAP_choicepoint(lua_State *);

	void luaAP_untrail(lua_Trail *, MR_untrail_reason); 
").

:- pragma foreign_code("C", "

	void luaAP_choicepoint(lua_State * L) {
		lua_Trail * t = malloc(sizeof(lua_Trail));
		t.L = L;
		t->top = lua_gettop(L);
		
		MR_trail_function(luaAP_untrail, t);
	}

	void luaAP_untrail(lua_Trail * t,
        MR_untrail_reason reason)
    {

        switch(reason) {
            case MR_undo:       /* Fall through. */
            case MR_exception:  /* Fall through. */
            case MR_retry:
                lua_State L = t.L;
       			int old = trail->top;
				int top = lua_gettop(L);
				if (old > top)
					MR_fatal_error(
	""Found more values on the Lua stack on backtrack than should be there."");
				
				if (old < top);
				lua_settop(L, old);

                
                break;

            case MR_solve:  /* Fall through */
            case MR_commit: 
                break;

            default:
                MR_fatal_error(""lua.state.m: unknown MR_untrail_reason"");
        }
    }



:- pred lua_get_type(lua::mdi, lua::muo, index::in, lua_type::out)
	is det.

:- pred lua_get_top(lua::mdi, lua::muo, index::out) is det.

:- pred lua_check_stack_semidet(lua::mdi, lua::muo, index::in) 
	is semidet.

:- pred lua_check_stack(lua::mdi, lua::muo, index::in) is det.

:- pred lua_raw_equal(lua::mdi, lua::muo, index::in, index::in)
	is semidet.


:- pragma foreign_proc("C", lua_get_type(L::di, L1::muo, Index::in, Type::out), 
	[will_not_call_mercury, promise_pure],
	"Type = lua_type(L, I);
	L1 = L; ").
	
lua_get_type(L, I) = Type :- lua_get_type(L, I, Type).

:- pragma foreign_proc("C", lua_get_top(L::di, L1::muo, Top::out), 
	[will_not_call_mercury, promise_pure],
	"Top = lua_gettop(L);
	L1 = L; ").
	
lua_get_top(L) = Top :- lua_get_top(L, Top).

:- pragma foreign_proc("C", lua_check_stack_semidet(L::di, L1::muo, N::in), 
	[will_not_call_mercury, promise_pure],
	"SUCCESS_INDICATOR = lua_checkstack(L, N);
	L1 = L; ").
	
:- check_stack(L, N) :- lua_check_stack_semidet(L, N) ;
	throw("Failed to allocate new elements on the Lua stack.").

:- pragma foreign_proc("C", lua_raw_equal(L::di, L1::muo, I1::in, I2::in), 
	[will_not_call_mercury, promise_pure],
	"SUCCESS_INDICATOR = lua_rawequal(L, I1, I2);
	L1 = L; ").






:- pred lua_push_value(lua::mdi, lua::muo, index::in) is det.
:- pred lua_push_nil(lua::mdi, lua::muo) is det.
:- pred lua_push_int(lua::mdi, lua::muo, int::in) is det.
:- pred lua_push_float(lua::mdi, lua::muo, float::in) is det.
:- pred lua_push_string(lua::mdi, lua::muo, string::in) is det.
:- pred lua_push_bool(lua::mdi, lua::muo, bool::in) is det.
:- pred lua_push_thread(lua::mdi, lua::muo) is det.
:- pred lua_push_thread(lua::mdi, lua::muo, bool::out) is det.
:- pred lua_push_c_function(lua::mdi, lua::muo, c_function::in) is det.
:- pred lua_push_c_pointer(lua::mdi, lua::muo, c_pointer::in) is det.
:- pred lua_push_lightuserdata(lua::mdi, lua::muo, lightuserdata::in) 
	is det.



:- pragma foreign_type("C", lua_state, "lua_State *").

:- pragma foreign_type("C", c_function, "lua_CFunction").

:- pragma foreign_proc("C", lua_push_value(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushvalue(L, (lua_Integer)T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_nil(L::di, L1::muo, I::di, O::uo), 
	[promise_pure, will_not_call_mercury], "lua_pushnil(L);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_int(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushinteger(L, (lua_Integer)T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_int(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushinteger(L, (lua_Integer)T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_float(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushnumber(L, (lua_Number)T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_string(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_push(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_bool(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], "
	if (T == MR_YES)
		lua_pushboolean(L, 1);
	else
		lua_pushboolean(L, 0);
").

:- pragma foreign_proc("C", lua_push_thread(L::di, L1::muo), 
	[promise_pure, will_not_call_mercury], "lua_pushthread(L);
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_thread(L::di, L1::muo, B::out), 
	[promise_pure, will_not_call_mercury], "
	if(lua_pushthread(L)
		B = MR_YES;
	else
		B = MR_NO;
;
	L1 = L; ").

:- pragma foreign_proc("C", lua_push_c_function(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_pushcfunction(L, T);
	L1 = L; ").

lua_push_lightuserdata(L, T) :- push_c_function(L, T).



:- pred lua_xmove(lua::mdi, lua::muo, lua_state::in, int::in) is det.

:- pragma foreign_proc("C", lua_xmove(L1::in, L2::in, N::in), 
	[promise_pure, will_not_call_mercury], 
	"lua_xmove(L1, L2, N);
	L1 = L; ").


:- pred lua_is_nil(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_number(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_string(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_boolean(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_table(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_function(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_c_function(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_thread(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_userdata(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_c_pointer(lua::mdi, lua::muo, int::in) is semidet.
:- pred lua_is_lightuserdata(lua::mdi, lua::muo, int::in) is semidet.

:- implementation.

:- pragma foreign_proc("C", lua_is_nil(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isnil(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_number(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isnumber(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_string(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isstring(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_boolean(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isboolean(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_function(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isfunction(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_c_function(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_iscfunction(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_thread(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isthread(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_userdata(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_isuserdata(L, T);
	L1 = L; ").

:- pragma foreign_proc("C", lua_is_c_pointer(L::di, L1::muo, T::in), 
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = lua_is(L, T);
	L1 = L; ").

lua_is_lightuserdata(L, T) :- lua_is_c_pointer(L, T);





:- pred lua_to_int(lua::mdi, lua::muo, index::in, int::out) is semiddet.
:- pred lua_to_float(lua::mdi, lua::muo, index::in, float::out) 
	is semiddet.
:- pred lua_to_string(lua::mdi, lua::muo, index::in, string::out) 
	is semiddet.
:- pred lua_to_bool(lua::mdi, lua::muo, index::in, bool::out) 
	is semiddet.
:- pred lua_to_thread(lua::mdi, lua::muo, index::in, lua_state::out) 
	is semiddet.
:- pred lua_to_c_function(lua::mdi, lua::muo, index::in, c_function::out) 
	is semiddet.
:- pred lua_to_c_pointer(lua::mdi, lua::muo, index::in, c_pointer::out) 
	is semiddet.
:- pred lua_to_lightuserdata(lua::mdi, lua::muo, index::in, lightuserdata::out, io::di, 
	io::uo) is semiddet.



lua_to_int(L, Index, T) :- lua_is_number(L, Index), 
	lua_to_int_unsafe(L, Index, T).

:- pred lua_to_int_unsafe(lua::mdi, lua::muo, int::in, int::out) is det.

:- pragma foreign_proc("C", lua_to_int_unsafe(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], 
	"T = (MR_Integer)lua_tointeger(L, N);
	L1 = L; ").

lua_to_float(L, Index, T) :- lua_is_number(L, Index), 
	lua_to_float_unsafe(L, Index, T).

:- pred lua_to_float_unsafe(lua::mdi, lua::muo, int::in int::out) is det.

:- pragma foreign_proc("C", lua_to_float_unsafe(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], 
	"T = (MR_Float)lua_tonumber(L, N);
	L1 = L; ").

:- pragma foreign_proc("C", lua_to_string(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], "
	T = (MR_String)lua_tostring(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;
	L1 = L; ").

:- pragma foreign_proc("C", lua_to_bool(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], "
	T = lua_toboolean(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;
	L1 = L; ").

:- pragma foreign_proc("C", lua_to_thread(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], "
	T = lua_tothread(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;
	L1 = L; ").

:- pragma foreign_proc("C", lua_to_c_function(L::di, L1::muo, N::in, T::out), 
	[promise_pure, will_not_call_mercury], "
	T = lua_tocfunction(L, N);
	if (T = NULL) SUCCESS_INDICATOR = 0; else SUCCESS_INDICATOR = 1;
	L1 = L; ").

lua_to_c_pointer(L, Index, T) :- lua_is_lightuserdata(L, Index), 
	lua_to_lightuserdata_unsafe(L, Index, T).

:- pred lua_to_c_pointer_unsafe(lua::mdi, lua::muo, int::in, c_pointer::out) 
	is det.

:- pragma foreign_proc("C", 
	lua_to_c_pointer_unsafe(L::di, L1::muo, N::in, T::out),
	[promise_pure, will_not_call_mercury], 
	"T = lua_touserdata(L, N);
	L1 = L; ").

lua_to_lightuserdata(L, N, T) :- lua_to_c_pointer_unsafe(L, N, T).





