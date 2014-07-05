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





% :- pragma foreign_proc("C", type(L::in, 





lua_call(MRcall, L, !IO) = Return :-
	call_function(MRcall, L, In, 

:- pred get_value(lua_state::in, int::in, lua_value::out, io.state::di, 
	io.state::uo) is semidet.

get_value(L, I, V, !IO) :-
	get_primitive(L, I, P, Type, !IO),
	  Type = nil -> V = nil
	; Type = number -> V = number(P)
	; Type = boolean -> V = boolean(P)
	; Type = string -> V = string(P)
	; Type = table ->
		push_value(L, nil, !IO) ,
		get_table(L, I, map.init, V)
	; Type = function ->
		V = function(ref(L, I))
	; Type = thread ->
		V = thread(ref(L, I))
	; Type = userdata ->
		V = userdata(ref(L, I))
	; Type = lightuserdata ->
		V = userdata(ref(L, I)).
:- pred push_value(lua_state::in, lua_value::out, io.state::di, 
	io.state::uo) is det.

push_value(L, nil, !IO) :- push_primitive(L, 0, nil, !IO).
push_value(L, integer(V), !IO) :- push_primitive(L, float(V), number, !IO).
push_value(L, number(V), !IO) :- push_primitive(L, V, number, !IO).
push_value(L, string(V), !IO) :- push_primitive(L, V, string, !IO).
push_value(L, boolean(V), !IO) :- push_primitive(L, V, boolean, !IO).

push_value(L, table(M), !IO) :- 
	push_primitive(L, 0, table, !IO)
	map.member(M, K, V),
	push_value(L,K, !IO),
	push_value(L,V, !IO),
	set_table(L, I, !IO).

push_value(L, function(ref(_, I)), !IO) :- push_primitive(L, I, function, !IO).
push_value(L, thread(ref(_, I)), !IO) :- push_primitive(L, I, thread, !IO).
push_value(L, userdata(ref(_, I)), !IO) :- push_primitive(L, I, userdata, !IO).
	


:- pred top(lua_state::in, int::out, io.state:di, io.state::uo).
:- pragma foreign_proc("C", top(L::in, Top::out, _I, _O), 
	"Top = lua_gettop(L);").

:- pred get_primitive(lua_state::in, int::in, T::out, lua_type::out, 
	io.state::di, io.state::uo) is det.


:- pragma foreign_proc("C", get_primitive(L::in, I::in, V::out, Type::out,
	 _, _),	[promise_pure, will_not_call_mercury], "
		
	Type = lua_type(L, I);
	switch(Type)
	{
		case LUA_TNUMBER:
			V = (MR_Float) lua_tonumber(L, I);
			break;
		case LUA_TBOOLEAN:
			if(lua_toboolean(L, I))
				V = MR_YES;
			else
				V = MR_NO;
			break;
		case LUA_TSTRING:
			V = (MR_String) lua_tostring(L, I);
			break;
		default:
			V = 0;
		break;
	}"
).

:- pred push_table(lua_state::in, map(lua_value, lua_value)::in, io_state::di,
	io_state::uo) is semidet.




:- pred get_table(lua_state::in, int::in, map(lua_value, lua_value)::in, 
	map(lua_value, lua_value)::out, io.state::di, io.state::uo) is det.

get_table(L, I, !M, !IO) :-
	get_next(L, I, !IO) ->
		rawget(L, I) ,
		get_value(L, -2, K, !IO) ,
		get_value(L, -1, V, !IO) , 
		!:M = map.insert(!.M, K, V) ,	
		push_value(L, K, !IO) ,
		get_table(L, I, !M, !IO)
	;
		!:M = !.M.

:- pred rawget(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawget(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawget(L, I);").

:- pred rawset(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawset(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawset(L, I);").

:- pred pop(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", pop(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_pop(L, I);").

:- pred push_primitive(lua_state::in, int::in, T::in,  lua_type::in, 
	io.state::di, io::state::uo) is det.

:- pragma foreign_proc("C", push_primitive(L::in, V::in, Type::in, _, _),
	[promise_pure, will_not_call_mercury], "
	assert(lua_checkstack(L, 1));	
	
	
	switch(Type)
	{
		case LUA_TNIL:
			lua_pushnil(L);
			break;
		case LUA_TNUMBER:
			lua_pushnumber(L, (lua_Number)V);
			break;
		case LUA_TBOOLEAN:
			if(V = MR_YES)
				lua_pushboolean(L, 1);
			else
				lua_pushboolean(L, 0);
			break;
		case LUA_TSTRING:
			lua_pushstring(L, V);
			break;
		case LUA_TTABLE:
			lua_checkstack(L, 2);
			lua_newtable(L);
			break;
		default:
			lua_pushvalue(L, I);
		break;
	}"
).
		
:- pred get_next(lua_state::in, int::in, io.state::di,
	io.state::uo) is semidet.

:- pred foreign_proc("C", get_next(L::in, I::in, _I, _O),
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = lua_next(L, I);"
).
			
		



%%%%%%%%%%%%%%%%%
% The Lua State %
%%%%%%%%%%%%%%%%%


:- type lua_state.
:- pragma foreign_type("C", lua_state, "lua_State *").

:- type c_function.
:- pragma foreign_type("C", c_function, "lua_CFunction").

:- type index == int.

:- pred valid_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_index(L, I, !IO) :- I = global ; I = registry ; valid_stack_index(L, I, !IO).

:- pred valid_stack_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_stack_index(L, I, !IO) :- I \= 0 , abs(I) =< get_top(L, !IO).

%%%%%%%%%%%%%	
% Lua Types %
%%%%%%%%%%%%%

:- pragma foreign_enum("C", lua_type, [
	none - "LUA_TNONE",
	nil - "LUA_TNIL",
	boolean - "LUA_TBOOLEAN",
	lightuserdata - "LUA_TLIGHTUSERDATA",
	number - "LUA_TNUMBER",
	string - "LUA_TSTRING",
	table - "LUA_TTABLE",
	function - "LUA_TFUNCTION",
	userdata - "LUA_TUSERDATA",
	thread - "LUA_TTHREAD"
]).

% TODO type(V) = Type

%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%

:- pred push(lua_state, T, io.state, io.state).
:- pred some [T] pull(lua_state, index, T, io.state, io.state).


%%%%%%%%%%%%
% Trailing %
%%%%%%%%%%%%


:- pragma require_feature_set([trailing]).


:- pred locked(lua_state::in) is semidet.		
:- pred locked(lua_state::in, lua_state::out) is semidet.

 locked(L, L) :- locked(L).


:- pragma foreign_proc("C", locked(L::in), 
	[will_not_call_mercury, promise_pure], "
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	SUCCESS_INDICATOR = lua_toboolean(L, -1);
	lua_pop(L, 1);").

:- pred lock(lua::in, lua::muo) is det.

:- pragma foreign_proc("C", lock(L::in, L1::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	L1 = L;
").

:- pred unlock(lua::mdi, lua::out) is det.

:- pragma foreign_proc("C", unlock(L::mdi, L1::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	L1 = L;
").

:- pred unlock(lua::mdi) is det.

unlock(L) :- unlock(L, _).

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
		int ref;
		} lua_Trail;

	void luaAP_choicepoint(lua_State *, int);

	void luaAP_untrail(lua_Trail *, MR_untrail_reason); 
").

:- pragma foreign_code("C", "//c

	void luaAP_choicepoint(lua_State * L, int ref);
	{
		lua_Trail * t = malloc(sizeof(lua_Trail));
		t.L = L;
		t->ref = ref;
		t->top = lua_gettop(L);
		
		MR_trail_function(luaAP_untrail, t);
	}

	void luaAP_untrail(lua_Trail * t,
        MR_untrail_reason reason)
    {

        switch(reason) 
        {
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
").



%%%%%%%%%%%%%%%%%%%%% CLEANUP %%%%%%%%%%%%%

:- pred get_value(lua_state::in, int::in, lua_value::out, io.state::di, 
	io.state::uo) is semidet.


get_value(L, I, V, !IO) :-
	get_primitive(L, I, P, Type, !IO),
	  Type = nil -> V = nil
	; Type = number -> V = number(P)
	; Type = boolean -> V = boolean(P)
	; Type = string -> V = string(P)
	; Type = table ->
		push_value(L, nil, !IO) ,
		get_table(L, I, map.init, V)
	; Type = function ->
		V = function(ref(L, I))
	; Type = thread ->
		V = thread(ref(L, I))
	; Type = userdata ->
		V = userdata(ref(L, I))
	; Type = lightuserdata ->
		V = userdata(ref(L, I)).
:- pred push_value(lua_state::in, lua_value::out, io.state::di, 
	io.state::uo) is det.

push_value(L, nil, !IO) :- push_primitive(L, 0, nil, !IO).
push_value(L, integer(V), !IO) :- push_primitive(L, float(V), number, !IO).
push_value(L, number(V), !IO) :- push_primitive(L, V, number, !IO).
push_value(L, string(V), !IO) :- push_primitive(L, V, string, !IO).
push_value(L, boolean(V), !IO) :- push_primitive(L, V, boolean, !IO).

push_value(L, table(M), !IO) :- 
	push_primitive(L, 0, table, !IO)
	map.member(M, K, V),
	push_value(L,K, !IO),
	push_value(L,V, !IO),
	set_table(L, I, !IO).

push_value(L, function(ref(_, I)), !IO) :- push_primitive(L, I, function, !IO).
push_value(L, thread(ref(_, I)), !IO) :- push_primitive(L, I, thread, !IO).
push_value(L, userdata(ref(_, I)), !IO) :- push_primitive(L, I, userdata, !IO).
	


:- pred top(lua_state::in, int::out, io.state:di, io.state::uo).
:- pragma foreign_proc("C", top(L::in, Top::out, _I, _O), 
	"Top = lua_gettop(L);").

:- pred get_primitive(lua_state::in, int::in, T::out, lua_type::out, 
	io.state::di, io.state::uo) is det.


:- pragma foreign_proc("C", get_primitive(L::in, I::in, V::out, Type::out,
	 _, _),	[promise_pure, will_not_call_mercury], "
		
	Type = lua_type(L, I);
	switch(Type)
	{
		case LUA_TNUMBER:
			V = (MR_Float) lua_tonumber(L, I);
			break;
		case LUA_TBOOLEAN:
			if(lua_toboolean(L, I))
				V = MR_YES;
			else
				V = MR_NO;
			break;
		case LUA_TSTRING:
			V = (MR_String) lua_tostring(L, I);
			break;
		default:
			V = 0;
		break;
	}"
).

:- pred push_table(lua_state::in, map(lua_value, lua_value)::in, io_state::di,
	io_state::uo) is semidet.




:- pred get_table(lua_state::in, int::in, map(lua_value, lua_value)::in, 
	map(lua_value, lua_value)::out, io.state::di, io.state::uo) is det.

get_table(L, I, !M, !IO) :-
	get_next(L, I, !IO) ->
		rawget(L, I) ,
		get_value(L, -2, K, !IO) ,
		get_value(L, -1, V, !IO) , 
		!:M = map.insert(!.M, K, V) ,	
		push_value(L, K, !IO) ,
		get_table(L, I, !M, !IO)
	;
		!:M = !.M.

:- pred rawget(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawget(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawget(L, I);").

:- pred rawset(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", rawset(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_rawset(L, I);").

:- pred pop(lua_state::in, int::in, io.state::di, io.state::uo) is det.

:- pragma foreign_proc("C", pop(L::in, I::in, _, _),
	[promise_pure, will_not_call_mercury], "lua_pop(L, I);").

:- pred push_primitive(lua_state::in, int::in, T::in,  lua_type::in, 
	io.state::di, io::state::uo) is det.

:- pragma foreign_proc("C", push_primitive(L::in, V::in, Type::in, _, _),
	[promise_pure, will_not_call_mercury], "
	assert(lua_checkstack(L, 1));	
	
	
	switch(Type)
	{
		case LUA_TNIL:
			lua_pushnil(L);
			break;
		case LUA_TNUMBER:
			lua_pushnumber(L, (lua_Number)V);
			break;
		case LUA_TBOOLEAN:
			if(V = MR_YES)
				lua_pushboolean(L, 1);
			else
				lua_pushboolean(L, 0);
			break;
		case LUA_TSTRING:
			lua_pushstring(L, V);
			break;
		case LUA_TTABLE:
			lua_checkstack(L, 2);
			lua_newtable(L);
			break;
		default:
			lua_pushvalue(L, I);
		break;
	}"
).
		
:- pred get_next(lua_state::in, int::in, io.state::di,
	io.state::uo) is semidet.

:- pred foreign_proc("C", get_next(L::in, I::in, _I, _O),
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = lua_next(L, I);"
).





