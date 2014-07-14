/* This file is a dumping ground, a proverbial rubbish bin for snippets 
that I started to implement before realizing that I was on the wrong track.

Some of this is garbage, but I wanted to hold onto some of this, temporarily
in the event that I could use it again.  

Please take anything you find here with a mountain of salt, and know that this
file will not survive final implementation of the project. */


:- pred convert_error(lua_state, string, F, T).
:- mode convert_error(in, in, in,  in)  is erroneous.
:- mode convert_error(in, in, in,  out)  is erroneous.
:- mode convert_error(in, in, out, in)  is erroneous.
:- mode convert_error(in, in, out,  out)  is erroneous.

% Adapted from the body of univ.det_univ_to_type by fjh.
convert_error(L, FuncName, From, To) :- 
        FromTypeName = type_name(type_of(From)),
        ToTypeName = type_name(type_of(To)),
        string.append_list([FuncName, ": conversion failed\n",
            "\tFrom Type: ", FromTypeName, "\n",
            "\tTo Type: ", ToTypeName], ErrorString),
        lua_error(L, ErrorString).



:- pragma foreign_proc("C", var_to_ref(L::in, Var::in, Reg::in, Ref::out), 
	[promise_pure, may_call_mercury], 
"	Ref = luaAP_get_ref(L, Var, Reg);").	

:- pragma foreign_proc("C", var_to_ref(L::out, Var::out, Ref::in), 
	[promise_pure, will_not_call_mercury], "
	L = luaAP_ref_state(Ref);
	luaAP_push_ref(L, Ref);
	
	Var = luaAP_get_var(L)
	
	if(Var)
		SUCCESS_INDICATOR = 1;
	else
		SUCCESS_INDICATOR = 0;
		
	lua_pop(L, 1);

").





	% Through the Lua State, a mercury value can be passed as the value 
	% to a new Lua var.  int, float, bool, string and c_pointer are passed
	% by value, while other types are passed by refrence.
	%
	% The following predicates will pass Mercury values to lua, instantiate
	% them in Lua variables, and then return a refrence to the new Lua 
	% variable back to Mercury.
	%
	% The Lua state may be omitted to deconstruct a lua variable
	%
	% Example construction:  	L ^ int(3) = Var 
	% Example deconstruction: 	Var = int(N), N = 3
	% Alternate deconstruction: 	to_int(Var) = 3
	%
	% The det_to_Value functions are identical to the to_Value functions,
	% Only they will will abort instead of failing. 

	% Lua type conversions *

	% The nil Lua type indicates abscence of value. This stands in contrast
	% to Lisp where 'nil' refers to the empty list ('[]' in Mercury).
	% Mercury has little use for a 'nil' or 'null' type (outside of compiler
	% invocation) due to the fact that Mercury can fail and backtrack if
	% the abscence of value is encountered. The following predicates allows
	% Mercury to test Lua variables against, and create Lua variables with
	% nil values.
	
	% int

:- pred int(int, var, lua_state).
:- mode int(in, out, in) is det.
:- mode int(out, in, out) is semidet.

:- func int(int, lua_state) = var.
:- mode int(in, in) = out is det.
:- mode int(out, out) = in is semidet.

:- pred to_int(var::in, int::out) is semidet.
:- func to_int(var) = int is semidet.

:- pred det_to_int(var::in, int::out) is det.
:- func det_to_int(var) = int is det.

	% float

:- pred float(float, var, lua_state).
:- mode float(in, out, in) is det.
:- mode float(out, in, out) is semidet.

:- func float(float, lua_state) = var.
:- mode float(in, in) = out is det.
:- mode float(out, out) = in is semidet.

:- pred to_float(var::in, float::out) is semidet.
:- func to_float(var) = float is semidet.

:- pred det_to_float(var::in, float::out) is det.
:- func det_to_float(var) = float is det.

	% bool

:- pred bool(bool, var, lua_state).
:- mode bool(in, out, in) is det.
:- mode bool(out, in, out) is semidet.

:- func bool(bool, lua_state) = var.
:- mode bool(in, in) = out is det.
:- mode bool(out, out) = in is semidet.

:- pred to_bool(var::in, bool::out) is semidet.
:- func to_bool(var) = bool is semidet.

:- pred det_to_bool(var::in, bool::out) is det.
:- func det_to_bool(var) = bool is det.

	% string

:- pred string(string, var, lua_state).
:- mode string(in, out, in) is det.
:- mode string(out, in, out) is semidet.

:- func string(string, lua_state) = var.
:- mode string(in, in) = out is det.
:- mode string(out, out) = in is semidet.

:- pred to_string(var::in, string::out) is semidet.
:- func to_string(var) = string is semidet.

:- pred det_to_string(var::in, string::out) is det.
:- func det_to_string(var) = string is det.

	% c_pointer

:- pred c_pointer(c_pointer, var, lua_state).
:- mode c_pointer(in, out, in) is det.
:- mode c_pointer(out, in, out) is semidet.

:- func c_pointer(c_pointer, lua_state) = var.
:- mode c_pointer(in, in) = out is det.
:- mode c_pointer(out, out) = in is semidet.

:- pred to_pointer(var::in, c_pointer::out) is semidet.
:- func to_pointer(var) = c_pointer is semidet.

:- pred det_to_pointer(var::in, c_pointer::out) is det.
:- func det_to_pointer(var) = c_pointer is det.

	% univ

:- pred univ(univ, var, lua_state).
:- mode univ(in, out, in) is det.
:- mode univ(out, in, out) is semidet.

:- func univ(univ, lua_state) = var.
:- mode univ(in, in) = out is det.
:- mode univ(out, out) = in is semidet.

:- pred to_univ(var::in, univ::out) is semidet.
:- func to_univ(var) = univ is semidet.

:- pred det_to_univ(var::in, univ::out) is det.
:- func det_to_univ(var) = univ is det.

	% A polymorphic alternative to the above predicates, T will be tested
	% against each of the above mentioned mercury types. 
	%
:- pred var(T, var, lua_state).
:- mode var(in, out, in) is det.
:- mode var(out, in, out) is semidet.

:- func var(T, lua_state) = var.
:- mode var(in, in) = out is det.
%:- mode var(out, out) = in is semidet.

:- pred from_var(var::in, T::out) is semidet.
:- func from_var(var) = T is semidet.

:- pred det_from_var(var::in, T::out) is det.
:- func det_from_var(var) = T is det.



:- some [T] pred some_value(var::in, T::out, lua_state::out) is semidet.

some_value(V, T, L) :-
	L = V ^ state,
	(I = to_int(V) ->
		T = I
	; F = to_float(V) ->
		T = F
	; B = to_bool(V) ->
		T = B
	; S = to_string(V) ->
		T = S
	; P = to_pointer(V) ->
		T = P
	; 
		T = to_univ(V)
	).


	% Type to be thrown if Lua throws an error while interacting with
	% Mercury.
	%
:- type lua_error ---> error(message::string, code::lua_error_code).

	% 'runtime' represents a standard runtime error.
	% 'memory' represents a memory allocation error.
	% 'error' represents an error called while trying to handle an
	%	error.
	% 
:- type lua_error_code
	--->	runtime
	;	memory
	;	error.

	% Accepts a string of Lua source code that is dynamically compiled
	% into a Lua Function.  This function may not refrence any upvalues
	% or global variables.
	%
:- pred load_string(lua_state::in, string::in, var::out) is semidet.
:- func load_string(lua_state, string) = var is semidet.

%TODO: Implement a version of load_string that accepts a 
% string_builder or stream

	% Pass a higher order call to Lua that may be called as a function.
	%
:- func export_pred(pred(lua_state, list(var))) = function.
:- mode export_pred(pred(in, out) is det) = (out) is det.
:- mode export_pred(pred(in, out) is semidet) = (out) is det.
% :- mode export_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_pred(pred(in, out) is nondet) = (out) is det.

	% The same as above, but with 
:- func export_io_pred(pred(lua_state, list(var), io, io)) = function.
:- mode export_io_pred(pred(in, out, di, uo) is det) = (out) is det.
:- mode export_io_pred(pred(in, out, di, uo) is semidet) = (out) is det.
% :- mode export_io_pred(pred(in, out) is multi) = (out) is det.
% :- mode export_io_pred(pred(in, out) is nondet) = (out) is det.

	% For collecting arguments passed from Lua to Mercury in a 
	% function call.
	%
:- type args.

:- pred args(lua_state::in, args::out) is semidet.
:- func args(lua_state) = args is semidet.

:- pred arg(args::in, args::out, T::out) is semidet.
:- func arg(args::in, args::out) = (T::out) is semidet.

	% An alternative method
	%
:- pred list_args(lua_state::in, list(var)::out) is det.
:- func list_args(lua_state) = list(var).


	% Extract values from tables (without calling metamethods), note that
	% the function will return nil if the key does not exist. 
	% If the variable is not a table, the call will always fail.
	%
:- pred get(var, T, var).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is semidet. 
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.

:- func get(var, T) = var.
:- mode get(in, in) = in is semidet.
:- mode get(in, out) = out is semidet.
:- mode get(in, out) = in is nondet.
:- mode get(in, out) = out is nondet.

:- func var ^ T = var.
:- mode in ^ in = in is semidet.
:- mode in ^ in = out is semidet.
:- mode in ^ out = in is nondet.
:- mode in ^ out = out is nondet.




:- pred userdata(lua_state::in, T::in, var::out) is det.
:- func userdata(lua_state, T) = var is det.

:- some [T] pred userdata(T::out, var::in) is semidet.
:- some [T] func userdata(T::out) = (var::in) is semidet.

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





