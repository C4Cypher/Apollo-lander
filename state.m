%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: state.m.
% Main author: c4cypher.
% Stability: low.
% 
% Utilities for making it easier to make pure calls with the lower level api.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module state.

:- interface.

:- import_module trail.

	% The various types that might be used to backtrack a Lua state
:- type lua_trail
	--->	lua_func(lua_func)
	;	c_function(c_function)
	;	ref(ref)
	;	empty_trail.


	% Abbriviated choicepoint id.
:- type id == choicepoint_id.

	% The current choicepoint.
:- func current_id = id.

	% A null choicepoint.
:- func null_id = id.

	% Fail if the current choicepoint is newer than 
	% the stored choicepoint.
	%
:- semipure pred current(lua_state).
:- mode current(ui) is semidet.
:- mode current(mui) is semidet.



	% Construct or deconstruct a Lua state 
	%
:- func lua_state(lua, id, lua_trail) = lua_state.
:- mode lua_state(in, in, in) = uo is det.
:- mode lua_state(out, out, out) = di is det.
:- mode lua_state(out, out, out) = mdi is det.

	% Unique deconstructor
	%
:- func unique_state(lua, id, lua_trail) = lua_state.
:- mode unique_state(out, out, out) = ui is det.
:- mode unique_state(out, out, out) = mui is det.

% Access the members of a Lua state while preserving it's uniqueness.

:- func lua(lua_state) = lua.
:- mode lua(ui) = uo is det.
:- mode lua(mui) = uo is det.

:- func id(lua_state) = id.
:- mode id(ui) = uo is det.
:- mode id(mui) = uo is det.

:- func trail(lua_state) = lua_trail.
:- mode trail(ui) = out is det.
:- mode trail(mui) = out is det.


	% Register a new trail function, it will be called before the existing
	% trail_func is called.
:- pred update_lua_trail(lua_func, ls, ls).
:- mode update_lua_trail(in, mdi, muo) is det.

	% Register the trail_func of a lua_state on the trail, update the
	% choicepoint ID, and reset the trail func.
:- impure pred trail_lua_closure(lua_func::in, ls::mdi, ls::muo) is det.

	% Provide a guard closure to determine whether or not to backtrack.
	%
:- impure pred trail_lua_closure(impure pred(untrail_reason), lua_func, ls, ls).
:- mode trail_lua_closure(pred(in) is semidet, in, mdi, muo) is det.

	% If the current id is newer, trail as normally, however, if it isn't
	% Just update the trail_func.
	%
:- impure pred trail_if_newer(lua_func::in, ls::mdi, ls::muo) is det.


	% Predicates that can be used to register a trail_func with the trail.
	% The latter form will only backtrack on undo, exception or retry.
	%
:- impure pred backtrack(lua_trail::in, lua::in) is det.
:- impure pred backtrack(untrail_reason::in, 
	lua_trail::in, lua_state::in) is det.
	


%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

current_id = I :- impure I = current_choicepoint_id.
null_id = null_choicepoint_id.

current(lua_state(_, Id, _)) :- choicepoint_newer(current_id, Id).

:- pragma foreign_decl("C", "
	typedef struct luaMR_Struct_lua_state {
		lua_State ** lua;
		MR_ChoicePointID id;
		MR_Word * trail;
	} luaMR_lua_state;
").


:- pragma foreign_proc("C", lua_state(L::in, I::in, T::in) = (S::uo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state newstate;
	newstate->lua = L;
	newstate.id = I;
	newstate->trail = T;
	S = newstate;
").

:- pragma foreign_proc("C", lua_state(L::in, I::in, T::in) = (S::muo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state newstate;
	newstate->lua = L;
	newstate.id = I;
	newstate->trail = T;
	S = newstate;
").

:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out) = (S::di),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S.id;
	T = S->trail;
").

:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out) = (S::mdi),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S.id;
	T = S->trail;
").
	
:- pragma foreign_proc("C", unique_state(L::out, I::out, T::out) = (S::ui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S.id;
	T = S->trail;
").

:- pragma foreign_proc("C", unique_state(L::out, I::out, T::out) = (S::ui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S.id;
	T = S->trail;
").


lua(unique_state(L, _, _)) = L.
id(unique_state(_, I, _)) = I.
trail(unique_state(_, _, T)) = T.


update_lua_trail(T, lua_state(L, C, T0), lua_state(L, C, lua_func(F))) :-
	require_complete_switch [T] some [F0]
	( T = lua_func(F0)
	; T = empty_trail, 
		unexpected($module, $pred,
		"cannot update the Lua trail with an empty_trail") 
	; some [R] 
		( T = c_function(C), 
			impure lua_pushcfunction(L, C),
			semipure R = lua_toref(L, index(-1)),
			impure lua_pop(L, 1)
		; T = ref(R)
		) -> F0 = ( impure func(L::in) = (Ret - 1::out) is det :-
			impure lua_pushref(L, R),
			semipure Err_index = lua_gettop(L),
			impure Ret = lua_pcall(L, index(-1)),
			impure lua_remove(L, index(Err_index))
		)
		; unexpected($module, $pred, 
			"Unknown lua_trail value provided.")
	),			 
	F = lua_func( 
		( impure func(L::in) = ((0)::out) is det :-
			impure backtrack(F, L),
			impure backtrack(F0, L)
		)
	).
	

trail_lua_closure(F0, LS, lua_state(L, current_id, empty_trail)) :-
	update_trail_func(F0, LS, lua_state(L, _, F)),
	Closure = ( impure (pred) is det :- 
		impure backtrack(F, L)
	),
	trail_closure_on_backtrack(Closure).
		
trail_lua_closure(P, F0, LS, lua_state(L, current_id, empty_trail)) :-
	update_trail_func(F0, LS, lua_state(L, _, F)),
	Closure = ( impure pred(U) is det :-
		impure impure_call(P, U), 
		impure backtrack(F, L)
	),
	trail_closure_on_backtrack(Closure).
	

trail_if_newer(F, !LS) :-
	current(!.LS) -> 
		update_trail_func(F, !LS)
	;
		trail_lua_closure(F, !LS).
		



	
		
	
	
	

backtrack(F, L) :- impure impure_apply(F, L) = _ -> true
	; impure lua_error(L).
	
backtrack(U, F, L) :-
	( U = untrail_undo ; U = untrail_exception ; U = untrail_retry) ->
		impure backtrack(F, L)
	; U = untrail_gc ->
		impure push_value(L, lua_error(unhandled_error, 
		"Encountered MR_gc while backtracking.")),
		impure lua_error(L)
	; true.
		
	
