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


	% Abbriviated choicepoint id.
:- type id == choicepoint_id.

	% The current choicepoint.
:- func current_id = id.

	% A null choicepoint.
:- func null_id = id.

	% Fail if the current choicepoint is newer than 
	% the stored choicepoint.
	%
:- semipure func current(lua_state).
:- mode current(ui) is semidet.
:- mode current(mui) is semidet.


	% Construct or deconstruct a Lua state while preserving 
	% it's uniqueness.
	%
:- func lua_state(lua, id, lua_func) = lua_state.
:- mode lua_state(in, in, in) = uo is det.
:- mode lua_state(in, in, in) = muo is det.
:- mode lua_state(out, out, out) = ui is det.
:- mode lua_state(out, out, out) = mui is det.

% Access the members of a Lua state while preserving it's uniqueness.

:- func lua(lua_state) = lua.
:- mode lua(ui) = uo is det.
:- mode lua(mui) = uo is det.

:- func id(lua_state) = id.
:- mode id(ui) = uo.
:- mode id(mui) = uo.

:- func trail_func(lua_state) = lua_func.
:- mode trail_func(ui) = out(lua_trail).
:- mode trail_func(mui) = out(lua_trail).



	% Register a new trail function, it will be called before the existing
	% trail_func is called.
:- pred update_trail_func(lua_func, ls, ls).
:- mode update_trail_func(in, mdi, muo) is det.

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
:- impure pred trail_if_newer(impure pred(untrail_reason::in) is semidet, 
	lua_func::in, ls::mdi, ls::muo) is det.

	% Predicates that can be used to register a trail_func with the trail.
	% The latter form will only backtrack on undo, exception or retry.
	% Will throw an exception if the trail_func fails, using the value
	% on the top of the stack.
	%
:- impure pred backtrack(trail_func::in, lua::in) is det.
:- impure pred backtrack(untrail_reason::in, 
	trail_func::in, lua_state::in) is det.
	


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

lua_state(L, I, T) = S :- S0 = lua_state2(L, I, T)


:- pragma foreign_proc("C", lua_state(L::in, Id::in, 
	Trail::in) = (State::uo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state newstate;
	newstate->lua = L;
	newstate.id = Id;
	newstate->trail = Trail;
	*State = newstate;
").

:- pragma foreign_proc("C", lua_state(L::in, Id::in, 
	Trail::in) = (State::muo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state newstate;
	newstate->lua = L;
	newstate.id = Id;
	newstate->trail = Trail;
	*State = newstate;
").

:- pragma foreign_proc("C", lua_state(L::out, Id::out, 
	Trail::out) = (State::ui),
	[will_not_call_mercury, promise_pure], "
	
	L = State->lua;
	ID = State.id;
	Trail State->trail;
").

:- pragma foreign_proc("C", lua_state(L::out, Id::out, 
	Trail::out) = (State::mui),
	[will_not_call_mercury, promise_pure], "
	
	L = State->lua;
	ID = State.id;
	Trail State->trail;
").
	
	
lua(lua_state(L, _, _)) = L.
id(lua_state(_, I, _)) = I.
trail(lua_state(_, _, T)) = T.

update_trail_func(F, lua_state(L, C, F0), lua_state(L, C, 
	( impure func(L::in) = ((0)::out) is det :-
		impure backtrack(F, L),
		impure backtrack(F0, L)
	)
).

trail_lua_closure(F0, LS, lua_state(L, current_id, empty_trail) :-
	update_trail_func(F0, LS, lua_state(L, _, F)),
	trail_closure_on_backtrack(impure pred is det :- 
		impure backtrack(F, L))).
		
trail_lua_closre(P, F0, LS, lua_state(L, current_id, empty_trail) :-
	update_trail_func(F0, LS, lua_state(L, _, F)),
	trail_closure_on_backtrack(impure pred is det :- 
		impure backtrack(F, L))).
	




	current(!.LS) -> 
		update_trail_func(F0, !LS)
	;
		
	
	
	

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
		
	
