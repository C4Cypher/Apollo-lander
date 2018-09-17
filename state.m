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
	--->	mr_func(mr_func)
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
:- pred current(lua_state).
:- mode current(mui) is semidet.

:- pred current(lua_state::mdi, lua_state::muo) is semidet.



	% Construct or deconstruct a Lua state 
	%
:- func lua_state(lua, id, lua_trail) = lua_state.
:- mode lua_state(in, in, in) = uo is det.
:- mode lua_state(out, out, out) = di is det.
:- mode lua_state(out, out, out) = mdi is det.

	% Construct or deconstruct with io state
:- func lua_state(lua, id, lua_trail, io) = lua_state.
:- mode lua_state(in, in, in, di) = uo is det.
:- mode lua_state(out, out, out, uo) = di is det.
:- mode lua_state(out, out, out, uo) = mdi is det.

	% Unique deconstructor
	%
:- func unique_state(lua, id, lua_trail) = lua_state.
:- mode unique_state(out, out, out) = ui is det.
:- mode unique_state(out, out, out) = mui is det.

	% Abriviated forms
:- func ls(lua, id, lua_trail) = ls.
:- mode ls(in, in, in) = uo is det.
:- mode ls(out, out, out) = di is det.
:- mode ls(out, out, out) = mdi is det.

:- func ls(lua, id, lua_trail, io) = ls.
:- mode ls(in, in, in, di) = uo is det.
:- mode ls(out, out, out, uo) = di is det.
:- mode ls(out, out, out, uo) = mdi is det.

:- func us(lua, id, lua_trail) = lua_state.
:- mode us(out, out, out) = ui is det.
:- mode us(out, out, out) = mui is det.

% Access the members of a Lua state while preserving it's uniqueness.

:- func lua(lua_state) = lua.
:- mode lua(ui) = out is det.
:- mode lua(mui) = out is det.

:- func id(lua_state) = id.
:- mode id(ui) = out is det.
:- mode id(mui) = out is det.

:- func trail(lua_state) = lua_trail.
:- mode trail(ui) = out is det.
:- mode trail(mui) = out is det.



	% Register a new trail function, it will be called before the existing
	% trail_func is called.
:- pred update_lua_trail(mr_func, ls, ls).
:- mode update_lua_trail(in, di, muo) is det.
:- mode update_lua_trail(in, mdi, muo) is det.

	% Register the trail_func of a lua_state on the trail, update the
	% choicepoint ID, and reset the trail func.
:- impure pred trail_lua_closure(mr_func, ls, ls).
:- mode trail_lua_closure(mri, di, muo) is det.
:- mode trail_lua_closure(mri, mdi, muo) is det.


	% If the current id is newer, trail as normally, however, if it isn't
	% Just update the trail_func.
	%
:- impure pred trail_if_newer(mr_func, ls, ls).
:- mode trail_if_newer(mri, di, muo) is det.
:- mode trail_if_newer(mri, mdi, muo) is det.


	% Predicates that can be used to register a trail_func with the trail.
	% The latter form will only backtrack on undo, exception or retry.
	%
:- impure pred backtrack(mr_func, lua).
:- mode backtrack(mri, in) is det.

:- impure func backtrack(mr_func, mr_func, lua) = int.
:- mode backtrack(mri, mri, in) = out is det.

:- impure pred backtrack(mr_func, mr_func, lua, int).
:- mode backtrack(mri, mri, in, out) is det.

:- func get_backtrack(mr_func, lua) = (impure (pred)).
:- mode get_backtrack(mri, in) = out((pred) is det) is det.

:- func trail_to_pred(lua_trail, lua) = mr_func.
:- mode trail_to_pred(in, in) = mro is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").


current_id = I :- impure I = current_choicepoint_id.

:- pragma promise_pure(current_id/0).

null_id = null_choicepoint_id.

current(L) :- choicepoint_newer(L^id, current_id).

current(ls(L, I, T), ls(L, I, T)) :- choicepoint_newer(I, current_id).

:- pragma foreign_decl("C", "
	typedef struct luaMR_lua_state {
		lua_State * lua;
		MR_ChoicepointId id;
		MR_Word trail;
	} luaMR_lua_state;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua_state(L::in, I::in, T::in) = (S::uo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
").


:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out) = (S::di),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out) = (S::mdi),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", lua_state(L::in, I::in, T::in, _IO::di) = (S::uo),
	[will_not_call_mercury, promise_pure], "
	
	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
").


:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out, _IO::uo) = (S::di),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", lua_state(L::out, I::out, T::out, _IO::uo) = (S::mdi),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

%-----------------------------------------------------------------------------%
	
:- pragma foreign_proc("C", unique_state(L::out, I::out, T::out) = (S::ui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", unique_state(L::out, I::out, T::out) = (S::mui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", ls(L::in, I::in, T::in) = (S::uo),
	[will_not_call_mercury, promise_pure], "


	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
").


:- pragma foreign_proc("C", ls(L::out, I::out, T::out) = (S::di),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", ls(L::out, I::out, T::out) = (S::mdi),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", ls(L::in, I::in, T::in, _IO::di) = (S::uo),
	[will_not_call_mercury, promise_pure], "


	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
").


:- pragma foreign_proc("C", ls(L::out, I::out, T::out, _IO::uo) = (S::di),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", ls(L::out, I::out, T::out, _IO::uo) = (S::mdi),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

%-----------------------------------------------------------------------------%
	
:- pragma foreign_proc("C", us(L::out, I::out, T::out) = (S::ui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

:- pragma foreign_proc("C", us(L::out, I::out, T::out) = (S::mui),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
	I = S->id;
	T = S->trail;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C", lua(S::ui) = (L::out),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
").

:- pragma foreign_proc("C", lua(S::mui) = (L::out),
	[will_not_call_mercury, promise_pure], "
	
	L = S->lua;
").

:- pragma foreign_proc("C", id(S::ui) = (I::out),
	[will_not_call_mercury, promise_pure], "
	
	I = S->id;
").

:- pragma foreign_proc("C", id(S::mui) = (I::out),
	[will_not_call_mercury, promise_pure], "
	
	I = S->id;
").


:- pragma foreign_proc("C", trail(S::ui) = (T::out),
	[will_not_call_mercury, promise_pure], "
	
	T = S->trail;
").

:- pragma foreign_proc("C", trail(S::mui) = (T::out),
	[will_not_call_mercury, promise_pure], "
	
	T = S->trail;
").


%-----------------------------------------------------------------------------%

update_lua_trail(F0, ls(L, I, T0), LS) :-
	F1 = trail_to_pred(T0, L),
	F = backtrack(F0, F1),
	T = mr_func(F),
	LS = ls(L, I, T).


trail_to_pred(T, L) = P :-
	require_complete_switch [T] some [R]
	( T = mr_func(F0) -> P = 
		( impure pred(L1::in, Ret::out) is det :- 
			impure F0(L1, Ret) 
		)
	; T = empty_trail -> 
		P = ( impure pred(_::in, 0::out) is det :- true )
	; T = c_function(C) -> 
		impure lua_pushcfunction(C, L),
		semipure R = lua_toref(index(-1), L),
		P = ref_to_func(R),
		impure lua_pop(1, L)
	; T = ref(R) ->
		P = ref_to_func(R)
	; unexpected($module, $pred, 
			"Unknown lua_trail value provided.")
	).
	

:- pragma promise_pure(trail_to_pred/2).

	
:- func ref_to_func(ref) = mr_func.

ref_to_func(R) = ( impure pred(L::in, Ret::out) is det :-
			impure lua_pushref(R, L),
			semipure Err_index = lua_gettop(L),
			impure RV = lua_pcall(index(-1), L),
			( returned(Ret0) = RV ->
				Ret = Ret0 - 1,
				impure lua_remove(index(Err_index), L)
			; RV = returned_error(Err) ->
				impure lua_error(Err, L)
			; unexpected($module, $pred, "Invalid return value. (WTF?)")
			)
		).	

%-----------------------------------------------------------------------------%

trail_lua_closure(F0, LS, lua_state(L, current_id, empty_trail)) :-
	update_lua_trail(F0, LS, lua_state(L, _, T)),
	( T = mr_func(F), P = get_backtrack(F, L) ->
		impure trail_closure_on_backtrack(P)
	; unexpected($module, $pred, 
	"Previous call to update_lua_trail did not convert trail to a func.")
	).
%-----------------------------------------------------------------------------%

trail_if_newer(F, !L) :-
	current(!L) -> 
		update_lua_trail(F, !L)
	;
		impure trail_lua_closure(F, !L).

%-----------------------------------------------------------------------------%		

backtrack(F, L) :- impure F(L,_).

backtrack(F0, F1, L) = 0 :-
	impure backtrack(F0, L),	 
	impure backtrack(F1, L).
	
backtrack(F0, F1, L, 0) :-
	impure backtrack(F0, L),	 
	impure backtrack(F1, L).

get_backtrack(F, L) = (impure (pred) is det :- impure backtrack(F, L)).


