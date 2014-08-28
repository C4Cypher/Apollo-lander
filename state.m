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



% For use in pred fields where it can't be reordered.
:- func current_id = choicepoint_id.


	% Cast between lua and lua_state.
	%
:- func lua(lua) = lua_state.
:- mode lua(in) = uo is det.
:- mode lua(di) = uo is det.
:- mode lua(mdi) = uo is det.
:- mode lua(uo) = in is det.
:- mode lua(uo) = ui is det.
:- mode lua(uo) = mui is det.

	% Retreive the choicepoint assigned to the lua_state. 
%:- func id(lua_state) = choicepoint_id.



:- implementation.

current_id = C :- impure C = current_choicepoint_id.
:- pragma promise_pure(current_id/0).  

lua(L) = { L, _ }.



