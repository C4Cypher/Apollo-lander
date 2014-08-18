%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: scope.m.
% Main author: c4cypher.
% Stability: low.
% 
% Describe the module.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module scope.

:- interface.

:- type scope(L)
	---> 	scope(
			parent::L, % Parent scope
			size::int,% number of values allocated for this scope 
			local::map(var, value), % local variable associations 
			top::index
		).

% top/2 carries the index that triggers automatic use of lua_checkstack for
% additional values on the stack. 
	
%:- instance lua_state(scope(L)) <= lua_state(L).	
%:- instance lua(scope(L)) <= lua_state(L).
