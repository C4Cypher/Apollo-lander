%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: module.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This module presents types and predicates for constructing Lua loadable 
% modules.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.module.

:- interface.

:- import_module string.

% :- import_module lua. -  Omitted, due to the fact that this module is included in lua.m.

:- type name == string.

:- type lua.module.

:- func init = module.
:- pred init(module::out).


	% Export the provided module to
:- pred export(module::in, name:in, lua::di, lua::uo).

:- pred export(module::in, name::in, lua_state::in, io::di, io::uo).
:- mode export(in, in, in, di, uo) is det.
:- mode export(di, in, in, di, uo) is det.
