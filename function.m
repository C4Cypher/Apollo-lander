%---------------------------------------------------------------------------%
% vim: ft=mercury 
%---------------------------------------------------------------------------%
% Copyright (C) 1993-2000, 2003-2009, 2011-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: lua_h.m.
% Main author: fjh.
% Stability: medium.
% 
% This file defines the construction, passing and calling of Lua functions.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- type ipred == impure pred(lua).
:- inst ipred == impure pred(in).
