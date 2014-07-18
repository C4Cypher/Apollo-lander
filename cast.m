%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: cast.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This library defines a typeclass which defines a set of types which may be
% implicity upcast to a union type.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%





:- typeclass cast(T, U)
