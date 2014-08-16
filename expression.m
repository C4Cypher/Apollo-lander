%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: expression.m.
% Main author: c4cypher.
% Stability: low.
% 
% Describe the module.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module expression.

:- interface.


% In Lua, an expression is a part of Lua's syntax that is parsed via strict 
% evaluation. It can be assigned to a variable, passed as function arguments,
% return values, in table constructors and indexes, or just about anywhere a
% variable term could be placed.
%
% Unlike Lua function calls, expressions are functionally pure (so long as they
% do not invoke metamethods).  In order to treat the Lua state in purely
% declarative terms, we need to redefine some of the semantics and enforce
% some rules when it comes to interacting with the stack.
% 
% When Lua calls a function implemented with foreign code, the arguments
% passed to the function are pushed onto the stack.  Changing those values on
% the stack would modify the original context of the call. As a result,
% these values should be treated as if they are immutable.
%
% Within the context of this library, while the lua_state type is literally
% bound to a C pointer refrencing a lua_State struct, conceptually Lua stack is 
% never considered to be fully ground.  Any values pushed onto the stack past
% the initial arguments are considered part of the local scope, and as such, they
% may be added and removed from the stack in a manner that is consistent with
% variable scope and backtracking.

%%% Important! %%%
% an expression that removes or modifies values on the stack that it did not 
% add should be expected to produce undefined behavior as this is considered 
% impure behavior in the context of Mercury's pure functional semantics.

	% func(First, Last, Raw, Strict, L) = Index.
	%
	% An expression performs an evaluation based upon the values availible
	% to a Lua state, returning the stack index containing the value of the
	% evaluated expression.
	%
	% First is the first stack index considered to be a part of the local
	% scope. Values at or past this index will be removed when the scope
	% closes.
	%
	% Last is the last stack index Mercury will be free to use without
	% first checking to see if Lua has allocated space for it.
	%
	% Raw determines whether or not the expression is being evaluated
	% under a 'raw' context, if yes, it will avoid calling metamethods.
	% 
	% Strict determines whether or not the expression should return nil
	% when faced with invalid Lua syntax, or throw an exception if Strict
	% is yes.
	%
	% Index is the stack index of the evaluated expression.
	% 
:- type expression == (func(scope) = value).

:- type expr == expression.


% Note: calls to eval should never invoke metatables, as they should be pure.
% If metatable invocation is desired, do so within a statement.

% eval and det_eval return nil on an invalid expression, wheras strict_eval will
% throw an exception where Lua normally would.

	% Evaluate an expression with dynamic type cast.
	%
:- func eval(expression, lua_state) = T is semidet.

	% Throws a lua_error if an invalid expression is evaluated.
	%
:- func strict_eval(expression, lua_state) = T is semidet.

	% Evaluate without type cast
	%
:- some [T] func det_eval(expression, lua) = T.

	% Value to be passed if an expression is invalid
	%
:- type invalid_expression ---> invalid_expression(string).

%-----------------------------------------------------------------------------%
%
% expression operators
%  

:- type unop == func(expr) = expr.
:- type binop == func(expr, expr) = expr.

	% Access values passed as function arguments.
	%
:- func arg(int) = expr.

	% Access a function upvalue.
	%
:- func upvalue(int) = expr.

:- 

% Math operators.
:- func expr + expr = expr.
:- func expr - expr = expr.
:- func expr * expr = expr.
:- func expr / expr = expr.
:- func expr mod expr = expr.
:- func pow(expr, expr) = expr.
:- func - expr = expr.

% comparison operators.
:- func expr == expr = expr.
:- func expr ~= expr = expr.
:- func expr < expr = expr.
:- func expr =< expr = expr.
:- func expr > expr = expr. 	% := not expr =< expr.
:- func expr >= expr = expr. 	% := not expr < expr.

% Note: In lua syntax >= and =< are expressed as >= and <=

% logical operators

% Logical evaluations in Lua have a slightly different meaning than the commonly
% accepted interpretation in strictly typed languages, due to it's dynamically
% typed nature.  Any value can be passed as a logical comparison, and any value
% other than nil or false will evaluate to true.  Further more, the and/or
% operators will evaluate to one the value of one of their operands, not a 
% boolean value, allowing for some syntactic sugar tricks reminicent of using
% Mercury's conditional (if/then or->/;) operators to evaluate expressions.
%
% Most notable are the and/or operators. 'and' will return the second operand
% if the evaluation succeeds, and the first operand if it fails.  Conversely,
% the 'or' operator will return the first operand if the evaluation succeeds,
% or the second operand if it fails.

:- func not expr = expr. 	% Always evalutates to boolean true or false
:- func expr and expr = expr.	% A and B = C :- C = if (A , B) then B else A.  
:- func expr or expr = expr.	% A or B = C :- C = if A then A else B.  

	% The length operator returns the number of chars in a string, the 
	% number of values in the array portion of a table (from index 1 up 
	% until the first nil value), the size allocated for a userdata in 
	% bytes, or 0 for any other value.
	%
:- func len(expr) = expr.

:- func length(expr) = expr.	% length(Expr) = len(Expr).

	% Casts a value to a string.
	%
:- func to_string(expr) = expr.

	% Concatenates string values. This implicitly uses to_string on 
	% non-string values.
	%
:- func expr .. expr = expr.

	% Table lookup
	% Mercury: 	expr ^ index(expr) = expr.
	% Lua:		expr[expr] = expr
	%
:- func index(expr, expr) = expr.

	% function constructor.
	%
	% Functions constructed this way 
	%
:- func function(block) = expr.


	
%-----------------------------------------------------------------------------%
%
% Lua Value expressions.
%

	
% In Lua, variables are not typed, values are.  Lua recognizes eight types.

:- func nil = expr.		% the abscence of value
	
% numeric values
:- func number(float) = expr.		% double prescision, casts to float
:- func integer(int) = expr.		% int cast to Lua number

% boolean truth values, casts to bool
:- func	boolean(bool) = expr.
:- func	true = expr.
:- func false = expr.	

:- func string(string) = expr.		% string value, casts to string
:- func	lightuserdata(c_pointer)	% naked C pointer
	
	% Refrence types
	;	function(function)	% A Lua function
	;	table(table)		% A Lua table
	;	thread(lua_state)	% A Lua coroutine
	;	userdata(userdata).	% Full userdata 






%-----------------------------------------------------------------------------%
%
% variadic expressions
%  

% Certain operations in Lua may utilize variadic expressions.
% The syntax for such expressions use the ',' infix operator.
%
% Such cases include variable assignments:
%
% local x, y, z = 1, 2 3
%
% function calls:
%
% local x, y, z = some_function(1, 2, 3)
% 
% and table constructors, assigning values to the array portion of a table,  
% indexed by number, starting with 1:
%
% local t = { a, b, c } := local t = { [1] = a, [2] = b, [3] = c }
%
% Function calls can be used to populate the array portion of a table in the
% same manner:
%
% local t = { some_function(1, 2, 3) }
%
% Any unneeded values in a variadic expression are ignored, and any missing
% values in a variadic expression are populated by nil:
%
% local a, b = 1, 2, 3 --The third value in the right hand side is ignored
%
% local a, b, c = 1 := a, b, c = 1, nil, nil

	% variadic expressions encompass sequential sets of values
	% Values are indexed starting at one, incrementing until
	% the call fails.  If an invalid index is given, nil is returned.
	%
:- type variadic_expression == (func(int) `with_type` expression).

:- inst variadic_expression
	--->	(func(in, in, in, in, in) = out is det) 
	;	(func(out, in, in, in, in) = out is multi).
	
:- type expr_list == variadic_expression.
:- inst expr_list == variadic_expression.

% The semantics of eval/3 and det_eval/3 are the same as those for
% eval/2 and det_eval/2.

	% Evaluate variadic expressions dynamically
	%
:- func eval(int, expr_list, lua) = T.
:- mode eval(in, in, in) = out is semidet.
:- mode eval(out, in, in) = out is nondet.

	% Static evaluation of variadic expressions.
	%
:- some [T] func det_eval(int, expr_list, lua) = T.
:- mode det_eval(in, in, in) = out is det.
:- mode det_eval(out, in, in) = out is multi.


