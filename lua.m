%-----------------------------------------------------------------------------%
% vim: ft=mercury
%-----------------------------------------------------------------------------%
% Copyright (C) 2014 Charlie H. McGee IV.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: lua.m.
% Main author: C4Cypher.
% Stability: low.
% 
% This file presents an interface for handling and passing values between
% compiled Mercury modules and the Lua runtime VM.
%
% Lua is known for being a lightweight, easy to write, (comparatively) fast 
% dynamically typed scripting language.  With first-class functions, lexical 
% closures, varadic argument-passing/variable-assignment, it offers a set of
% language features one might expect of a functional language, rather than an 
% imperative scripting language.  With the usage of metatables and a stack
% based C interface, Lua is extremely exstensible and easily embedded or bound
% with other languages.  This flexibility allows Lua programmers to define
% and use their own semantics, be it functional, object-oriented or otherwise.
%
%
% The Semantic gap.
%
% a Lua program can be considered a set of instructions on what to do to.
% These instructions impose changes oand when.In Lua, statements represent 
% imperative changes to the Lua state by producing side effects. In sequential 
% order, Lua evaluates each statement and modifies the Lua state to reflect the 
% truth-value intended by the statement, within the context of the local scope. 
% As such,  Instead of requiring the declaration and deletion of variables, Lua 
% uses 'nil' to represent unnasigned values. 
% 
% In Lua, 'Foo = 3' can be read as 'Foo is now the number 3'.
%
% In contrast to Lua's imperative semantics, Mercury is a purely declarative
% language.  A Mercury program can be considered a set of predicates that
% describe whether or not things are true. Mercury variables aren't containers
% for values that can change, they represent values that Mercury may not know.
% A 'free' variable is one whose value has not yet been determined.
%
% in Mercury, 'Foo = 3' can be read as 'Foo is 3', a statement that can either
% be true or false.
% 
% These are two very different ways of looking at things, and in order to 
% bridge that gap, this Library defines a means of expressing Lua in a given
% context at a specific moment in time.  That context may be the entire Lua
% state of execution, or it could only be the local scope inside a function
% call.  
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

:- include_module state.

:- import_module io.
:- import_module float.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module univ.
:- import_module require.

%-----------------------------------------------------------------------------%
%
% The Lua State
%

	% A refrence to the Lua VM as defined by the lua_State type in lua.h
	%
:- type lua_state.

:- type lua == lua_state.


%-----------------------------------------------------------------------------%
%
% Lua values
%

	% A union of all of the types that can be natively passed to and from
	% Lua.
	%
:- type value
	--->	nil(nil)	% the abscence of value
	;	number(float)	% double prescision, casts to float
	;	integer(int)	% int cast to Lua number
	;	boolean(bool)	% boolean truth values, casts to bool
	;	string(string)	% string value, casts to string
	;	lightuserdata(c_pointer)	% naked C pointer
	;	userdata(userdata)	% fully allocated userdata
	;	thread(lua_state)	% A co_routine
	;	function(string)	% A function chunk
	;	c_function(c_function)  % A Lua callable function pointer
	;	var(var)		% A refrence to a value in Lua
	;	error(lua_error). 	% Non fatal error
	
:- type values == list(value).

:- type c_function.	% A Lua callable function defined in C



% The nil value
%
% In Lua, nil represents the abscence of value.  Looking up a key in a Lua table 
% that is not assigned a value with produce a nil result.
%
% Furthermore, assigning a key value to nil will unassign that value. Note that 
% this does not neccicarily delete the value, if Lua holds a refrence to that
% value elsewhere, it will not be garbage collected.
%
% In normal Lua semantics, using nil as a key value produces an error, however
% due to the Mercury semantics used in this library, doing so will either fail
% or return another nil value.  This is both for the sake of safer runtime
% integration of Mercury's strict type system with Lua's dynamic type system,
% and also as a practical consideration of Mercury's potentially
% nondeterministic nature, as testing for a paticular type wil result in a
% backtracking failure.
%
% It is to be noted that Lua's nil value is not to be confused with C's NULL
% value.  While used in similar ways, Lua will interpret C's NULL as the number
% zero, wheras C has no direct representation for Lua's nil value.
%
% As a result of this, Lua's semantics on conditional tests are slightly
% different than C's.   C interprets any numeric value other than 0 as true.
% In contrast, Lua interprets ANY value other than boolean false or nil as true.

:- type nil ---> nil.

%-----------------------------------------------------------------------------%
%
% Lua variables
%

% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.

:- type var
	--->	local(index)	% An index on the the local stack
	;	global(string)	% A global variable
	;	registry(string) % A registry entry
	;	ref(ref)	% A strong refrence (like a pointer)
	;	up(upvalue).	% An upvalue pushed onto a C closure


:- type vars == list(var).

:- type index. 		% stack index
:- type upvalue. 	% upvalue index

% The ref type represents a strong refrence to a Lua variable instantiated in
% Lua, as a result, a refrenced variable will not be garbage collected by Lua
% until said refrence is unregistered or re-assigned.
%
% Note that these refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  

	
	% The ref type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type ref.

:- func ref(ref) = T is semidet.

:- func ref_type(ref) = lua_type.


:- implementation.


:- type index ---> index(int).
:- type upvalue ---> upvalue(int).

%-----------------------------------------------------------------------------%
%
% Refrences
%

:- pragma foreign_type("C", ref, "luaMR_Ref", [can_pass_as_mercury_type]).

:- pragma foreign_code("C",
"
typedef int * luaMR_Ref;


/* Creates a new refrence from the stack */
luaMR_Ref luaMR_new_ref(lua_State * L, int index) {
	lua_pushvalue(L, index);
	luaMR_Ref new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, luaMR_finalize_ref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void luaMR_push_ref(lua_State * L, luaMR_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaMR_finalize_ref(luaMR_Ref ref, lua_State * L) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}

"). 

:- interface.

%-----------------------------------------------------------------------------%
%
% Lua scope
%

	% The scope type is meant ensure the safety and purity of Mercury calls
	% to the Lua state, while at the same time, representing and behaving
	% like a concept that should be familiar to Lua programmers.
	%
:- type scope.

	% VarName ^ Scope




	% Retreive the number of arguments passed to a Lua state.
	%
:- func arg_count(lua_state) = int.

	% Declaratively access the arugments passed to a Lua state with dynamic
	% casting.
	%
:- pred args(int, T, lua_state).
:- mode args(in, out, in) is semidet.
:- mode args(out, out, in) is nondet.

	% Declaratively access the arugments passed to a Lua state with static
	% casting. If no arguments are passed, det_args will behave as if a
	% single nil value was passed.
	%
:- some [T] pred det_args(int, T, lua_state).
:- mode det_args(in, out, in) is det.
:- mode det_args(out, out, in) is multi.

	% Dynamic cast a specific argument.
	%
:- func arg(int, lua_state) = T is semidet.

	% Static cast a specific argument.
	%
:- some [T] func det_arg(int, lua_state) = T.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Lua expressions
%

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

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Lua statements
%  

% In Lua, a statement performs an impure change upon the Lua state. Given the
% imperative nature of Lua, a Lua program consists of a series of statements to
% be executed in sequential order.

:- type statement == (impure func(lua_state) = lua_result).

:- type block == list(statement).

:- pred do(block::in, lua::in, lua_result::out, io::di, io::uo) is det.


:- type lua_result
	---> 	ok
	;	error(lua_error)
	;	return(list(expression)).

%-----------------------------------------------------------------------------%
%
% Lua types
%	

:- type lua_type
	--->	none			% rarely used, represents invalid type
	;	nil_type		
	;	number_type		
	;	boolean_type		
	;	string_type		
	;	lightuserdata_type	
	;	function_type		
	;	table_type		
	;	thread_type		
	;	userdata_type.		
	
	% Look up the Lua type of a given variable. 
	% 
:- func lua_type(T) = lua_type.



:- implementation.

%-----------------------------------------------------------------------------%
%
% Lua Types
%
:- pragma foreign_enum("C", lua_type/0, 
[
	none 		- 	"LUA_TNONE",
	nil_type 	- 	"LUA_TNIL",
	boolean_type 	- 	"LUA_TBOOLEAN",
	lightuserdata_type - 	"LUA_TLIGHTUSERDATA",
	number_type 	- 	"LUA_TNUMBER",
	string_type 	- 	"LUA_TSTRING",
	table_type 	- 	"LUA_TTABLE",
	function_type 	- 	"LUA_TFUNCTION",
	userdata_type 	- 	"LUA_TUSERDATA",
	thread_type 	- 	"LUA_TTHREAD"
]).

:- interface.



%-----------------------------------------------------------------------------%
%
% Lua errors
%
	% Thrown when Lua experiences an error.
	%
:- type lua_error
	---> 	lua_error(error_type, string).

:- type error_type
	--->	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.

:- implementation.

:- pragma foreign_enum("C", error_type,
[
	runtime_error 	-	"LUA_ERRRUN",
	syntax_error 	-	"LUA_ERRSYNTAX",
	memory_error	-	"LUA_ERRMEM",
	unhandled_error	-	"LUA_ERRERR"
]). 

:- interface.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lua.state.

:- import_module type_desc.
:- import_module int.
:- import_module float.
:- import_module bool.
:- import_module string.
:- import_module require.

:- pragma require_feature_set([conservative_gc, double_prec_float]). 

:- pragma foreign_decl("C", 
"
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define MR_LUA_MODULE ""MR_LUA_MODULE""
#define MR_LUA_UDATA ""MR_LUA_UDATA_METATABLE""
#define MR_LUA_READY ""MR_LUA_IS_READY""

#define MR_LUA_TYPE ""__mercury_type""

").

	% Succeed if the given value is NULL.
	%
:- pred null(T::in) is semidet.

:- pragma foreign_proc("C", null(T::in),
	[promise_pure, will_not_call_mercury], 
	"SUCCESS_INDICATOR = (T == NULL);").

%-----------------------------------------------------------------------------%
%
% The Lua state
%



% The lua type represents the state of a running Lua Virtual Machine. (Lua VM
% for short) Note that as a convention borrowed from the C API, procedures 
% that query or manipulate the Lua state will use the variable term 'L' to refer 
% to the Lua state.

:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

	% For the sake of safety and sanity checking, produce a lua_state_ptr
	% instantiated as a NULL pointer.
	%
:- func null_state = lua_state.

:- pragma foreign_proc("C", null_state = (Null::out), 
	[promise_pure, will_not_call_mercury], 
	"Null = NULL;").

% WARNING! Refrences to Lua types (tables, functions, userdata) derived
% from one global lua_state are NOT compatible with other se+perately created
% lua_states. The only exception to this is lua_states created as threads.
% lua_threads may freely pass variables to or from their parent state and
% sibling threads.


%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "extern void luaMR_init(lua_State *);").

:- pragma foreign_code("C", 
"
void luaMR_getregistry(lua_State * L, const char * k) {
	lua_getfield(L, LUA_REGISTRYINDEX, k);
}

void luaMR_setregistry(lua_State * L, const char * k) {
	lua_setfield(L, LUA_REGISTRYINDEX, k);
}

void luaMR_getupvalue(lua_State * L, const int id) {
	lua_pushvalue(L, lua_upvalueindex(id));
}

void luaMR_setupvalue(lua_State * L, const int id) {
	lua_replace(L, lua_upvalueindex(id));
}

void luaMR_init(lua_State * L) {
	
	/* set the given Lua state as the current Lua state */
	
	luaMR_current_lua_state = L;
	
	/* Add tables to the registry. */
	
	lua_newtable(L);
	luaMR_setregistry(L, MR_LUA_MODULE);

	/* TODO: Define and export luaMR_userdata_metatable. */
	luaMR_userdata_metatable(L);
	luaMR_setregistry(L, MR_LUA_UDATA);
	
	

	/* Add loader to package.loaders */
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const lua_Integer length = (lua_Integer)lua_objlen(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaMR_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	luaMR_setregistry(L, MR_LUA_READY);
} 

").

:- pred init(lua_state::in, io::di, io::uo) is det.

:- pragma foreign_proc("C", init(L::in, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "luaMR_init(L);").

:- pragma foreign_decl("C", "int luaMR_ready(lua_State *);").

:- pragma foreign_code("C", 
"
	/* Check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
		lua_checkstack(L, 1);
		luaMR_getregistry(L, MR_LUA_READY);
		int ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}

").

:- pred ready(lua_state::in) is semidet.

:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").

:- pred ready(lua_state::in, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C", ready(L::in, Answer::out, _I::di, _O::uo), 
	[promise_pure, will_not_call_mercury], "
	if(luaMR_ready(L))
		Answer = MR_YES;
	else
		Answer = MR_NO;

").



:- pragma foreign_code("C", 
"

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaMR_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		luaMR_getregistry(L, MR_LUA_MODULE);
		lua_getfield(L, 2, module_name);
		return 1;
	}
	return 0;
}

").








