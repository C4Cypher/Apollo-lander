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

:- include_module lua.state.

:- import_module io.
:- import_module univ.


%-----------------------------------------------------------------------------%
%
% The Lua Global environment.
%
	
	
% These calls are relevant inside the context of a Mercury predicate being
% called as a Lua function.


	% If the call in question passes io.state, access and modification
	% of global variables is permitted, as is the calling of other
	% Lua functions.
	
:- pred get_global(string::in, univ::out, io::di, io::uo) is det.
:- pred set_global(string::in, T::in, io::di, io::uo) is det.

:- pred call_function(function::in, vars::in, vars::out, io::di, io::uo) 
	is det.

	% Thrown when Lua experiences an error.
	%
:- type lua_error(error_type, string).

:- type error_type
	--->	runtime_error
	;	syntax_error
	;	memory_error
	;	unhandled_error.




%-----------------------------------------------------------------------------%
%
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

	% Utility pred for evaluating whether or not an existential value is nil.
	%
:- pred is_nil(var::in, lua::in) is semidet.

%-----------------------------------------------------------------------------%
%
% Lua Variables.
%

	% This type represents the Mercury equivalent to Lua's variable list
	% espression, used to pass varadic function arguments and return
	% values.
	
:- type vars.
	
:- implementation. % TODO: Merge with main implementation

:- type vars
	--->	var(univ)
	;	cons(univ, vars))
	where equality is unify_vars.	
	


	% Due to nil's special status as the abscence of value, a single
	% nil value is passed in place of the empty list and will unify
	% with lists of nil.
	%
:- pred unify_vars(vars::in, vars::in) is det.

unify_vars(var(U), var(U)).
 
unify_vars(cons(U, Us1), cons(U, Us2) :-
	unify_vars(Us1, Us2).
	
unify_vars(var(U), cons(U, Us)) :- 
	nil_vars(Us).
	
unify_vars(cons(U, Us)), var(U)) :- 
	nil_vars(Us).

	% A given set of vars is composed of nothing but nil values.
	%
:- pred nil_vars(vars::in) is semidet.

nil_vars(var(univ(nil))).
nil_vars(cons(univ(nil), Us)) :- nil_vars(Us).

:- interface.

:- func vars(T) = vars.
:- mode vars(in) = out is det.
:- mode vars(out) = in is semidet.

:- func T1 , T2 = vars.
:- mode in, in = out is det.
:- mode out, out = in is semidet.

:- func univ_list(list(univ)) = vars.
:- mode univ_list(in) = out is det.
:- mode uinv_list(out) = in is det.

:- implementation. % TODO: Merge with main implementation

vars(T) = Vars :- 
	( T:vars -> 
		Vars = T 
	; 
		Vars = var(univ(T))
	).
	
T1 , T2 = cons(vars(T1), vars(T2)).


univ_list([]) = var(univ(nil)).
univ_list([U]) = var(U) :- not(U = univ(nil))).
univ_list([U | Us]) = cons(U, Us).

:- interface.

%-----------------------------------------------------------------------------%
%
% Lua Types
%

	
% In Lua, variables are not typed, values are.  Lua recognizes eight types.

:- type lua_type
	--->	none			% rarely used, represents invalid type
	
	% Value types
	;	nil_type		% the abscence of value
	;	number_type		% double prescision, casts to float
	;	boolean_type		% boolean truth value, casts to bool
	;	string_type		% string value, casts to string
	;	lightuserdata_type	% A C pointer
	
	% Refrence types
	;	function_type		% A Lua function
	;	table_type		% A Lua table
	;	thread_type		% A Lua coroutine
	;	userdata_type.		% Full userdata 
	
	

	% Look up the Lua type of a given variable. 
	% 
:- func lua_type(T) = lua_type.




%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
% Lua objects/refrence types
%

% These types represent Lua values that cannot easily be passed by value as a 
% C or Mercury primitive type. In terms of Lua semantics, these variables are
% passed by refrence, not by value. 
% 
% This is important to keep in mind when performing equality tests.  
% An equality test on two of these variables created seperately will fail, 
% even if they both have the same literal value. Equality will succeed when
% two refrence type variables refrence the same memory adress. 
%
% Note that the refrences discussed here are NOT normal C pointers, but values 
% internal to Lua's register-based VM.  As a result, (with the exception of 
% userdata) these values cannot be instantiated outside of Lua, only refrenced.
%
% Lua variables with assigned refrence types may be assigned metatables, 
% Lua tables that store metadata about how Lua may interact with said variables.

:- type ref.

:- func value(ref) = T is semidet.

%-----------------------------------------------------------------------------%
%
% Lua tables
%


% For the purposes of this library, values may only be assigned to unique 
% tables created in Mercury.  Any tables that have been exposed to Lua
% execution must be considered immutable to preserve Mercury's
% pure declarative semantics.


	% In Lua, the only native data-structure is the table. The Lua table
	% is implemented with a hybrid array/hash table data structure,
	% allowing tables to be efficiently used in the place of arrays, lists
	% maps and sets. 
	%
	% Tables may be assigned metatables, which can store metadata about
	% 
:- type table.

:- implementation. % TODO: Merge with main implementation

:- type table
	---> table(lua, ref).
	
:- interface.

% Note that the nondeterministic calls for table lookups have a notably higher
% performance cost than the semidet ones.

	% Lookup a value in a table by a given key.  Key's that have not been
	% assigned a value, (or usage of the nil value as a key) will produce
	% a nil result.  The semantics of this library reserves failure for
	% type incompatability.  That said, in Mercury, a table will NEVER 
	% produce a non-nil value from usage of nil as a table's key (attempting
	% to do so in Lua will result in an error).
	%
	% This library does not provide a way to perform lookups on unique 
	% tables that preserve a table's uniqueness, given that unique tables
	% represent new tables created and assigned values by Mercury.    
	%
:- pred get(K, V, table).
:- mode get(in, out, in) is semidet.
:- mode get(out, in, in) is nondet.
:- mode get(out, out, in) is nondet.

:- func table ^ index(K) = V is semidet.

:- implementation. % TODO: Merge with main implementation

:- import_module state.

%get(K, V, table(L, R)) :- Hold that thought...



% Tables may not be modified unless they are unique, garunteeing that Mercury
% holds the only refrence to a table, and as a result avoids causing any 
% unintended side effects in Lua.

	% Create an empty table.
	%
:- pred new_table(table::uo) is det.
:- func new_table = table.
:- mode new_table = uo.

	% Create an empty table and assign a metatable to it.
	%
:- pred table_meta(table::uo, table::in) is det.
:- func table_meta(table) = table.
:- mode table_meta(in) = uo is det.

% Metatables can define 'weakness' in tables, stating that keys and/or values in
% tables are weak refrences that do not prevent Lua from garbage collecting
% them.

:- type weakness
	---> 	none
	;	keys
	;	values
	;	any.

	% Determine the weakness of a table.
	%
:- pred weakness(weakness::out, table::in) is det.
:- func weakness(table) = weakness.

	% The following is shorthand for table_meta, assigning a metatable to a
	% new table that only defines the table's weakness.
	%
:- pred new_weak_table(weakness::in, table::uo) is det.
:- func new_weak_table(weakness) = table.
:- mode new_weak_table(in) = uo.

	% Assign a value to a unique table.
	%
	% Assigning multiple values to the same key will overwrite any existing
	% value assigned to that key.
	%
	% WARNING: attempting to assign a value to a nil key will produce an
	% error.
	%
:- pred set(K::in, V::in, table::di, table::uo) is det.

:- func (table ^ elem(K) := V) = table.
:- mode (di ^ elem(in) := in) = uo is det.


%-----------------------------------------------------------------------------%
%
% Lua functions
%

	
% Lua functions are either compiled chunks of Lua source code, or are C
% function pointers as defined by the C type "lua_CFunction". Lua functions 
% are varadic, accepting variable numbers of arguments and
% return values.
% 
% Lua handles functions as first-class variables, they can be assigned
% to variables and passed as function arguments or return values in
% the same manner as any other value in Lua.
% 
% Lua functions are inherently impure.  Not only can they cause
% side effects to variables passed as arguments, but they are lexically 
% scoped, allowing side effects on any local variable declared in a scope
% outside of the function's scope.
%
% This fact effectively removes any way of determining a Lua function's purity
% outside of compiling a function directly with calls like loadstring and
% dostring, or passing a lua_CFunction function pointer.


:- type function.
	

%-----------------------------------------------------------------------------%
%
% Lua userdata
%
	% Lua handles all foreign values with the userdata value type.
	% Under normal circumstances, Lua can't actually do anything with
	% userdata. Attempts to use indexing, arithmetic or other operators
	% will result in a lua error.  However, as with tables and functions,
	% it is possible to assign metatables to userdata, allowing one to
	% extend the valid syntax with which Lua can interact with a given
	% userdata.
	% 
	% Note that userdata that contains a Mecury type will be passed to 
	% Mercury as that type.  Userdata may contain foreign types that are
	% not compatable with Mercury, although such types can still be 
	% refrenced by c_pointer.
	
% TODO: Metamethods for userdata







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

:- mutable(current_lua_state, lua_state, null_state, ground, 
	[trailed, attatched_to_io_state, 
	foreign_name("C", "luaMR_current_lua_state"), thread local]).


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

:- pred init(lua_state::in, io::di, io::uo).

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

:- pragma foreign_proc("C", ready(L::in, , Answer::out, _I::di, _O::uo), 
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




%-----------------------------------------------------------------------------%
%
% Refrences
%

% A Lua variable can be used to store any value that can be stored as a 
% C type.  Furthermore, because variables are instantiated and stored within
% the Lua state, Mercury cannot construct, deconstruct, equality test or 
% refrence Lua variables directly like it can C types. These operations are
% handled by the C API.
%
% Another thing to note is the fact that the lifetimes of Lua variables are
% manged by the Lua garbage collector, a mark and sweep collector not unlike
% the Bohem GC. 
%
% The current implementation of this library places reservations on and
% registers finalizer callbacks with variables passed by refrence between
% Mercury and Lua in such a manner that a Variable passed to it's non-native 
% environment will not be collected by it's own garbage collector until the
% non-native garbage collector calls the finalizer associated with it.

	% The ref type represents an indirect refrence to a variable 
	% instantiated in Lua. So long as Mercury does not garbage collect 
	% the var, Lua will not garbage collect the refrenced variable.
	%
:- type ref.

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



%-----------------------------------------------------------------------------%
%
% Lua Types
%
:- pragma foreign_enum("C", lua_type/0, 
[
	none - "LUA_TNONE",
	nil_type - "LUA_TNIL",
	boolean_type - "LUA_TBOOLEAN",
	lightuserdata_type - "LUA_TLIGHTUSERDATA",
	number_type - "LUA_TNUMBER",
	string_type - "LUA_TSTRING",
	table_type - "LUA_TTABLE",
	function_type - "LUA_TFUNCTION",
	userdata_type - "LUA_TUSERDATA",
	thread_type - "LUA_TTHREAD"
]).




is_nil(T) :- dynamic_cast(T, nil:nil).

%-----------------------------------------------------------------------------%
%
% Lua tables
%



:- type table ---> table(ref).


%-----------------------------------------------------------------------------%
%
% Lua functions
%

:- type function 
	---> function(ref).


%-----------------------------------------------------------------------------%
%
% Lua userdata
%

:- type userdata ---> userdata(ref).


%-----------------------------------------------------------------------------%
%
% Lua thread
%

:- type thread == lua_state.






