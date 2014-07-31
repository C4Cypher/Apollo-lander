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

:- import_module io.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module map.


%-----------------------------------------------------------------------------%
%
% The Lua state
%

	% The lua_state type represents the state of a running 
	% Lua Virtual Machine. (Lua VM for short) Note that as a convention 
	% borrowed from the C API, procedures that query or manipulate the Lua 
	% state will use the variable term 'L' to refer to the Lua state.
	%
:- type lua_state

	% Abstract representation of a global Lua state
	--->	global_state(	
			globals::map(string, value), 
			refs::map(int, value),
			registry::map(string, value)
		),
		
		
	;	lua_state(lua_state_ptr)	% Concrete lua_State
	;	coroutine(thread)		% Child thread
	;	scope(environment)		% Local scope
	;	call(environment, args) 	% Function call
	;	return(lua_state, return)	% Call with return values
	.
	
	
:- type lua == lua_state.


	% The lua_state_ptr is a refrence to a lua_State derived from an
	% instantiated Lua VM.  This type is defined in lua.h as
	% the C type "lua_State *". 
	%
:- type lua_state_ptr.


% WARNING! Refrences to Lua types (tables, functions, userdata) derived
% from one global lua_state are NOT compatible with other seperately created
% lua_states. The only exception to this is lua_states created as threads.
% lua_threads may freely pass variables to or from their parent state and
% sibling threads.

%-----------------------------------------------------------------------------%
%
% Lua Variables
%

	% In Lua, Variables contain values, however, due to the fact that 
	% the values represented by Lua variables are stored in the Lua state
	% by string name or int index, outside the context of a Lua state,
	% a Lua variable is meaningless.  In Mercury, Lua variables act more
	% like identifiers, used to look up a desired value from the Lua state.
	%
:- type var
	--->	global(string)
	;	local(string)
	;	var(id, ref)
	;	feild(var, var)		% Table lookup
	.
	
	% Opaque identifier for local variables id's,
	%
:- 
	
:- type var(T)
	---> 	var(T)

	% Look up the value represented by a variable, or create a new variable
	% representing a value. Fails on Mercury type mismatch.
	%
:- pred var(T::out, var::in, lua::in) is semidet.

:- pred var(T, var, lua, lua).
:- mode var(in, out, in, out) is det.
:- mode var(out, in, in, out) is semidet. 

:- func value(var, lua) = T is semidet.
:- func det_value(var, lua) = value is det.
	

%-----------------------------------------------------------------------------%
%
% Lua Values and types
%

:- type value
	--->	some(univ)		% unknown type
	
% In Lua, variables are not typed, values are.  Lua recognizes eight types.
	
	% Value types
	;	nil				% the abscence of value.
	;	number(float)			% double prescision float
	;	true				% boolean true
	;	false				% boolean false
	;	string(string)			% string
	;	lightuserdata(c_pointer)	% A C void pointer
	
	% Refrence types
	;	function(function)		% A Lua function
	;	table(table)			% A Lua table
	;	thread(thread)			% A Lua coroutine
	;	userdata(userdata)		% Full userdata
	.
				

	% Succeeds if two values are equal under Lua semantics.
	% Will not call metamethods, does NOT unify Mercury variables.
:- pred equal(T1::in, T2::in, lua::in) is semidet.
:- pred equal(T1::in, T2::in, lua::in, lua::out) is semidet.


	% Operators for use with DCG notation
	
:- pred ==(T1::in, T2::in, lua::in, lua::out) is semidet.
:- pred ~=(T1::in, T2::in, lua::in, lua::out) is semidet.


	% The type lua.type represents the types that Lua recognizes.
	% 
:- type lua_type 
	--->	none
	;	nil
	;	boolean
	;	lightuserdata
	;	number
	;	string
	;	table
	;	function
	;	userdata
	;	thread.

	% Look up the Lua type of a given variable. 
	% 
:- func lua_type(lua, var) = lua_type.

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
:- pred is_nil(T::in) is semidet.

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
:- mode get(out, in, in) is cc_nondet.
:- mode get(out, out, in) is nondet.

:- func get(K, table) = V.
:- mode get(in, in) = out is semidet.
:- mode get(out, in) = in is cc_nondet.
:- mode get(out, in) = out is nondet.

	% The keys of a table.
	%
:- pred key(K, table).
:- mode key(in, in) is semidet.
:- mode key(out, in) is nondet.

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
:- mode table_meta(in) = uo.

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
:- pred new_weak_table(weakness::in, table::uo).
:- func new_weak_table(weakness) = table.
:- mode new_weak_table(in) = uo.

	% Assign a value to a unique table.
	%
	% !Table ^ set(Key, Value).
	% Table0 ^ set(Key) := Value = Table1.
	%
	% Assigning multiple values to the same key will overwrite any existing
	% value assigned to that key.
	%
	% WARNING: attempting to assign a value to a nil key will produce an
	% error.
	%
:- pred set(K::in, V::in, table::di, table::uo) is det.
:- pred 'set :='(K::in, table::di, V::in) = (table::uo) is det.

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


	% Load a string and compile it into a Lua function.
	%
:- func load_string(string) = function.



% Function prototypes are func and pred types that Lua can cast directly to
% a Lua function.  Note that this is a one way process. Once converted to a Lua
% function, these values cannot be compared or tested against the preds or funcs
% used to construct them.  
%
% It is highly reccomended that function prototypes be made polymorphic, not 
% bound to a paticular type.  If more type strictness is needed, use of the
% function typeclass is reccomended.

	% Prototype for a functionally pure Lua function.
	% Semidet/cc_nondet versions return nil on falure.
	%
:- type function_pred(K, V) == pred(closure(K,V), closure(int,V)). 
:- inst function_pred(K, V) 
	--->	pred(ci, uo) is det
	;	pred(ci, uo) is semidet
	;	pred(ci, uo) is cc_multi
	;	pred(ci, uo) is cc_nondet.

:- func pred_to_function(function_pred(K,V)) = function.

:- type function_func(K, V) == func(closure(K,V)) = closure(int,V). 
:- inst function_func(K, V) 
	--->	func(ci) = uo is det
	;	func(ci) = uo is semidet.

:- func func_to_function(function_func(K, V)) = function.

% TODO: iterators?, multi/nondet functions?


%-----------------------------------------------------------------------------%
%
% Lua Statements
%

	% In Lua 'statements' are more commands telling Lua what to do.
	% Mercury's equivalent, the predicate, is a statement of absolute 
	% truth.  Here statements are closer to predicates, describing truth
	% values within a given Lua context.  
	
%-----------------------------------------------------------------------------%

% TODO: coroutines?

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
	
:- type userdata.

:- type lightuserdata ---> lightuserdata(c_pointer).

:- func userdata(T) = userdata.
:- mode userdata(in) = out is det.
:- mode userdata(out) = in is semidet.




%-----------------------------------------------------------------------------%
%
% Lua Coroutines
%

% TODO: Explain Lua coroutines.

% TODO: Implement coroutines with threads.

:- type thread.



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

#define MR_LUA_MODULE ""MR_LUA_LANDER_MODULE""
#define MR_LUA_READY ""MR_LUA_LANDER_READY""
#define MR_LUA_IMMUTABLE ""MR_LUA_IMMUTABLE""
#define MR_LUA_UDATA ""MR_LUA_UDATA_METATABLE""

#define MR_LUA_TYPE ""__mercury_type""



#define MR_LUA_FUNCTION_UPVALUE 1


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

	% The lua_state type is a literal refrence to the Lua runtime, but the
	% lua type is an abstraction of the Lua state.
	%
:- pragma foreign_type("C", lua_state, "lua_State *",
	[can_pass_as_mercury_type]).

	% For the sake of safety and sanity checking, produce a lua_state
	% instantiated as a NULL pointer.
	%
:- func null_state = lua_state.

:- pragma foreign_proc("C", null_state = Null::out, 
	[promise_pure, will_not_call_mercury], 
	"Null = NULL;").


% TODO: Module/thread local mutvar for current state.

 

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

/* Mark a Lua state (and all of it's child threads) as immutable. */
void luaMR_make_immutable(lua_State * L) {
	lua_pushboolean(L, 1);
	luaMR_setregistry(L, MR_LUA_IMMUTABLE);
}

/* Check to see if a lua_State is marked as immutable. */
int luaMR_is_immutable(lua_State * L) {
	luaMR_getregistry(L, MR_LUA_IMMUTABLE);
	const int result = lua_toboolean(L, -1);
	lua_pop(L, 1);
	return result;
}

").

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

:- pragma foreign_proc("C", ready(L::in), 
	[promise_pure, will_not_call_mercury], "
	SUCCESS_INDICATOR = luaMR_ready(L);
").

:- pragma foreign_proc("C", ready(L::di, L1::uo, Answer::out), 
	[promise_pure, will_not_call_mercury], "
	if(luaMR_ready(L))
		Answer = MR_YES;
	else
		Answer = MR_NO;

	L1 = L;
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
%-----------------------------------------------------------------------------%
%
% Variables
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

:- type var(T)
	---> 	arg(int)
	;	local(int)
	;	upvalue(int)
	;	ref(ref)
	;	value(T)
	where equality is var_equals.

var(T) = V :-
	( 
		I = Index, 
			( 
				V@arg(Index) 
			; 
				V@local(Index)
			),
		

:- pred var_equals(var(T)::in, var(T)::in

:- impure pred push_arg(lua_state, arg


%-----------------------------------------------------------------------------%
%
% Refrences
%

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
	nil - "LUA_TNIL",
	boolean - "LUA_TBOOLEAN",
	lightuserdata - "LUA_TLIGHTUSERDATA",
	number - "LUA_TNUMBER",
	string - "LUA_TSTRING",
	table - "LUA_TTABLE",
	function - "LUA_TFUNCTION",
	userdata - "LUA_TUSERDATA",
	thread - "LUA_TTHREAD"
]).


lua_type(V) = Type :-
	( V:nil ->
		Type = nil
	; V:bool ->
		Type = boolean
	; 
	; (V:int ; V:float) ->
		Type = number
	; V:bool	
 sorry($module, $pred), fail.

:- use_module lua.value.

:- type value == lua.value.value.


equal_value(_, _) :- sorry($module, $pred).


is_nil(T) :- dynamic_cast(T, nil).

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






