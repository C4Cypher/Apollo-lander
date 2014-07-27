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
% This file presents a simple interface for handling and passing values to
% and from the Lua runtime VM.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module lua.

:- interface.

:- include_module lua.state.

:- import_module nil.

:- import_module io.
:- import_module int.
:- import_module bool.
:- import_module string.
:- import_module list.
:- import_module maybe.






:- func global(string) = T is semidet.




%-----------------------------------------------------------------------------%
%
% Lua types
%

% In Lua, variables are not typed, values are.  Lua recognizes eight
% types.
%
% 'nil' represents the abscence of value. 
% 'number' is equivalent to float type.
% 'boolean' is equivalent to the bool type.
% 'string' is equivalent to the string type.
%
% 'table', a refrence to an associative array, used for lists and maps.
%	Unlike an assoc_list, a Lua table may not associate more than
%	one value with any given key, and thus, behaves more like the
% 	Mercury map type.
% 
% 'function' is a refrence to a Lua function, which are impure, 
%       may have a variable number of arguments, multiple return
%	values and can be passed freely like any other variable.
%
% 'userdata' is lua's type for handling foreign data, unless otherwise
%	noted, values stored as userdata are subject to collection by 	
% 	Lua's Garbage Collector.
%
% 'lightuserdata' is seen in Lua as identical to userdata, but contains
%	a C pointer which will not be collected by Lua's GC.
%
% 'thread' is a lua_state, usually a coroutine, note that the main
% lua_state should not be treated like a coroutine.



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

	% type/1 and type/2 query Lua for a Lua variable's type.  If 
	% provided a Mercury primitive value that can be implicity
	% cast to a Lua value, that type's equivalent value will be
	% returned.  For more complex Mercury types, 'none' will be
	% returned.
	% 
:- func lua_type(T) = lua_type.

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

% These types represent lua.vars of a specific Lua type.  In terms of Lua 
% semantics, these variables are passed by refrence, not by value. 
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

:- type var.

:- func var(T) = var.
:- mode var(in) = out is det.
:- mode var(out) = in is semidet.

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

	% Export Mercury code as a function.
	%
:- func function(T) = function <= function(T).

	% Load a string and compile it into a Lua function.
	%
:- func load_string(string) = function.

	% Var_Arg(Index::out, Argument::out) is nondet
	% 
	% A higher order type that represents varadic function arguments.
	% Index represents the Argument's placement in the order of arguments
	% passed, starting with 1 for the first argument.
	%
:- type var_arg(T) == pred(int, T).
:- inst var_arg(T) == pred(out, out) is nondet.


% Function prototypes are func and pred types that Lua can cast directly to
% a Lua function.  Note that this is a one way process. Once converted to a Lua
% function, these values cannot be compared or tested against the preds or funcs
% used to construct them.  
%
% It is highly reccomended that function prototypes be made polymorphic, not 
% bound to a paticular type.  If more type strictness is needed, use of the
% function typeclass is reccomended.

	% Accepts and returns varadic arguments.
	%
:- type func_function_prototype(T) == (func(var_arg(T)) = var_arg(T)).
:- inst func_function_prototype(T) == (func(in) = out is det).

	% Det always returns nil. Semidet returns boolean true or false.
	%
:- type pred_function_prototype(T) == (pred(var_arg(T))).
:- inst pred_function_prototype(T) 
	---> 	(pred(in) is det)
	;	(pred(in) is semidet).


	% The args typeclass represents a type that can be constructed from
	% varadic arguments.
	%
:- typeclass args(A) where [
	func varargs(var_arg(T)) = A
].


	% The return typeclass represents a type that can be deconstructed into
	% varadic arguments.
	%
:- typeclass return(R) where [
	func return_varargs(R) = var_arg(T)
].

 

:- typeclass function(F) where [
	func call_function(F, A) = R <= (args(A), return(R))
].




%-----------------------------------------------------------------------------%

% TODO: coroutines?, iterators?, multi/nondet functions?

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

:- type metamethod
	---> 	add
	;	subtract
	;	multiply
	;	divide
	;	modulo
	;	power
	;	negative
	;	concatenate
	;	length
	;	equals
	;	lessthan
	;	less_or_equal
	;	index
	;	newindex
	;	call
	;	gc.


	% This typeclass allows additinal metamethod calls to be attached
	% to specific Mercury types.
	%
:- typeclass userdata(T) where [
	
	
	% Return the metamethod associated with this instance.
	%
	func metamethod(metamethod, T) = (func(T),
	mode metamethod(in, in) = out is semidet
].
	
	% Examine the first (bottom) value on the stack and return it's
	% mercury value if it is of this type.
	% For use implementing self_is_userdata.
	%
pred self_is(T::out) is semidet.


%-----------------------------------------------------------------------------%
%
% Lua Coroutines
%

% TODO: Explain Lua coroutines.

:- type thread.



%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module lua__state.

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






