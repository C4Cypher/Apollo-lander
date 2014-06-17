:- module lua.

/*
%% lua.m - Apollo-lander

Mercury is a purely declarative logic language. It is related to both Prolog and
Haskell.[1] It features a strong, static, polymorphic type system, as well as a
strong mode and determinism system.

Apollo, the ancient Greek name for the planet Mercury, when observed just before
dawn as a morning star

The Apollo spacecraft was composed of three parts designed to accomplish the 
American Apollo program's goal of landing astronauts on the Moon by the end of 
the 1960s and returning them safely to Earth.

Lua - A lightweight programming language with dynamic typing.
From the Portuguese lua (“moon”). The inventors of the language were Brazilian.

%%----------------------------------------------------------------------------%%

This library is intended to provide a simple, easy to use interface between the
Mercury functional/logic compiled programming language and the Lua Virtual 
Machine in such a manner that it preserves the advantages of both environments.

Mercury and Lua are vastly different programming languages.  

Mercury is derived from Prolog, yet it is strongly-typed, functionally pure 
language that is compiled to C and then to native binary code. Any imperitive 
changes to the program state must be threaded through a state variable to 
preserve purity, or otherwise explicitly marked so that it can be handled safely
by the compiler.  The logical and functional purity of Mercury allow it's
compiler to perform exstensive optimization, and to catch potential problems at
compile-time that other languages would let through.

Lua is an imperitave dynamically-typed scripting language that is compiled to 
mid-level bytecode in realtime so that the interpreter (referred to as the Lua 
Virtual Machine) can execute Lua scripts on the fly.  The only native data 
structure is an efficiently implented hash-table.  Functions are treated as 
first-class variables, and Lua has an exstensive C-API that easily handles 
foreign data making it easy to bind to foreign code. This, coupled with the fact
that it is small and very fast makes it a superb choice for an embedded 
scripting language, or as glue-code between different environments.

Lua may not be the best choice for embedding in a Mercury program, it's
imperitive nature and the maleability of it's code would sacrifice much of the
advantage offered by Mercury's purity.  However, in order to interact with the
real world, Mercury provides language constructs for interacting with mutable
state that can be clunky and akward.

Apollo-lander is intended to allow Lua code to make calls on Mercury predicates
in a controlled manner, allowing Mercury to query the state of the Lua VM
without modifying it, and then returning values back to Lua in a manner that
capitalizes on Lua's capability to use varadic function calls and return values.

Lua functions may be called from Mercury, but only if the function in question
originated in a controlled manner from the Apollo interface.  Lua functions
passed as arguments to Mercury should be opaque, either passed back in return
values.

Planned features:
	-Allow outside coders to define additional types that can be easily
		passed to Lua as return values, using either userdata,
		metatables and/or Lua's existing primitive types, without
		compromising type-safety or having to resort to the Lua API.
	-Allow Lua to make calls on non-deterministic predicates via iterators
		and Mercury multithreading.
	-Initial support is for Lua 5.1, eventual support for Lua 5.2
	-Support for Lua coroutines

*/

:- interface.

:- include_module lua.state, lua.value, lua.table, lua.c_function, 
	lua.function, lua.userdata, lua.module.

:- import_module io, int, float, bool, string, list, lua.value, lua.state.

%%%%%%%%%%%%%	
% Lua Types %
%%%%%%%%%%%%%

:- type lua_type --->
	none;
	nil;
	number;
	boolean;
	string;
	table;
	function;
	userdata;
	thread;
	lightuserdata.
	
% Non-Lua values return the 'none' lua type.    
:- func type_of(T) = lua_type is det.
:- pred is_type(T::in, lua_type::in) is semidet.

:- pred is_nil(T::in) is semidet.

%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%

% Typeclass defined in lua.value



%%%%%%%%%%%%%%%%%
% Lua Variables %
%%%%%%%%%%%%%%%%%

% Represents a lua varaible constructor
:- type lua_var --->

	% Value constructors
	nil ;
	value(int) ;
	value(float) ;
	value(bool) ;
	value(string) ;
	some [T] (value(T) => lua_value(T))
	ref(state::lua_state, index::index) 

	where equality is var_equal, comparison is var_compare.
	
:- func value(T) = lua_var <= lua_value(T).


:- type index.


% Does not trigger metamethods
:- pred var_equal(lua_var, lua_var) is semidet.
:- pred var_compare(comparison_result::uo, lua_var::in, lua_var::in) is det.


%%%%%%%%%%%%%%%%%%%
% Lua Metamethods %
%%%%%%%%%%%%%%%%%%%









    
%%%%%%%%%%%%%%%%%%
:- implementation.
%%%%%%%%%%%%%%%%%%

:- type io == io.state

% C code

:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
").




:- pragma foreign_decl("C", "
// Names for the lua registry
#define AP_TYPE ""luaAPOLLO_LANDER_MERCURY_TYPE""
#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""


// Upvalues for function context
#define AP_SELF 1

// check to see if Apollo has already been initialized.
int apollo_ready(lua_State *);

// lua_CFunction that prepares a lua_State for use with Apollo_lander
int luaAP_init_apollo(lua_State *);
").

%% Body for C interface **

:- pragma foreign_code("C", "

// check to see if Apollo has already been initialized.
int apollo_ready(lua_State * L) {
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
	int ready = lua_toboolean(L, 1);
	lua_remove(L, 1);
	return ready;
}

/* take the provided module name and attempt to load an apollo module
passes any additional arguments. */
int luaAP_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1);
		lua_getfield(L, LUA_REGISTRYINDEX, AP_MODULE);
		lua_getfield(L, 2, module_name);
		return 1;
	}
	return 0;
}
		
int luaAP_init_apollo(lua_State * L) {
	
	// Add tables to the registry.
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_TYPE);
	
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_MODULE);

	// Add loader to package.loaders
	lua_getglobal(L, ""package"");
	lua_getfield(L, 1, ""loaders"");
	const lua_Integer length = (lua_integer)lua_objlen(L, 1);
	lua_pushinteger(L, length + 1);
	lua_pushcfunction(L, luaAP_loader);
	lua_settable(L, 2);
	lua_pop(L, 2);
	
	// Mark Apollo as ready
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_READY);
	return 0;
}

int luaAP

").	
%%%%%%%%%%%%%%%%%%

:- type index == int.

:- pred valid_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_index(L, I, !IO) = I = global ; I = registry ; valid_stack_index(L, I, !IO).

:- pred valid_stack_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_stack_index(L, I, !IO) :- I \= 0 , abs(I) =< get_top(L, !IO) .


%%%%%%%%%%%%%	
% Lua Types %
%%%%%%%%%%%%%

type_of(V) = Type :- 
	V = ref(L, I) , Type = get_type(L, I, !IO) ; 
	Type = none.
	
is_type(T, type_of(T)).

is_nil(T) :- type_of(T) = nil.


%%%%%%%%%%%%%%%%%
% Lua Refrences %
%%%%%%%%%%%%%%%%%

:- func global = index is det.

:- pragma foreign_proc("C", global_index = I::out, 
	[will_not_call_mercury, promise_pure],
	"I = LUA_GLOBALINDEX;").
	
:- func registry = index is det.

:- pragma foreign_proc("C", registry_index = I::out, 
	[will_not_call_mercury, promise_pure],
	"I = LUA_REGISTRYINDEX;").
	

%%%%%%%%%%%%%%%%%
% Lua Variables %
%%%%%%%%%%%%%%%%%

value(T) = 'new value'(T).

:- instance lua_value(lua_var) where [
	( push(L, Var, !IO) :- 
		Var = nil , push_nil(L, !IO) ;
		Var = value(Val) , push(L, Val, !IO) ;
		Var = ref(L1, I), (
			L = L1 -> push_value(L, I, !IO) ;
		
			check_stack(L1, 1, !IO) ,
			push_value(L1, I, !IO) ,
			xmove(L1, L, 1, !IO)
		)
	),
		
	( pull(L, I, !IO) = Var :- valid_index(L, I, !IO) , 
		(
			is_nil(L, I, !IO) , Var = nil ;
			Var = value(pull(L, I, !IO)) ;
 			Var = ref(L, I) 
 		)
 	)
].


var_equal(value(A), value(B)) :- A = B.

var_equal(A, B) :- A = B.
var_equal(value(A), ref(L, I)) :- A = pull(L, I, !IO).
var_equal(ref(L, I), value(A)) :- A = pull(L, I, !IO).

var_equal(ref(L1, I1), ref(L2, I2)) :- 
	L1 = L2 -> raw_equal(L1, L1, L2, !IO) ; (
		check_stack(L1, 1, !IO) ,
		check_stack(L2, 1, !IO) ,
		push_value(L2, I2, !IO) ,
		xmove(L2, L1, 1, !IO) ,
		raw_equal(L1, I1, -1, !IO) -> pop(L1, 1, !IO) ;
		( pop(L1, 1, !IO) , fail )
	).

var_compare(R, value(A), value(B)) :- compare(R, A, B).
var_compare(R, ref(L, I), value(A)) = compare(R, pull(L, I, !IO), A).
var_compare(R, value(A), ref(L, I)) = compare(R, A, pull(L, I, !IO)). 			
var_compare(R, ref(L1, I1), ref(L2, I2)) :- compare(R, pull(L1, I1, !IO), pull(L2, I2, !IO)).			
 			




	


    
