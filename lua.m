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

:- import_module io, int, float, bool, string, list, lua.value, lua.table,
	lua.c_function, lua.function, lua.userdata.

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
:- type lua_value.


%%%%%%%%%%%%%%%%%
% Lua Variables %
%%%%%%%%%%%%%%%%%

% Represents a lua varaible constructor
:- type lua_var --->

	% Value constructors
	nil ;
	int(int) ;
	float(float) ;
	bool(bool) ;
	string(string) ;
	
	% Some implemention as defined by lua.value
	value(lua_value) ;
	
	% refrence types
	table(table) ;
	function(function) ;
	userdata(userdata) ;
	thread(thread) ;
	
	% opaque refrence
	ref(lua_ref) 

	where equality is raw_equal, comparison is raw_compare.

	

% A refrence to an instantiated variable in Lua.
:- type lua_ref.







% Does not trigger metamethods
:- pred raw_equal(lua_var, lua_var) is semidet.
:- pred raw_compare(comparison_result::uo, lua_var::in, lua_var::in) is det.


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

:- import_module lua_state, lua_value.

:- pred valid_stack_index(lua_state::in, int::in, io::di, io::uo) is semidet.

valid_stack_index(L, I, !IO) :- I \= 0 , abs(I) =< get_top(L, !IO) ; 
	I = global ; I = registry.
	 


%%%%%%%%%%%%%	
% Lua Types %
%%%%%%%%%%%%%

type_of(V) = Type :- 
	(
		L = get_ref(V),
		check_top(L^state, 1, !IO) ,
		push_value(L^state, L^index, !IO) ,
		get_type(L^state, -1, Type, !IO)
	) ; Type = none.
	
is_type(T, type_of(T)).

is_nil(T) :- type_of(T) = nil.



%%%%%%%%%%%%%%%%%
% Lua Refrences %
%%%%%%%%%%%%%%%%%

:- typeclass lua_ref(T) where [ 
	func get_ref(T) = lua_ref,
	mode get_ref(in) = out ].

:- type lua_ref ---> ref(state::lua_state, index::int).

:- instance lua_ref(lua_ref) where [ get_ref(R) = R ].

:- instance lua_value(lua_ref) where [
	( push(L, R, !IO) :- 
		L = R^state , push_value(L, R^index, !IO) ;
		
		check_stack(R^state, 1, !IO) ,
		push_value(R^state, R^index, !IO) ,
		xmove(R^state, L, 1, !IO)
	),
	
	( pull(L, Index, !IO) = ref(L, Index) :- valid_stack_index(L, Index, !IO) )
].

:- func global = int is det.

:- pragma foreign_proc("C", global_index = I::out, 
	[will_not_call_mercury, promise_pure],
	"I = LUA_GLOBALINDEX;").
	
:- func registry = int is det.

:- pragma foreign_proc("C", registry_index = I::out, 
	[will_not_call_mercury, promise_pure],
	"I = LUA_REGISTRYINDEX;").
	
		

%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%

:- type lua_value --->
	some [T] (valid(T) => lua_value(T)) ;
	invalid.
	
:- func value(T) = lua_value is det.

value(Value) = Lua_Val :- Lua_Val = 'new valid'(Value) ; Lua_Val = invalid.

:- some [T] func value_of(lua_value) = T is semidet.

value_of(Lua_Val) = Value :- Lua_Val = valid(Value).  

%%%%%%%%%%%%%%%%%
% Lua Variables %
%%%%%%%%%%%%%%%%%

:- instance lua_value(lua_var) where [
	( push(L, Var, !IO) :- 
		Var = nil; push_nil(L, !IO) ;
		Var = int(Int), push_int(L, Int, !IO) ;
		Var = float(Float), push_float(L, Float, !IO) ;
		Var = bool(Bool), push_bool(L, Bool, !IO) ;
		Var = string(String), push_string(L, String, !IO) ;
		Var = value(Value), push(L, Value, !IO)
		Var = table(Table), push(L, Table, !IO) ;
		Var = function(Function), push(L, Function, !IO) ;
		Var = userdata(Userdata), push(L, Userdata, !IO) ;
		Var = thread(Thread), push(L, Thread, !IO) ;
		Var = ref(Ref), push(L, Ref, !IO). 
	),
		
	( pull(L, I, !IO) = Var :- valid_stack_index(L, I, !IO) ,
		get_type(L, I, T, !IO), 
		(
			T = nil, Var = nil ;
			T = number , (
				Var = int(V), to_int(L, I, V, !IO)  ;
				Var = float(V), to_float(L, I, V !IO)
			) ;
			T = bool, Var = bool(V), to_bool(L, I, V, !IO) ;
 			T = string, Var = string(V), to_string(L, I, V, !IO) ;
 			Var = table(pull(L, I, !IO)) ;
 			Var = function(pull(L, I, !IO)) ;
 			Var = userdata(pull(L, I, !IO)) ;
 			Var = thread(pull(L, I, !IO)) ;
 			Var = value('new valid'(pull(L, I, !IO))) ;
 			Var = ref(pull(L, I, !IO)) 
 		)
 	)
].
 			
 			
 			




	


    
