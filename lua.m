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
	-Allow Mercury to backtrack over changes made to Lua.
	-Initial support is for Lua 5.1, eventual support for Lua 5.2
	-Support for Lua coroutines

*/

:- interface.


% types that can be passed by value to Lua.
:- include_module lua.nil, lua.string lua.bool, lua.int, lua.float. 
:- include_module lua.thread,  lua.c_function, lua.lightuserdata.

% types that cannot be passed directly from lua, but can be refrenced purely from within a closure.
:- include_module lua.object, lua.table, lua.function.

% types that can be used to construct and deconstruct Lua tables
:- include_module lua.list, lua.assoc_list, lua.map, lua.set, lua.store, lua.array.

:- import_module io.


%%%%  Handling the lua state  %%%%

% This is the lua_state C type defined in lua.h
:- type lua_state.

% Typeclasses for types that can be passed to and from Lua variables
:- typeclass lua_value(T).
:- typeclass lua_var(T).

% Represents types
:- type lua_var --->
	some [T] (value(T) => lua_value(T)).

%%%%  Lua modules  %%%%

:- typeclass lua_module(T) where [
	pred load_module
	

%%%%  Lua type system  %%%%

/* In Lua, variables are dynamically typed, type is associated with value, not the variable.
There are eight basic types in Lua, one of which is further divided into two variations in C.

Value Types - Primitive values
	nil 	indicates the abscence of value, used as a return value and as a list terminator
	number 	equivalent to Mercury float or C double, Lua also allows Integers to be easily passed 
	boolean	truth value equivalent to Mercury bool
	string	similar to Mercury strings, can be passed as char *
	
Refrence Types - Cannot be passed directly, but must be handled by refrence, their functionality
		can be extended with the use of metatables
		
	table		efficient associative array implemented by a hash table
	function	varadic first class value in lua, lexically scoped with varadic return values
	userdata	Lua's type for foreign values
	thread		Used to handle refrences to the lua_state and coroutines
	lightuserdata	A special case of userdata, it holds a C pointer and cannot have a metatable
	
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
    
:- func type(T) = lua_type is det.  % Non compatable values return the 'none' lua type. 

type


    
%%%%-------------------------------------------------------------------------------------------------------------------%%%%
:- implementation.

% Header %
:- pragma foreign_decl("C", "
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>




// Names for the lua registry
#define AP_TYPE "luaAPOLLO_LANDER_MERCURY_TYPE"
#define AP_MODULE "luaAPOLLO_LANDER_MODULE"
#define AP_READY "luaAPOLLO_LANDER_READY"


// Upvalues for function context
#define AP_SELF 1


int luaAP_init_apollo(lua_State *);
int apollo_ready(lua_State *);
").

:- pragma foreign_type("C", lua_state, "lua_State *").

:- type c_function. % A C function pointer with the signature int (lua_Cfunction *)(lua_State *);

:- pragma foreign_type("C", c_function, "lua_CFunction *").

:- pragma foreign_code("C", "

// check apollo for modules when loading modules in lua.
int apollo_ready(lua_State * L) {
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_READY);
	int ready = lua_toboolean(L, 1);
	lua_remove(L, 1);
	return ready;
}

/* take the provided module name and attempt to load an apollo module
passes any additional arguments.
int luaAP_loader(lua_State * L) {
	if (lua_isstring(L, 1)) {
		const char * module_name = lua_tostring(L, 1)


int luaAP_init_apollo(lua_State * L) {
	
	lua_checkstack(L, 3);
	
	// Add tables to the registry.
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_TYPE);
	
	lua_newtable(L);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_MODULE);
	

	
:- func lua_state(lua_var) = lua_state is semidet.
lua_state(ref(L, _)) = L.




:- pragma foreign_enum("C", lua_type, [
    none - "LUA_TNONE",
    nil - "LUA_TNIL",
    number - "LUA_TNUMBER",
    boolean - "LUA_TBOOLEAN",
    string - "LUA_TSTRING",
    table - "LUA_TTABLE",
    function - "LUA_TFUNCTION",
    userdata - "LUA_TUSERDATA",
    thread - "LUA_TTHREAD",
    lightuserdata - "LUA_TLIGHTUSERDATA" ]).
    


    
