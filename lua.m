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

% For interfacing with Lua
:- include_module lua.module, lua.stack.

% types that can be passed by value to Lua as primitives.
:- include_module lua.primitive, lua.nil, lua.string lua.bool, lua.int, lua.float. 
:- include_module lua.thread,  lua.c_function, lua.lightuserdata.

% types that cannot be passed directly from lua, but can be refrenced from within a closure.
:- include_module lua.object, lua.table, lua.function.

% types that can be used to construct and deconstruct Lua tables
:- include_module lua.list, lua.assoc_list, lua.map, lua.set, lua.store, lua.array.




%%%% 	Handling the lua state  

% This is the lua_state C type defined in lua.h, all operations performed on a lua_state must leave the lua state in
% the same condition that it was found.
:- type lua_state.
:- type lua.state == lua_state.


:- func new_state = lua_state::uo is det.
:- pred load_libs(lua_state::di, lua_state::uo) is det.
:- pred load_mercury(lua_state

 




% A compatable mercury can represent a single lua variable, or, in special cases can represent a list of lua values.

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









    
%%%%-------------------------------------------------------------------------------------------------------------------%%%%
:- implementation.

:- pragma foreign_decl("C",
	"#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").

:- pragma foreign_type("C", lua_state, "lua_State *").
:- pragma foreign_type("C", c_function, "lua_CFunction *").

:- import_module lua.value, lua.api.

	
:- func lua_state(lua_var) = lua_state is semidet.
lua_state(ref(L, _)) = L.

:- pred push(lua_var::di, lua_state::in, lua_var::uo) is semidet.
push(Var, In, Out) :- check_stack(1, In, Lua), (
	value(Val) = Var, push_value(Val, Lua, Out);
	ref(Lua, local(_)) = Var, Out = Var);
	ref(Other, local(I)) = Var, Other /= Lua, xmove
	
:- type lua_var --->
	some [T] (value(T) => lua_value(T));
	ref(lua_state, lua_ref);
	invalid.
	
:- type lua_ref --->
	local(int);
	global(string);
	registry(string);
	refrence(int);
	upvalue(int).
	
:- some [T] (func value_of(lua_var) = T => lua_value(T)).
:- mode value_of(di) = out is semidet.

value_of(Var) = Val :- 
	Val = value(Var) ;
	Val = push(Var, Ref), ; 
	
	


:- func maybe_value(lua_var) = maybe(T). 

:- func make_var(T) 

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
    
% Backtracking

:- pragma foreign_code("C", "
	
	typedef struct luaM_Checkpoint {
		lua_State * lua_state;
		int top;
	} luaM_Checkpoint;
    
    void luaM_restore_checkpoint(void * p, MR_untrail_reason r) {
    	
    	switch(r){
		case MR_undo:       /* Fall through. */
		case MR_exception:  /* Fall through. */
		case MR_retry:
		   	luaM_Checkpoint * checkpoint = (checkpoint *) p;
    			int current  = lua_gettop(checkpoint.lua_state);
    			assert(current >= checkpoint.top);
    			if(current == checkpoint.top)
    				break;
    			lua_settop(checkpoint.lua_state, checkpoint.top);
			break;

		case MR_solve:  /* Fall through */
		case MR_commit: 
			break;

		default:
			MR_fatal_error(""lua.m: unknown MR_untrail_reason"");
		}
	}
").

:- pred set_checkpoint(lua_state::in) is det.

:- pragma foreign_proc("C", set_checkpoint(L::in), "
	luaM_Checkpoint * c = malloc(sizeof(luaM_Checkpoint));
	c.lua_state = L;
	c.top = lua_gettop(L);
	
	MR_trail_function(luaM_restore_checkpoint, (void *) c);
").
    
    
