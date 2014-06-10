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

:- import_module list, string, bool, int, float, maybe.

:- include_module lua.value, lua.api.

:- pragma require_feature_set([trailing]).

%%%% 	Handling the lua state  

% This is the lua_state C type defined in lua.h, all operations performed on a lua_state must leave the lua state in
% the same condition that it was found.
:- type lua_state.


% Represents either a value instantiated in lua or a mercury value that is compatable with lua.  These values should be
% considered immutable and their scope should be limited to the scope of the exact lua state that produced them.
% The types which can be converted to lua variables can be extended using the typeclass defined in lua.value.
:- type lua_var.
:- type lua_vars == list(lua_var).

:- pred valid(lua_var::in) is semidet.
:- func type(lua_var) = lua_type is det.

:- func value(T) = lua_var.
:- mode value(in) = out is det.
:- mode value(in) = uo is det.
:- mode value(in) = muo is det.
:- mode value(out) = in is semidet.
:- mode value(out) = di is semidet.
:- mode value(out) = mdi is semidet.

	
% The following types specify operations that can be performed on a lua state, such operations must be pure, leaving no
% side effects on the lua_state, and leaving the stack in the same state that it was found. Note that Lua itself does not
% hold any distinction between subroutines, predicates, functions and iterators, treating them all as functions. The 
% distinction is made so that mercury can handle

% Perform a pure operation in lua
:- pred run(pred(lua_state), lua_state).
:- mode run(in(pred(in) is det), in) is det.
:- mode run(in(pred(in) is semidet), in) is semidet.

% Lua ^ run(Something) <=> run(Something, Lua) :- Something(Lua).


% Return a function that does nothing.
:- func end = pred(lua_state).
:- mode end = out(pred(in) is det) is det.

% Retrieve the value(s) currently on the top of the stack
:- func get(T, pred(lua_state)) = pred(T, lua_state) <= lua_value(T).
:- mode get(out, in(pred(in) is det)) = out(pred(in) is semidet)) is det. 
:- mode get(out, in(pred(in) is semidet)) = out(pred(in is semidet)) is det.

% somefunc(Lua) = Out :- Lua ^ run(get(Out) ^ end) <=> run(Out, get(Out, end), Lua).

:- func return(pred(lua_state), lua_state) = T <= lua_value(T).
:- mode return(in(pred(in)), in) = out is semidet.

% Lua ^ return(Something) <=> return(

% Push a value onto the stack, perform an operation with it, and then push said values off the stack
:- func local(pred(lua_var, lua_state), T) = pred(lua_state) <= lua_value(T).
:- mode local(in(pred(in, in) is det), in) = out(pred(in) is det) is det.  
:- mode local(in(pred(in, in) is semidet), in) = out(pred(in) is semidet) is det.

% Lua ^ local(Something, Var) ^ do <=> Something(Var, Lua) ^ do <=> do(Something(Var, Lua))

% Rawget a global variable onto the stack, perform an operation with it and then push said variable off the stack.
:- func global(pred(lua_var, lua_state), string) = pred(lua_state).
:- mode global(in(pred(in, in) is det), in) = out(pred(in) is det) is det. 
:- mode global(in(pred(in, in) is semidet), in) = out(pred(in) is semidet) is det.

% Check to see the type of the last value on the stack
:- func type(
:- 
:- func if(pred(lua_state), pred(lua_state)) = pred(lua_state).
:- mode if(in(pred(in) is semidet), in(pred(in) is det)) = out(pred(in)) is det.

:- func if(pred(lua_state), pred(lua_state), pred(lua_state)) = pred(lua_state).
:- mode if(in(pred(in) is semidet), in(pred(in) is det), in(pred(in) is det)) = out(pred(in)) is det.



% Type conversion from mercury types to  lua types and back is handled in the lua.value module via the lua_value typeclass.

% A compatable mercury can represent a single lua variable, or, in special cases can represent a list of lua values.
% Such cases are represented by varag
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

% A type unique to lua, representing the abscence of value.
:- type nil ---> nil.


:- type c_function.  	% cfunction typedef function pointer
:- type m_function == func(I) = O.
:- mode m_function == (func(in) = out
:- type userdata. 	% TODO: use existential typeclass?




    
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
    
    
