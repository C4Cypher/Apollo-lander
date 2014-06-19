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

:- include_module lua.int, lua.float, lua.bool, lua.string,
	lua.state, lua.list, lua.map, lua.userdata.


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
:- pred (T::in, lua_type::out) is det.
:- func type(T) = lua_type is det.




%%%%%%%%%%%%%%%%%
% Lua Interface %
%%%%%%%%%%%%%%%%%

/* The lua type represents an individual value instantiated in Lua. */

:- type lua.

:- 



:- pred new(lua::in, lua::uo)

:- pred next(lua, lua).
:- mode next(in, in) is semidet.
:- mode next(in, out) is semidet.
:- mode next(out, in) is semidet.


:- func next(lua) = lua.
:- mode next(in) = in is semidet.
:- mode next(in) = out is semidet.
:- mode next(out) = in is semidet.


:- pred previous(lua, lua).
:- mode previous(in, in) is semidet.
:- mode previous(in, out) is semidet.
:- mode previous(out, in) is semidet.


:- func previous(lua) = lua.
:- mode previous(in) = in is semidet.
:- mode previous(in) = out is semidet.
:- mode previous(out) = in is semidet.


:- pred top(lua::in) is semidet.

:- 

:- func [ T | lua ] = lua.
:- mode [ in | in ] = in is semidet.
:- mode [ in | out ] = in is semidet.


%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%


:- typeclass lua(T) where [ 
	% push a value on the top of lua's stack, and then return it.
	pred to_stack(T, lua),
	mode to_stack(in, in) is det,

	some [T] (func from_stack(lua) = T)),
	mode from_stack(in) = out is det,
	mode from_stack(in) = in is semidet ].
	 
	
:- instance lua(lua).

:- pred get(lua, K, V) <= (lua(K), lua(V)).
:- mode get(in, in, in) is semidet.
:- mode get(in, in, out) is semidet.
:- mode get(in, out, in) is nondet.
:- mode get(in, out, out) is nondet.


:- pred set(lua, k










    
%%%%%%%%%%%%%%%%%%
:- implementation.
%%%%%%%%%%%%%%%%%%

:- import_module io.

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
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""


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

	
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);

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

").	

%%%%%%%%%%%%%%%%%
% The Lua State %
%%%%%%%%%%%%%%%%%


:- type lua_state.
:- pragma foreign_type("C", lua_state, "lua_State *").

:- type c_function.
:- pragma foreign_type("C", c_function, "lua_CFunction").

:- type index == int.

:- pred valid_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_index(L, I, !IO) :- I = global ; I = registry ; valid_stack_index(L, I, !IO).

:- pred valid_stack_index(lua_state::in, index::in, io::di, io::uo) is semidet.

valid_stack_index(L, I, !IO) :- I \= 0 , abs(I) =< get_top(L, !IO).


%%%%%%%%%%%%%	
% Lua Types %
%%%%%%%%%%%%%

% TODO type(V) = Type

%%%%%%%%%%%%%%
% Lua Values %
%%%%%%%%%%%%%%

:- typeclass lua(T) where [ 
	% push a value on the top of lua's stack, and then return it.
	pred to_stack(T, lua_state, lua_state),
	mode to_stack(in, mdi, muo) is det,

	some [T] (pred from_stack(int, T, lua_state, lua_state)),
	mode from_stack(in, out, mdi, muo) is det,
	mode from_stack(in, in, mdi, muo) is semidet ].
	
:- type lua --->

	stack(lua_state, index) ;
	key(lua, lua) ;
	global(lua_state, string) ;
	registry(lua_state, string) ;
	scope(lua_state, index) ;
	ref(lua_state, index, int) ;
	upvalue(lua_state, int) ;
	

%%%%%%%%%%%%%
% Refrences %
%%%%%%%%%%%%%


%%%%%%%%%%%%
% Trailing %
%%%%%%%%%%%%


:- pragma require_feature_set([trailing]).


:- pred locked(lua_state::in) is semidet.		
:- pred locked(lua_state::in, lua_state::out) is semidet.

 locked(L, L) :- locked(L).


:- pragma foreign_proc("C", locked(L::in), 
	[will_not_call_mercury, promise_pure], "
	lua_checkstack(L, 1);
	lua_getfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	SUCCESS_INDICATOR = lua_toboolean(L, -1);
	lua_pop(L, 1);").

:- pred lock(lua::in, lua::muo) is det.

:- pragma foreign_proc("C", lock(L::in, L1::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 1);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	L1 = L;
").

:- pred unlock(lua::mdi, lua::out) is det.

:- pragma foreign_proc("C", unlock(L::mdi, L1::out), 
	[will_not_call_mercury, promise_pure], "
	lua_pushboolean(L, 0);
	lua_setfield(L, LUA_REGISTRYINDEX, AP_LOCKED);
	L1 = L;
").

:- pred unlock(lua::mdi) is det.

unlock(L) :- unlock(L, _).

:- pred choicepoint(lua::mdi, lua::muo) is det.

:- pragma foreign_proc("C", choicepoint(L::mdi, L1::muo), 
	[will_not_call_mercury, promise_pure], "
	luaAP_choicepoint(L);
	L1 = L;
").

:- pragma foreign_decl("C", "

	typedef struct lua_Trail {
		lua_State * L;
		int top;
		int ref;
		} lua_Trail;

	void luaAP_choicepoint(lua_State *, int);

	void luaAP_untrail(lua_Trail *, MR_untrail_reason); 
").

:- pragma foreign_code("C", "

	void luaAP_choicepoint(lua_State * L, int ref);
	{
		lua_Trail * t = malloc(sizeof(lua_Trail));
		t.L = L;
		t->ref = ref;
		t->top = lua_gettop(L);
		
		MR_trail_function(luaAP_untrail, t);
	}

	void luaAP_untrail(lua_Trail * t,
        MR_untrail_reason reason)
    {

        switch(reason) 
        {
            case MR_undo:       /* Fall through. */
            case MR_exception:  /* Fall through. */
            case MR_retry:
                lua_State L = t.L;
       			int old = trail->top;
				int top = lua_gettop(L);
				if (old > top)
					MR_fatal_error(
	""Found more values on the Lua stack on backtrack than should be there."");
				
				if (old < top);
				lua_settop(L, old);
				
				

                
                break;

            case MR_solve:  /* Fall through */
            case MR_commit: 
                break;

            default:
                MR_fatal_error(""lua.state.m: unknown MR_untrail_reason"");
        }
    }
").









	


    
