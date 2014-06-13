:- module lua.type.

%%%%  Lua type system  %%%%

/* In Lua, variables are dynamically typed, type is associated with value, not the variable.
There are eight basic types in Lua, one of which is further divided into two variations in C.

%% Value Types - Primitive values

nil	- indicates the abscence of value, used as a return value and as a list 
	terminator

number 	- equivalent to Mercury float or C double, Lua also allows integers to 
	be easily passed as numbers.

boolean	- truth value equivalent to Mercury bool
 
string	- similar to Mercury strings, can be passed as char *
	
%% Refrence Types - Cannot be passed directly, but must be handled by refrence, 
their functionality can be extended with the use of metatables
		
table	- efficient associative array implemented by a hash table
	
function - varadic first class value in lua, lexically scoped with varadic 
	return values

userdata - Lua's type for foreign values
	
thread - Used to handle refrences to the lua_state and coroutines

lightuserdata -	A special case of userdata, it holds a C pointer and cannot 
	have a metatable

*/



:- interface.


	
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


%%%%%%%%%%%%%%%%%%
:- implementation.
%%%%%%%%%%%%%%%%%%

:- import_module io.

% TODO: Implement type/1
type(T) = Type :- Type = none.

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
    

 
