:- module lua.impure.

/* This module exposes some of the raw, impure calls made availible by the
C api in lua.h and lauxlib.h 

WARNING: Do not use the operations in this library unless you are familiar with
Lua's C api, Lua assumes that any operation called via the C API will safely
manage the stack, and will not check for errors. Improper manipulation of the 
stack will not produce comprehensable error messages, instead Lua will allow
itself to become corrupted, resulting in segfaults and other undefined behavior.

*/

:- interface.

:- import_module lua_value.

% Create a new lua state. Equivalent to luaL
:- pred newstate(lua_state::uo) is semidet.
:- func newstate = lua_state::uo is semidet.

/* Types specifying the possible methods for refrencing variables in lua from 
the C api. Comments after each type specify equivelent expressions in lua or a
 description of the equivelent intended C call.  

In the C api, lua values are refrenced by integer indexes, positive integers 
refrence values from the bottom of the stack starting at 1, negative integers 
refrence the stack from the top.  There are also pseudoindexes that allow access
to global variables, values stored in the registry and values stored as upvalues
in C functions.  Be warned, refrencing an invalid index has undefined behavior.
*/

:- type lua_index ---> 
	global(string);	% globalvar = value
	global(string, list(lua_var)); % globalvar[7].foo.bar = value 
	stack(int);	% pushing a value onto the stack, or viewing said value. 
	stack(int, list(lua_var); % accessing a table on the stack
	registry(string); % same as above, but for the C registry	
	registry(string, list(lua_var));
	upvalue(int); % accessing an upvalue from the current C closure 
	upvalue(int, list(lua_var));
	call(lua_index, list(lua_index)) % Call a function and access the result
	ref(lua_ref). % a value created by luaL_ref on the registry
	
% Some calls to Lua do not permit the use of pseudoindexes.
:- type stack_index ---> stack(int).

:- type lua_ref.  % The indexes for lua refrences should be opaque


% Lua variables are dynamic, and thus, untyped, in Lua typing is associated with
% a value that a variable contains, NOT the variable itself.
:- type lua_var --->
	nil;
	string(string);
	boolean(bool);
	number(int);
	number(float);
	table(lua_index); % refrence to a table
	function(c_function);
	function(lua_index); % refrence to a lua function
	userdata(lua_index);
	lightuserdata(c_pointer);		
	index(lua_index). % opaque value

:- func true = bool is det.
:- func false = bool is det.

% Types that can be passed by value freely to and from lua
:- type lua_value --->
	nil;
	string(string);
	boolean(bool);
	number(int);
	number(float);
	function(c_function); % C Functions are passed as pointers
	lightuserdata(c_pointer). 


% Note: Tables, functions and full userdata are refrence types and cannot
% easily be passed by value, refrences must be used.  However, C pointers can
% be passed freely back and forth from Lua, but it can't do much with them.

%-----------------------------------------------------------------------------%
% Standard lua_state manipulation.

:- type ls == lua_state.


% Push variables onto the stack.
:- pred push(lua_var, ls, ls).
:- mode push(di, di, uo) is det.
:- mode push(di, di, uo) is semidet.


% Remove variables from the top of the stack.
:- pred pop(lua_var, ls, ls). 
:- mode pop(uo, di, uo) is semidet.
:- mode pop(uo, di, uo) is det.
:- mode pop(di, uo) is semidet.
:- func pop(ls, ls) = lua_var.
:- mode pop(di, uo) = uo is semidet.
:- mode pop(di, uo) = uo is det.
:- mode pop(di, uo) = di is semidet.

:- pred metatable(lua_index, lua_var, ls, ls).
:- mode metatable(di, uo, di, uo) is semidet.
:- mode metatable(di, di, di, uo) is semidet.
:- func metatable(lua_index, ls, ls) = lua_var.
:- mode metatable(di, di, uo) = uo is semidet.
:- mode metatable(di, di, uo) = di is semidet.

% Place a lua variable in the registry, be sure that created refs are unrefed.
:- pred ref(lua_var, lua_ref, ls, ls).
:- mode ref(di, di, di, uo) is semidet.
:- mode ref(di, uo, di, uo) is det.
:- mode ref(di, uo, di, uo) is semidet.
:- func ref(lua_var, ls, ls) = lua_ref.
:- mode ref(di, di, uo) = di is semidet.
:- mode ref(di, di, uo) = uo is det.
:- mode ref(di, di, uo) = uo is semidet.

:- pred unref(lua_ref::di, ls::di, ls::uo). is det.




% Examine variables in Lua, may cause side effects due to metamethods
% Get calls will leave the stack in the same state it was found
% If the function needs to return a refrence, then the variable will be
% stored in the registry.
:- pred get(lua_index, lua_var, ls, ls).
:- mode get(di, di, di, uo) is semidet.
:- mode get(di, uo, di, uo) is semidet.
:- func get(lua_index, ls, ls) = lua_var.
:- mode get(di, di, uo) = di is semidet.
:- mode get(di, di, uo) = uo is semidet.

% Assign values to lua
:- pred set(lua_index::di, lua_var::di, ls::di, ls::uo).

% equality with metamethods (side effects)
:- pred equals(lua_index::di,lua_index::di, ls::di, ls::uo) is semidet.

% The raw operations access lua without metatables 
:- pred rawget(lua_index, lua_var, ls, ls).
:- mode rawget(di, di, di, uo) is semidet.
:- mode rawget(di, uo, di, uo) is semidet.
:- func rawget(lua_index, ls, ls) = lua_var.
:- mode rawget(di, di, uo) = di is semidet.
:- mode rawget(di, di, uo) = uo is semidet.

% Assign values to lua
:- pred rawset(lua_index::di, lua_var::di, ls::di, ls::uo).

% equality with metamethods (side effects)
:- pred rawequals(lua_index::di,lua_index::di, ls::di, ls::uo) is semidet.

% Unless otherwise documented, the following calls directly call the C api,
% Documentation on what they do can be found in the Lua Refrence Manual.

% This is important for ensuring that there is avalilbe space
% on the stack
:- pred checkstack(int::in, ls::di, ls::uo) is semidet.


% Manipulates the top value (and number of values) on the stack.
:- pred top(int, ls, ls).
:- mode top(in, di, uo) is semidet.
:- mode top(out, di, uo) is det.
:- func top(ls, ls) = int.
:- mode top(di, uo) = in is semidet.
:- mode top(di, uo) = out is det.



:- pred newstate(ls::uo) is semidet. % Equivalent to luaL_newstate
:- func newstate = ls::uo is semidet.
:- pred close(ls::di) is det.
:- pred openlibs(ls::di, ls::uo) is det. % Load the Lua standard libraries.
:- pred loadfile(string::in, ls::in, ls::uo) is semidet.
:- pred loadstring(string::in, ls::in, ls::uo) is semidet.
:- pred dofile(string::in, ls::in, ls::uo) is semidet.
:- pred dostring(string::in, ls::in, ls::uo) is semidet.


:- implementation.

:- type lua_ref ---> ref(int).
