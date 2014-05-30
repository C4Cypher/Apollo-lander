:- module lua.

:- interface.

:- import_module list, string, int, float, pair, assoc_list, map, io.

:- type lua_state.

:- type nil.

:- type cfunction.

:- type mfunction == (pred(lua_state::in, list(T)::in, list(T)::out


% For transparently handling lua variables on the stack

:- type index --->
  stack(int);
  global(string); 
  environment(string);  
  registry(string);
  upvalue(int).  % Manipulating c function upvalues

%:- type userdata. % Not sure how to implement this, do later after I get the rest working

% types that can be converted to and be manipulated like lua values
% All calls assume that the indexes called are from the same lua_state
:- typeclass lua_value(T) where [  
  pred peek(int,T::out,lua_state::in) is semidet,
  pred push(T::in,lua_state::in) is semidet ].


:- instance lua_value(nil).
:- instance lua_value(int).
:- instance lua_value(float).
:- instance lua_value(string).
:- instance lua_value(map(K,V)) <= (lua_value(K), lua_value(V)). %For lua tables
:- instance lua_value(list(T)) <= lua_value(T). %For lua array tables

% Standard get/set calls
:- pred get(lua_state::in,T::in,K::in,V::out) <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- func get(lua_state::in,T::in,K::in) = V::out <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- pred get(lua_state::in,index::in,V::out) <= (lua_value(V)) is semidet.
:- func get(lua_state::in,index::in) = V::out <= (lua_value(V)) is semidet.

:- pred set(lua_state::in,T::in,K::in,V::in) <= (lua_value(T), lua_value(K), lua_value(V)) is det.
:- pred set(lua_state::in,index::in,V::in) <= (lua_value(V)) is det.

:- pred rawget(lua_state::in,T::in,K::in,V::out) <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- func rawget(lua_state::in,T::in,K::in) = V::out <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- pred rawget(lua_state::in,index::in,V::out) <= (lua_value(V)) is semidet.
:- func rawget(lua_state::in,index::in) = V::out <= (lua_value(V)) is semidet.

:- pred rawset(lua_state::in,T::in,K::in,V::in) <= (lua_value(T), lua_value(K), lua_value(V)) is det.
:- pred rawset(lua_state::in,index::in,V::in) <= (lua_value(V)) is det.

% Get/set calls with I/O threading
:- pred get(lua_state::in,T::in,K::in,V::out,io::di,io::uo) <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- func get(lua_state::in,T::in,K::in,io::di,io::uo) = V::out <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- pred get(lua_state::in,index::in,V::out,io::di,io::uo) <= (lua_value(V)) is semidet.
:- func get(lua_state::in,index::in,io::di,io::uo) = V::out <= (lua_value(V)) is semidet.

:- pred set(lua_state::in,T::in,K::in,V::in,io::di,io::uo) <= (lua_value(T), lua_value(K), lua_value(V)) is det.
:- pred set(lua_state::in,index::in,V::in,io::di,io::uo) <= (lua_value(V)) is det.

:- pred rawget(lua_state::in,T::in,K::in,V::out,io::di,io::uo) <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- func rawget(lua_state::in,T::in,K::in,io::di,io::uo) = V::out <= (lua_value(T), lua_value(K), lua_value(V)) is semidet.
:- pred rawget(lua_state::in,index::in,V::out,io::di,io::uo) <= (lua_value(V)) is semidet.
:- func rawget(lua_state::in,index::in,io::di,io::uo) = V::out <= (lua_value(V)) is semidet.

:- pred rawset(lua_state::in,T::in,K::in,V::in,io::di,io::uo) <= (lua_value(T), lua_value(K), lua_value(V)) is det.
:- pred rawset(lua_state::in,index::in,V::in,io::di,io::uo) <= (lua_value(V)) is det.

% TODO: rawget and rawset

% Predicates and functions for calling lua functions (or metatabled values) in lua
:- pred call(lua_state::in,T::in,list(T)::in,list(T)::out) <= lua_value(T) is semidet.


%TODO:  calling mercury from lua

:- typeclass userdata(T) where [TODO].  
  
:- implementation.

:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").


