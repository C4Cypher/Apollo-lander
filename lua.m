:- module lua.

:- interface.

:- import_module list, string, int, float, pair, assoc_list, map, io.

% Refrence to the lua runtime to be passed for lua operations
:- type lua.state.

:- typeclass lua.stack_compatable 
% For transparently handling lua variables on the stack

:- type index --->
  stack(int);
  global(string); 
  environment(string);  
  registry(string);
  upvalue(int).  % Manipulating c function upvalues

%:- type userdata. % Not sure how to implement this, do later after I get the rest working


  
:- implementation.

:- pragma foreign_decl("C", "#include <lua.h>;  #include <lauxlib.h>; #include <lualib.h>;").


