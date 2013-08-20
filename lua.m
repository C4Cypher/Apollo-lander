:- module lua.

% Lua Syntax and semantics as defined in https://github.com/supranove/prolua/blob/master/implementation.pdf
% thanks to Jeremy OTHIENO for putting this together

:- interface.

:- include_module list, string, int, float, pair.

:- typeclass lua(T).

:- type luaversion --->
  c_lua;
  mercury_lua;
  luajit. % TODO, will fail
  
:- type c_lua ---> lua5_1 ; lua5_2.

:- type lua ---> lua(luaversion) <= lua(luaversion).


:- type expression --->  % Lua expression
  variable  ;
  value     .
  

:- type explist == list(expression).

% name(S) = N :- S \= '...', S = N.
:- type name ---> name(string).  % Lua identifier
:- type namelist == list(name).

:- type parname ---> name ; '...'.  % union of name and '...'
:- type parlist == list(parname).


:- type variable ---> variable(name).
:- type varlist == list(variable).


:- type value --->
  nil       ;
  boolean   ;
  number    ;
  string    ;
  table     ;
  function  ;
  thread    .
  

:- type literal --->
  nil       ;
  boolean   ;
  number    ;
  string    ;
  refrence  .
  
:- type chunk == list(statement).

:- type block == chunk.
  
:- type statement --->
  assign(varlist,explist) ;
  functioncall            ;
  do(block)               ;
  while(expression,block) ;
  repeat(block,expression);
  ifstatement;
  for(field,expression,block);
  for(field,expression,expression,block);
  for(namelist,explist,block);
  deffunction(funcname,funcbody);
  ldeffunction(funcname,funcbody);
  local(namelist)         ;
  local(namelist,explist) .
  
:- type ifstatement --->
  if(expression,thenblock).
  
:- type thenblock --->
  block   ;
  else(block,block);
  elseif(block,ifstatement).
  
:- type funcname --->
  name ;
  list(name);
  method(name,name);
  method(list(name),name).
  
:- type funcbody ---> funcbody(parlist,block).
  


% refrence(R) = I :- I > 0, I = R.
:- type refrence ---> refrence(int).   % refrence ---> positive non-zero int

:- type refrencestack == list(refrence).

:- type boolean ---> true ; false.


% number(F) = N :- F = N.
:- type number ---> number(float).

:- type key ---> name ; expression.

:- pred key(key::in) is semidet. % key(K) :- not niltype(K).

:- type field == pair(key,expression).
:- func '='(key,expression) = field is semidet.
:- func key(field) = key is det.            % key(F) = fst(F).
:- func val(field) = expression is det.     % val(F) = snd(F).

:- type fields == list(field).

:- pred fields(fields:in) is semidet. % feilds([First|Rest]) :- key(key(First)), feilds(Rest).


:- type table ---> table(fields).





