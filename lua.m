:- module lua.

% Lua Syntax and semantics as defined in https://github.com/supranove/prolua/blob/master/implementation.pdf
% thanks to Jeremy OTHIENO for putting this together

:- interface.

:- include_module list, string, int, float, pair.


:- type expression --->  % Lua expression
  variable  ;
  value     .
  

:- type explist == list(expression).


:- type name.  % Lua identifier
:- type namelist == list(name).

:- pred name(string::in) is semidet. % name(S) :- S \= '...'.

:- func name(string) = name.        % name(S) = N :- name(S), S = N.
:- mode name(out) = in is det.
:- mode name(in) = out is semidet.
:- mode name(in) = in is semidet.


:- type parname ---> name ; '...'.  % union of name and '...'
:- type parlist == list(parname).


:- type variable.
:- type varlist == list(variable).


:- type value --->
  nil       ;
  boolean   ;
  number    ;
  string    ;
  table     ;
  function  ;
  thread    .
  
:- type valuelist == list(value).


:- type statement.
:- type statementlist == list(statement).


:- type refrence.   % refrence ---> positive non-zero int

:- pred refrence(int::in) is semidet. % refrence(N) :- N > 0.

:- func refrence(int) = refrence.   % refrence(N) = R :- refrence(N), N = R.
:- mode refrence(out) = in is det.
:- mode refrence(in) = out is semidet.
:- mode refrence(in) = in is semidet.

:- type refrencestack == list(refrence).

:- pred niltype(value::in) is semidet.  % niltype(V) :- V = nil.
:- func niltype(nil) = value is det.


:- type boolean ---> true ; false.

:- pred booleantype(value::in) is semidet.
:- func booleantype(boolean) = value  is det.

:- type number == float ; int.

:- pred numbertype(value::in) is semidet.
:- func numbertype(number) = value is det.


:- pred stringtype(value::in) is semidet.

:- type key ---> name ; expression.

:- pred key(key::in) is semidet. % key(K) :- not niltype(K).

:- type field == pair(key,expression).
:- func key(field) = key is det.            % key(F) = fst(F).
:- func val(field) = expression is det.     % val(F) = snd(F).

:- type fields == list(field).

:- pred fields(fields:in) is semidet. % feilds([First|Rest]) :- key(key(First)), feilds(Rest).


:- type table ---> table(fields).

% refrencetype(N) ??? TODO




