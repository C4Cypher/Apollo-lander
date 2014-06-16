:- module lua.table.

:- interface.


% A lua_var confirmed to be table in lua
:- type lua_table.

:- typeclass lua_table(T) where [
	func lua_table(T) = lua_table,
	mode lua_table(in) = out is det,
	mode lua_table(out) = in is semidet ].

% All operations are pure, no metamethods are to be triggered
:- func to_table(T) = lua_table is semidet.
:- func table_to_var(lua_table) = lua_var is det.
:- func new_table = lua_table is det.

:- func get_metatable(lua_var) = lua_table is semidet.
:- func set_metatable(lua_var, lua_table) = lua_var is semidet.

:- pred get(lua_table::in, lua_var::in, lua_var::out) is det.
:- func get(lua_table, lua_var) = lua_var is det.
:- pred set(lua_table::in, lua_var::in, lua_var::in, lua_table::out) is det.
:- func set(lua_table, lua_var, lua_var) = lua_table is det.

:- func lua_table ^ lua_var = lua_var is det.
:- func lua_table ^ lua_var := lua_var = lua_table is det.

:- pred shallow_copy(lua_table::in, lua_table::out) is det.
:- func shallow_copy(lua_var) = lua_var is det.

:- pred deep_copy(lua_table::in, lua_table::out) is det
:- func deep_copy(lua_table) = lua_table.



:- pred empty_table(lua_table::in) is semidet.
:- func empty_table = lua_table::in is semidet.

% Empty tables produce nil
:- pred first(lua_table::in, lua_var::out) is det.
:- func first(lua_table) = lua_var is det.

:- pred next(lua_table::in, pair(lua_var)::in, pair(lua_var)::out) is semidet.
:- func next(lua_table, pair(lua_var)) = pair(lua_var) is semidet.

:- interface.

