:- module lua.value.

:- interface.

:- typeclass lua_value(T) where [
    func to_var(T) = lua_var is det,
    func from_var(lua_var) = T is semidet ].
    
:- implementation.
