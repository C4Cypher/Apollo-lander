/* The luaAP_Var class encapsulates a refrence to a value instantiated in Lua
this object will automatically remove the refrence when collected, allowing
Lua to properly perform garbage collection. */

#ifndef AP_LUA_VAR_H_
#define AP_LUA_VAR_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <gc.h>


/* Creates a new refrence from the stack */
extern luaAP_Var luaAP_new_var(lua_State *);

/* Retreives the refrenced Lua state */
extern lua_State * luaAP_var_state(luaAP_Var);

/* Push a refrence onto the provided stack */
extern void luaAP_push_var(lua_State *, luaAP_Var);


#ifdef __cplusplus
}
#endif

#ifdef AP_LUA_USE_VAR_CLASS

#ifdef __cplusplus

	class luaAP_Var
	{
	public:
		// Constructor
		luaAP_Var(lua_State *, int);

		// Destructor - removes refrence from the registry
		~luaAP_Var(void);

		// Retreives the refrenced Lua state
		lua_State * get_state(void);

		// Pushes the refrenced value onto the stack provided.
		void push(lua_State *);

	protected:

		lua_State * state;
		int id;
	};

#else /* __cplusplus */

	typedef struct luaAP_Var
		luaAP_Var;

#endif /* __cplusplus */


#else /* AP_LUA_USE_VAR_CLASS */

	typedef struct luaAP_Var {
		lua_State * state;
		int id;
	} luaAP_Var;



/* Creates a new refrence from the stack */
luaAP_Var luaAP_new_var(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Var * new_var = GC_MALLOC(sizeof(luaAP_Var));
	new_var->state = L;
	new_var->id = id;
	GC_REGISTER_FINALIZER(new_var, luaAP_finalize_var, NULL, NULL);
	return new_var*;
}

/* Retreives the refrenced Lua state */
lua_State * luaAP_var_state(luaAP_Var var) {
	return var.state;
}

/* Push a refrence onto the provided stack */
void luaAP_push_var(lua_State * L, luaAP_Var var) {
	if (var.id == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else if (L == var.state) {
		lua_rawgeti(L, LUA_REGISTRYINDEX, var.id);
	}
	else {
		lua_checkstack(var.state, 1);
		lua_rawgeti(var.state, LUA_REGISTRYINDEX, id);
		lua_xmove(var.state, L, 1);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaAP_finalize_var(luaAP_Var * var, void * dummy) {
	luaL_unref(state, LUA_REGISTRYINDEX, id);
}

#endif /* AP_LUA_USE_VAR_CLASS */

#endif /* AP_LUA_VAR_H_ */

	
