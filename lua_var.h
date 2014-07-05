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

#ifdef __cplusplus
}
#endif

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

#ifdef __cplusplus
extern "C" {
#endif

/* Creates a new refrence from the stack */
extern luaAP_Var luaAP_new_var(lua_State *);

/* Retreives the refrenced Lua state */
extern lua_State * luaAP_var_state(luaAP_Var);

/* Push a refrence onto the provided stack */
extern void luaAP_push_var(lua_State *, luaAP_Var);

#ifdef __cplusplus
}
#endif

#endif /* AP_LUA_VAR_H_ */

	
