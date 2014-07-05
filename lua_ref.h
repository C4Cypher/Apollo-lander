/* The luaAP_Ref class encapsulates a refrence to a value instantiated in Lua
this object will automatically remove the refrence when collected, allowing
Lua to properly perform garbage collection. */

#ifndef AP_LUA_REF_H_
#define AP_LUA_REF_H_

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
	class luaAP_Ref
	{
	public:
		// Constructor
		luaAP_Ref(lua_State *, int);

		// Destructor - removes refrence from the registry
		~luaAP_Ref(void);

		// Retreives the refrenced Lua state
		lua_State * get_state(void);

		// Pushes the refrenced value onto the stack provided.
		void push(lua_State *);

	protected:

		lua_State * state;
		int id;
	};

#else /* __cplusplus */

	typedef struct luaAP_Ref
		luaAP_Ref;

#endif /* __cplusplus */

#ifdef __cplusplus
extern "C" {
#endif

/* Creates a new refrence from the stack */
extern luaAP_Ref luaAP_new_ref(lua_State *);

/* Retreives the refrenced Lua state */
extern lua_State * luaAP_ref_state(luaAP_Ref);

/* Push a refrence onto the provided stack */
extern void luaAP_push_ref(lua_State *, luaAP_Ref);

#ifdef __cplusplus
}
#endif

#endif /* AP_LUA_REF_H_ */

	
