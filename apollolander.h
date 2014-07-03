#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#define AP_TYPE ""luaAPOLLO_LANDER_MERCURY_TYPE""
#define AP_MODULE ""luaAPOLLO_LANDER_MODULE""
#define AP_READY ""luaAPOLLO_LANDER_READY""
#define AP_LOCKED ""luaAPOLLO_LANDER_LOCKED""

#define AP_SELF 1 // Upvalues for function context

/* luaopen function as required by Lua package.load */
int luaopen_apollo_lander(lua_State *);

/* check to see if Apollo has already been initialized. */
int luaAP_apollo_ready(lua_State *); 

/* lua_CFunction that prepares a lua_State for use with Apollo_lander */
int luaAP_init_apollo(lua_State *);
