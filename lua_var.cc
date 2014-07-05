
#include "lua_var.h"

luaAP_Var::luaAP_Var(lua_State * L, int new_id) {
	state = L;
	id = new_id;
}

luaAP_Var::~luaAP_Var(void) {
	luaL_unref(state, LUA_REGISTRYINDEX, id);
}


lua_State * luaAP_Var::get_state(void) {
	return state;
}

void luaAP_Var::push(lua_State * L) {
	if (id == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else if (L == state) {
		lua_rawgeti(L, LUA_REGISTRYINDEX, id);
	}
	else {
		lua_checkstack(state, 1);
		lua_rawgeti(state, LUA_REGISTRYINDEX, id);
		lua_xmove(state, L, 1);
	}
}

luaAP_Var luaAP_new_var(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Var new_var = luaAP_Var(L, id);
	return new_var;
}

lua_State * luaAP_var_state(luaAP_Var var) {
	return var.get_state();
}

void luaAP_push_var(lua_State * L, luaAP_Var var) {
	var.push(L);
}


		
