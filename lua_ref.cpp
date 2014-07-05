
#include "lua_ref.h"

luaAP_Ref::luaAP_Ref(lua_State * L, int new_id) {
	state = L;
	id = new_id;
}

luaAP_Ref::~luaAP_Ref(void) {
	luaL_unref(state, LUA_REGISTRYINDEX, id);
}


lua_State * luaAP_Ref::get_state(void) {
	return state;
}

void luaAP_Ref::push(lua_State * L) {
	lua_checkstack(L, 1);
	
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

luaAP_Ref luaAP_new_ref(lua_State * L, int index) {
	int id = luaL_ref(L, LUA_REGISTRYINDEX);
	luaAP_Ref new_ref = luaAP_Ref(L, id);
	return new_ref;
}

lua_State * luaAP_ref_state(luaAP_Ref ref) {
	return ref.get_state();
}

void luaAP_push_ref(lua_State * L, luaAP_Ref ref) {
	ref.push(L);
}


		
