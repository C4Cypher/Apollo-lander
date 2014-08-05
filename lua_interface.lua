-- lua_interface.lua

local proc = "foreign_proc"

local types = {
	["nil"] = "nil",
	integer = "int",
	number = "float",
	boolean = "bool",
	string = "string",
	thread = "lua_state_ptr",
	lightuserdata = "c_pointer",
	userdata = "T"
}

print ":- interface.\n\n"

for k,v in pairs(types) do
	print(":- semipure pred is_"..k..
	"(lua_state_ptr::in, int::in) is semidet.")
end

print "\n"

for k,v in pairs(types) do
	print(":- semipure pred pull_"..k.."(lua_state_ptr::in, "..v..
		"::out) is det.")
end

print "\n"

for k,v in pairs(types) do
	print(":- impure pred push_"..k.."(lua_state_ptr::in, "..v..
		"::in) is det.")
end

print "\n"

print ":- implementation.\n\n"


for k,v in pairs(types) do
	print(":- pragma foreign_proc(\"C\", is_"..k..
	"\t(L::in, Index::in),\n [promise_semipure, will_not_call_mercury],\n"..
"\"\n\t SUCCESS_INDICATOR = lua_is"..k.."(L, Index);\n\").")
end

print "\n"

for k,v in pairs(types) do
	print(":- pragma foreign_proc(\"C\", pull_"..k.."(L::in, Index::in, "..
	"V::out),\n [promise_semipure, will_not_call_mercury],\n"..
"\"\n\t V = lua_to"..k.."(L, Index);\n\").")
end

for k,v in pairs(types) do
	print(":- pragma foreign_proc(\"C\", push_"..k.."(L::in, V::in),\n"..
	 "[will_not_call_mercury],\n"..
"\"\n\t lua_push"..k.."(L, V);\n\").")
end

print "\n"



	
	
