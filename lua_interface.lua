-- lua_interface.lua

local types = {
	--[[
	["nil"] = "nil",
	integer = "int",
	number = "float",
	boolean = "bool",
	string = "string",
	table = "table",
	[ "function" ] = "function",
	cfunction = "c_function",
	thread = "lua_state",
	lightuserdata = "c_pointer",
	userdata = "T",
	]]
	ref = "ref"
	
}

local function line()
	print(
"\n%-----------------------------------------------------------------------------%\n"
) end

for k,v in pairs(types) do

	print("\t% Value is "..k..".\n\t%")
	print(":- semipure pred is_"..k..
	"(lua::in, int::in) is semidet. \n")
	
	print(":- pragma foreign_proc(\"C\", is_"..k..
	"\t(L::in, Index::in),\n\t[promise_semipure, will_not_call_mercury],\n"..
"\"\n\t SUCCESS_INDICATOR = lua_is"..k.."(L, Index);\n\").\n")
end


line()

for k,v in pairs(types) do

	print("\t% Pull "..k.." value.\n\t%")
	print(":- semipure pred pull_"..k.."(lua::in, index::in, "..v..
		"::out) is det.\n")
		
	print(":- pragma foreign_proc(\"C\", pull_"..k.."(L::in, Index::in, "..
	"V::out),\n\t[promise_semipure, will_not_call_mercury],\n"..
"\"\n\t V = lua_to"..k.."(L, Index);\n\").\n")
end

line()

for k,v in pairs(types) do

	print("\t% Push "..k.." value.\n\t%")
	print(":- impure pred push_"..k.."(lua::in, "..v..
		"::in) is det.\n")

	print(":- pragma foreign_proc(\"C\", push_"..k.."(L::in, V::in),\n"..
	 "\t[will_not_call_mercury],\n"..
"\"\n\t lua_push"..k.."(L, V);\n\").\n")
end






	
	
