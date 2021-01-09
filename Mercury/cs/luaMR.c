/*
** Automatically generated from `luaMR.m'
** by the Mercury compiler,
** version 20.06.1
** configured for x86_64-unknown-cygwin.
** Do not edit.
**
** The autoconfigured grade settings governing
** the generation of this C file were
**
** TAG_BITS=3
** UNBOXED_FLOAT=yes
** UNBOXED_INT64S=yes
** PREGENERATED_DIST=no
** HIGHLEVEL_CODE=no
**
** END_OF_C_GRADE_INFO
*/

/*
INIT mercury__luaMR__init
ENDINIT
*/

#define MR_ALLOW_RESET
#include "mercury_imp.h"
#line 28 "Mercury/cs/luaMR.c"
#include "array.mh"

#line 31 "Mercury/cs/luaMR.c"
#line 32 "Mercury/cs/luaMR.c"
#include "benchmarking.mh"

#line 35 "Mercury/cs/luaMR.c"
#line 36 "Mercury/cs/luaMR.c"
#include "bitmap.mh"

#line 39 "Mercury/cs/luaMR.c"
#line 40 "Mercury/cs/luaMR.c"
#include "builtin.mh"

#line 43 "Mercury/cs/luaMR.c"
#line 44 "Mercury/cs/luaMR.c"
#include "char.mh"

#line 47 "Mercury/cs/luaMR.c"
#line 48 "Mercury/cs/luaMR.c"
#include "construct.mh"

#line 51 "Mercury/cs/luaMR.c"
#line 52 "Mercury/cs/luaMR.c"
#include "dir.mh"

#line 55 "Mercury/cs/luaMR.c"
#line 56 "Mercury/cs/luaMR.c"
#include "exception.mh"

#line 59 "Mercury/cs/luaMR.c"
#line 60 "Mercury/cs/luaMR.c"
#include "float.mh"

#line 63 "Mercury/cs/luaMR.c"
#line 64 "Mercury/cs/luaMR.c"
#include "int.mh"

#line 67 "Mercury/cs/luaMR.c"
#line 68 "Mercury/cs/luaMR.c"
#include "int16.mh"

#line 71 "Mercury/cs/luaMR.c"
#line 72 "Mercury/cs/luaMR.c"
#include "int32.mh"

#line 75 "Mercury/cs/luaMR.c"
#line 76 "Mercury/cs/luaMR.c"
#include "int64.mh"

#line 79 "Mercury/cs/luaMR.c"
#line 80 "Mercury/cs/luaMR.c"
#include "int8.mh"

#line 83 "Mercury/cs/luaMR.c"
#line 84 "Mercury/cs/luaMR.c"
#include "io.mh"

#line 87 "Mercury/cs/luaMR.c"
#line 88 "Mercury/cs/luaMR.c"
#include "luaMR.mh"

#line 91 "Mercury/cs/luaMR.c"
#line 92 "Mercury/cs/luaMR.c"
#include "math.mh"

#line 95 "Mercury/cs/luaMR.c"
#line 96 "Mercury/cs/luaMR.c"
#include "pretty_printer.mh"

#line 99 "Mercury/cs/luaMR.c"
#line 100 "Mercury/cs/luaMR.c"
#include "private_builtin.mh"

#line 103 "Mercury/cs/luaMR.c"
#line 104 "Mercury/cs/luaMR.c"
#include "rtti_implementation.mh"

#line 107 "Mercury/cs/luaMR.c"
#line 108 "Mercury/cs/luaMR.c"
#include "stm_builtin.mh"

#line 111 "Mercury/cs/luaMR.c"
#line 112 "Mercury/cs/luaMR.c"
#include "store.mh"

#line 115 "Mercury/cs/luaMR.c"
#line 116 "Mercury/cs/luaMR.c"
#include "string.mh"

#line 119 "Mercury/cs/luaMR.c"
#line 120 "Mercury/cs/luaMR.c"
#include "table_builtin.mh"

#line 123 "Mercury/cs/luaMR.c"
#line 124 "Mercury/cs/luaMR.c"
#include "time.mh"

#line 127 "Mercury/cs/luaMR.c"
#line 128 "Mercury/cs/luaMR.c"
#include "trail.mh"

#line 131 "Mercury/cs/luaMR.c"
#line 132 "Mercury/cs/luaMR.c"
#include "type_desc.mh"

#line 135 "Mercury/cs/luaMR.c"
#line 136 "Mercury/cs/luaMR.c"
#include "uint.mh"

#line 139 "Mercury/cs/luaMR.c"
#line 140 "Mercury/cs/luaMR.c"
#include "uint16.mh"

#line 143 "Mercury/cs/luaMR.c"
#line 144 "Mercury/cs/luaMR.c"
#include "uint32.mh"

#line 147 "Mercury/cs/luaMR.c"
#line 148 "Mercury/cs/luaMR.c"
#include "uint64.mh"

#line 151 "Mercury/cs/luaMR.c"
#line 152 "Mercury/cs/luaMR.c"
#include "uint8.mh"

#line 155 "Mercury/cs/luaMR.c"
#line 156 "Mercury/cs/luaMR.c"
#include "version_array.mh"

#line 159 "Mercury/cs/luaMR.c"
#line 160 "Mercury/cs/luaMR.c"
#include "luaMR.api.mh"

#line 163 "Mercury/cs/luaMR.c"
#line 164 "Mercury/cs/luaMR.c"
#include "luaMR.state.mh"

#line 167 "Mercury/cs/luaMR.c"
#line 168 "Mercury/cs/luaMR.c"
#ifndef LUAMR_DECL_GUARD
#define LUAMR_DECL_GUARD

#line 172 "Mercury/cs/luaMR.c"
#line 474 "luaMR.m"

#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>

#include <mercury_memory.h>

/* Checking for Lua language features introduced with 5.2 */
#if LUA_VERSION_NUM >= 502

#define AFTER_502

#else /*  LUA_VERSION_NUM < 502 */ 

#define BEFORE_502

#endif /* LUA_VERSION_NUM < 502 */ 

#ifdef BEFORE_502
#define LUA_RIDX_MAINTHREAD     1
#define LUA_RIDX_GLOBALS        2
#define LUA_RIDX_LAST           LUA_RIDX_GLOBALS
#endif /* BEFORE_502 */

#define LUA_MR_MODULES "LUA_MR_MODULES"
#define LUA_MR_READY "LUA_MR_READY"

/* metatable values*/
#define LUA_MR_TYPE "__mercury_type"
#define LUA_MR_USERDATA "__mercury_userdata"




#line 208 "Mercury/cs/luaMR.c"
#line 567 "luaMR.m"
void luaMR_init(lua_State *);
#line 211 "Mercury/cs/luaMR.c"
#line 569 "luaMR.m"
int luaMR_ready(lua_State *);
#line 214 "Mercury/cs/luaMR.c"
#line 688 "luaMR.m"


	typedef int * luaMR_Ref;
	
	luaMR_Ref luaMR_newref(lua_State *, int);
	void luaMR_pushref(lua_State *, luaMR_Ref);
	void luaMR_finalizeref(lua_State *, luaMR_Ref);

#line 224 "Mercury/cs/luaMR.c"
#line 1050 "luaMR.m"

	size_t luaMR_len(lua_State *, int);

#line 229 "Mercury/cs/luaMR.c"
#line 1076 "luaMR.m"

	void luaMR_getregistry(lua_State *, const char *);
	void luaMR_setregistry(lua_State *, const char *);
	int  luaMR_getupvalue(lua_State *, const int);
	void luaMR_setupvalue(lua_State *, const int);

#line 237 "Mercury/cs/luaMR.c"
#line 1136 "luaMR.m"

	int luaMR_loader(lua_State *);

#line 242 "Mercury/cs/luaMR.c"
#line 1186 "luaMR.m"

	MR_Word * luaMR_new(MR_Word);
	int luaMR_free(lua_State *);

#line 248 "Mercury/cs/luaMR.c"
#line 249 "Mercury/cs/luaMR.c"

#endif
#line 252 "Mercury/cs/luaMR.c"

#ifdef _MSC_VER
#define MR_STATIC_LINKAGE extern
#else
#define MR_STATIC_LINKAGE static
#endif


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_0 {
	MR_Word * f1[2];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_0 mercury_common_0[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_1 {
	MR_Word * f1;
	MR_Integer f2;
	MR_Word * f3;
	MR_Word * f4;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_1 mercury_common_1[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_2 {
	MR_Word * f1[3];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_2 mercury_common_2[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_3 {
	MR_Word * f1[4];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_3 mercury_common_3[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_4 {
	MR_Word * f1[5];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_4 mercury_common_4[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_5 {
	MR_Word * f1[2];
	MR_Integer f2;
	MR_Word * f3[3];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_5 mercury_common_5[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_6 {
	MR_Word * f1;
	MR_Code * f2;
	MR_Integer f3;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_6 mercury_common_6[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_7 {
	MR_Word * f1;
	MR_Word * f2;
	MR_Integer f3;
	MR_Word * f4;
	MR_Word * f5;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_7 mercury_common_7[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_8 {
	MR_Word * f1;
	MR_Integer f2;
	MR_Word * f3[4];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_8 mercury_common_8[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_9 {
	MR_Integer f1;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_9 mercury_common_9[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_10 {
	MR_Word * f1[2];
	MR_Integer f2;
	MR_Word * f3[4];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_10 mercury_common_10[];

extern const MR_TypeCtorInfo_Struct
	mercury_data_luaMR__type_ctor_info_c_function_0,
	mercury_data_luaMR__type_ctor_info_error_type_0,
	mercury_data_luaMR__type_ctor_info_index_0,
	mercury_data_luaMR__type_ctor_info_ls_0,
	mercury_data_luaMR__type_ctor_info_lua_0,
	mercury_data_luaMR__type_ctor_info_lua_error_0,
	mercury_data_luaMR__type_ctor_info_lua_state_0,
	mercury_data_luaMR__type_ctor_info_lua_type_0,
	mercury_data_luaMR__type_ctor_info_mr_func_0,
	mercury_data_luaMR__type_ctor_info_nil_0;

extern const MR_TypeCtorInfo_Struct
	mercury_data_luaMR__type_ctor_info_ref_0,
	mercury_data_luaMR__type_ctor_info_value_0,
	mercury_data_luaMR__type_ctor_info_values_0,
	mercury_data_luaMR__type_ctor_info_var_0,
	mercury_data_luaMR__type_ctor_info_vars_0;
MR_decl_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0, 1,2)
MR_decl_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0, 1,2)
MR_decl_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0, 1,2)
MR_decl_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0, 1,2)
MR_decl_label6(luaMR__call_lua_func_5_0, 2,3,4,5,7,9)
MR_decl_label3(luaMR__get_4_0, 2,4,5)
MR_decl_label3(luaMR__get_4_1, 2,4,5)
MR_decl_label1(luaMR__local_3_0, 3)
MR_decl_label2(luaMR__local_3_1, 3,5)
MR_decl_label1(luaMR__local_table_3_0, 2)
MR_decl_label1(luaMR__ready_1_0, 1)
MR_decl_label1(luaMR__ready_2_0, 1)
MR_decl_label1(luaMR__ready_3_0, 3)
MR_decl_label1(luaMR__ready_3_1, 3)
MR_decl_label2(luaMR__ref_table_3_0, 2,4)
MR_decl_label7(luaMR__return_args_3_0, 2,6,7,4,8,9,10)
MR_decl_label10(luaMR__set_4_0, 4,2,8,9,10,11,6,15,16,17)
MR_decl_label10(luaMR__set_4_0, 13,22,24,25,21,27,28,19,35,36)
MR_decl_label9(luaMR__set_4_0, 37,33,40,41,42,38,46,47,48)
MR_decl_label2(luaMR__string_to_func_4_0, 2,4)
MR_decl_label2(luaMR__valid_var_3_0, 2,1)
MR_decl_label2(luaMR__value_equal_4_0, 2,1)
MR_decl_label2(luaMR__var_equal_4_0, 2,1)
MR_decl_label1(luaMR__var_type_4_0, 2)
MR_decl_label1(luaMR__var_type_4_1, 2)
MR_decl_label3(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0, 2,3,4)
MR_decl_label4(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0, 3,4,5,2)
MR_decl_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0, 2,4)
MR_decl_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0, 2,4)
MR_decl_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0, 2,4)
MR_decl_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0, 2,4)
MR_decl_label1(fn__f_108_117_97_77_82_95_95_94_2_0, 2)
MR_decl_label1(fn__luaMR__call_lua_func_4_0, 2)
MR_decl_label1(fn__luaMR__get_3_0, 2)
MR_decl_label1(fn__luaMR__get_3_1, 2)
MR_decl_label1(fn__luaMR__get_args_2_0, 2)
MR_decl_label1(fn__luaMR__local_2_0, 2)
MR_decl_label1(fn__luaMR__local_2_1, 2)
MR_decl_label1(fn__luaMR__local_table_2_0, 2)
MR_decl_label1(fn__luaMR__pop_one_1_0, 2)
MR_decl_label1(fn__luaMR__ref_table_2_0, 2)
MR_decl_label1(fn__luaMR__string_to_func_3_0, 2)
MR_decl_label7(fn__luaMR__to_string_1_0, 3,7,8,9,10,11,12)
MR_decl_label10(fn__luaMR__value_1_0, 3,2,7,6,11,10,15,14,19,18)
MR_decl_label10(fn__luaMR__value_1_0, 23,22,27,29,26,32,31,36,35,40)
MR_decl_label6(fn__luaMR__value_1_0, 39,44,43,48,47,51)
MR_decl_label10(fn__luaMR__value_1_1, 15,18,21,9,3,24,12,49,33,32)
MR_decl_label5(fn__luaMR__value_1_1, 35,6,27,39,1)
MR_decl_label1(fn__luaMR__var_type_3_0, 2)
MR_decl_label1(fn__luaMR__var_type_3_1, 2)
MR_decl_label2(__Unify___luaMR__lua_error_0_0, 3,1)
MR_decl_label10(__Unify___luaMR__value_0_0, 3,17,21,23,11,5,25,13,46,7)
MR_decl_label3(__Unify___luaMR__value_0_0, 27,35,1)
MR_decl_label9(__Unify___luaMR__var_0_0, 3,15,7,9,17,21,5,13,1)
MR_decl_label2(__Compare___luaMR__error_type_0_0, 2,3)
MR_decl_label2(__Compare___luaMR__index_0_0, 2,3)
MR_decl_label7(__Compare___luaMR__lua_error_0_0, 5,22,6,10,11,24,12)
MR_decl_label2(__Compare___luaMR__lua_type_0_0, 2,3)
MR_decl_label10(__Compare___luaMR__value_0_0, 99,271,121,128,141,142,150,151,152,166)
MR_decl_label10(__Compare___luaMR__value_0_0, 52,56,57,76,5,7,8,29,167,176)
MR_decl_label10(__Compare___luaMR__value_0_0, 177,291,178,192,77,82,98,266,236,30)
MR_decl_label9(__Compare___luaMR__value_0_0, 51,33,193,214,203,237,258,312,249)
MR_decl_label10(__Compare___luaMR__var_0_0, 63,69,24,27,29,30,81,87,96,103)
MR_decl_label10(__Compare___luaMR__var_0_0, 5,7,8,14,44,46,48,49,125,50)
MR_def_extern_entry(luaMR__new_state_1_0)
MR_def_extern_entry(fn__luaMR__new_state_0_0)
MR_def_extern_entry(luaMR__init_lua_3_0)
MR_def_extern_entry(luaMR__init_lua_2_0)
MR_def_extern_entry(luaMR__ready_1_0)
MR_def_extern_entry(luaMR__ready_3_0)
MR_def_extern_entry(luaMR__ready_3_1)
MR_def_extern_entry(luaMR__ready_2_0)
MR_def_extern_entry(fn__f_108_117_97_77_82_95_95_94_2_0)
MR_def_extern_entry(luaMR__valid_var_3_0)
MR_def_extern_entry(luaMR__var_equal_4_0)
MR_def_extern_entry(fn__luaMR__value_1_0)
MR_def_extern_entry(fn__luaMR__value_1_1)
MR_def_extern_entry(fn__luaMR__value_of_1_0)
MR_def_extern_entry(luaMR__value_equal_4_0)
MR_def_extern_entry(luaMR__get_4_0)
MR_def_extern_entry(luaMR__get_4_1)
MR_def_extern_entry(fn__luaMR__get_3_0)
MR_def_extern_entry(fn__luaMR__get_3_1)
MR_def_extern_entry(luaMR__set_4_0)
MR_def_extern_entry(luaMR__local_3_0)
MR_def_extern_entry(luaMR__local_3_1)
MR_def_extern_entry(fn__luaMR__local_2_0)
MR_def_extern_entry(fn__luaMR__local_2_1)
MR_def_extern_entry(fn__luaMR__local_table_2_0)
MR_def_extern_entry(luaMR__local_table_3_0)
MR_def_extern_entry(fn__luaMR__ref_table_2_0)
MR_def_extern_entry(luaMR__ref_table_3_0)
MR_def_extern_entry(fn__luaMR__make_lua_func_1_0)
MR_def_extern_entry(fn__luaMR__make_lua_func_1_1)
MR_def_extern_entry(fn__luaMR__make_nondet_lua_func_1_0)
MR_def_extern_entry(fn__luaMR__make_nondet_lua_func_1_1)
MR_def_extern_entry(fn__luaMR__make_nondet_lua_func_1_2)
MR_def_extern_entry(fn__luaMR__make_nondet_lua_func_1_3)
MR_def_extern_entry(luaMR__string_to_func_4_0)
MR_def_extern_entry(fn__luaMR__string_to_func_3_0)
MR_def_extern_entry(luaMR__call_lua_func_5_0)
MR_def_extern_entry(fn__luaMR__call_lua_func_4_0)
MR_def_extern_entry(luaMR__var_type_4_0)
MR_def_extern_entry(luaMR__var_type_4_1)
MR_def_extern_entry(fn__luaMR__var_type_3_0)
MR_def_extern_entry(fn__luaMR__var_type_3_1)
MR_def_extern_entry(luaMR__init_lua_1_0)
MR_def_extern_entry(fn__luaMR__pop_one_1_0)
MR_def_extern_entry(fn__luaMR__get_args_2_0)
MR_def_extern_entry(luaMR__return_args_3_0)
MR_def_extern_entry(fn__luaMR__to_string_1_0)
MR_def_extern_entry(__Unify___luaMR__c_function_0_0)
MR_def_extern_entry(__Compare___luaMR__c_function_0_0)
MR_def_extern_entry(__Unify___luaMR__error_type_0_0)
MR_def_extern_entry(__Compare___luaMR__error_type_0_0)
MR_def_extern_entry(__Unify___luaMR__index_0_0)
MR_def_extern_entry(__Compare___luaMR__index_0_0)
MR_def_extern_entry(__Unify___luaMR__ls_0_0)
MR_def_extern_entry(__Compare___luaMR__ls_0_0)
MR_def_extern_entry(__Unify___luaMR__lua_0_0)
MR_def_extern_entry(__Compare___luaMR__lua_0_0)
MR_def_extern_entry(__Unify___luaMR__lua_error_0_0)
MR_def_extern_entry(__Compare___luaMR__lua_error_0_0)
MR_def_extern_entry(__Unify___luaMR__lua_state_0_0)
MR_def_extern_entry(__Compare___luaMR__lua_state_0_0)
MR_def_extern_entry(__Unify___luaMR__lua_type_0_0)
MR_def_extern_entry(__Compare___luaMR__lua_type_0_0)
MR_def_extern_entry(__Unify___luaMR__mr_func_0_0)
MR_def_extern_entry(__Compare___luaMR__mr_func_0_0)
MR_def_extern_entry(__Unify___luaMR__nil_0_0)
MR_def_extern_entry(__Compare___luaMR__nil_0_0)
MR_def_extern_entry(__Unify___luaMR__ref_0_0)
MR_def_extern_entry(__Compare___luaMR__ref_0_0)
MR_def_extern_entry(__Unify___luaMR__value_0_0)
MR_def_extern_entry(__Compare___luaMR__value_0_0)
MR_def_extern_entry(__Unify___luaMR__values_0_0)
MR_def_extern_entry(__Compare___luaMR__values_0_0)
MR_def_extern_entry(__Unify___luaMR__var_0_0)
MR_def_extern_entry(__Compare___luaMR__var_0_0)
MR_def_extern_entry(__Unify___luaMR__vars_0_0)
MR_def_extern_entry(__Compare___luaMR__vars_0_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0)
MR_decl_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0)
MR_decl_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0)
MR_decl_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0)
MR_decl_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0)
MR_decl_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0)

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_ops__op_table__arity1__ops__mercury_op_table__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_ops__type_ctor_info_mercury_op_table_0;
extern const MR_TypeCtorInfo_Struct mercury_data_list__type_ctor_info_list_1;
extern const MR_TypeCtorInfo_Struct mercury_data_list__type_ctor_info_list_1;
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_var_0;
static const struct mercury_type_0 mercury_common_0[5] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_ops__op_table__arity1__ops__mercury_op_table__arity0__,
MR_CTOR0_ADDR(ops, mercury_op_table)
}
},
{
{
MR_LIST_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, value)
}
},
{
{
MR_LIST_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
MR_LIST_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
MR_LIST_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, var)
}
},
};

extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_func_0;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_int_0;
static const struct mercury_type_1 mercury_common_1[1] =
{
{
MR_CTOR0_ADDR(builtin, func),
2,
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
},
};

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__stream__arity2__io__output_stream__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_io__type_ctor_info_output_stream_0;
extern const MR_TypeCtorInfo_Struct mercury_data_io__type_ctor_info_state_0;
static const struct mercury_type_2 mercury_common_2[1] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__stream__arity2__io__output_stream__arity0__io__state__arity0__,
MR_CTOR0_ADDR(io, output_stream),
MR_IO_CTOR_ADDR
}
},
};

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__output__arity2__io__output_stream__arity0__io__state__arity0__[];
static const struct mercury_type_3 mercury_common_3[1] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__output__arity2__io__output_stream__arity0__io__state__arity0__,
MR_TAG_COMMON(0,2,0),
MR_CTOR0_ADDR(io, output_stream),
MR_IO_CTOR_ADDR
}
},
};

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__string__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_string_0;
extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__character__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_character_0;
static const struct mercury_type_4 mercury_common_4[2] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__string__arity0__io__state__arity0__,
MR_TAG_COMMON(0,3,0),
MR_CTOR0_ADDR(io, output_stream),
MR_STRING_CTOR_ADDR,
MR_IO_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__character__arity0__io__state__arity0__,
MR_TAG_COMMON(0,3,0),
MR_CTOR0_ADDR(io, output_stream),
MR_CHAR_CTOR_ADDR,
MR_IO_CTOR_ADDR
}
},
};

static const MR_UserClosureId
mercury_data__closure_layout__luaMR__get_4_0_1;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_int_0;
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_0;
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_var_0;
static const MR_UserClosureId
mercury_data__closure_layout__luaMR__get_4_1_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_lua_func_1_0_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_lua_func_1_1_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_0_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_1_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_2_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_3_1;
static const struct mercury_type_5 mercury_common_5[8] =
{
{
{
(MR_Word *) &mercury_data__closure_layout__luaMR__get_4_0_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_INT_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__luaMR__get_4_1_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_INT_CTOR_ADDR,
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_lua_func_1_0_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,0),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_lua_func_1_1_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,0),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_0_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,1),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_1_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,1),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_2_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,1),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_3_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_COMMON(8,1),
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
}
},
};

MR_decl_entry(fn__luaMR__api__to_refvar_2_0);
static const struct mercury_type_6 mercury_common_6[3] =
{
{
MR_COMMON(5,0),
MR_ENTRY_AP(fn__luaMR__api__to_refvar_2_0),
0
},
{
MR_COMMON(5,1),
MR_ENTRY_AP(fn__luaMR__api__to_refvar_2_0),
0
},
{
MR_COMMON(7,0),
MR_ENTRY_AP(fn__luaMR__pop_one_1_0),
0
},
};

static const MR_UserClosureId
mercury_data__closure_layout__luaMR__local_3_1_1;
static const struct mercury_type_7 mercury_common_7[1] =
{
{
(MR_Word *) &mercury_data__closure_layout__luaMR__local_3_1_1,
((MR_Word *) (MR_Integer) 0),
2,
MR_CTOR0_ADDR(luaMR, lua),
MR_INT_CTOR_ADDR
},
};

extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_func_0;
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_state_0;
static const struct mercury_type_8 mercury_common_8[2] =
{
{
MR_CTOR0_ADDR(builtin, func),
4,
{
MR_COMMON(0,3),
MR_CTOR0_ADDR(luaMR, lua_state),
MR_CTOR0_ADDR(luaMR, lua_state),
MR_COMMON(0,3)
}
},
{
MR_CTOR0_ADDR(builtin, func),
4,
{
MR_COMMON(0,3),
MR_CTOR0_ADDR(luaMR, lua_state),
MR_CTOR0_ADDR(luaMR, lua_state),
MR_CTOR0_ADDR(luaMR, var)
}
},
};

static const struct mercury_type_9 mercury_common_9[1] =
{
{
1
},
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0_1;
static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0_1;
static const struct mercury_type_10 mercury_common_10[4] =
{
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0_1,
((MR_Word *) (MR_Integer) 0)
},
4,
{
MR_COMMON(8,1),
MR_COMMON(0,4),
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0_1,
((MR_Word *) (MR_Integer) 0)
},
4,
{
MR_COMMON(8,1),
MR_COMMON(0,4),
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0_1,
((MR_Word *) (MR_Integer) 0)
},
4,
{
MR_COMMON(8,1),
MR_COMMON(0,4),
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
{
{
(MR_Word *) &mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0_1,
((MR_Word *) (MR_Integer) 0)
},
4,
{
MR_COMMON(8,1),
MR_COMMON(0,4),
MR_CTOR0_ADDR(luaMR, lua),
MR_CTOR0_ADDR(luaMR, var)
}
},
};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_c_function_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__c_function_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__c_function_0_0)),
	"luaMR",
	"c_function",
	{ 0 },
	{ 0 },
	-1,
	0,
	NULL
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_0 = {
	"no_error",
	0,
	0
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_1 = {
	"runtime_error",
	1,
	LUA_ERRRUN
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_2 = {
	"syntax_error",
	2,
	LUA_ERRSYNTAX
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_3 = {
	"memory_error",
	3,
	LUA_ERRMEM
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_4 = {
	"unhandled_error",
	4,
	LUA_ERRERR
};

const MR_ForeignEnumFunctorDescPtr mercury_data_luaMR__foreign_enum_ordinal_ordered_error_type_0[] = {
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_0,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_1,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_2,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_3,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_4
};

const MR_ForeignEnumFunctorDescPtr mercury_data_luaMR__foreign_enum_name_ordered_error_type_0[] = {
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_3,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_0,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_1,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_2,
	&mercury_data_luaMR__foreign_enum_functor_desc_error_type_0_4
};

const MR_Integer mercury_data_luaMR__functor_number_map_error_type_0[] = {
	1,
	2,
	3,
	0,
	4
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_error_type_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN_ENUM,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__error_type_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__error_type_0_0)),
	"luaMR",
	"error_type",
	{ (void *) mercury_data_luaMR__foreign_enum_name_ordered_error_type_0 },
	{ (void *) mercury_data_luaMR__foreign_enum_ordinal_ordered_error_type_0 },
	5,
	4,
	mercury_data_luaMR__functor_number_map_error_type_0
};
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_int_0;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_int_0;

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_index_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_EQUIV_GROUND,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__index_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__index_0_0)),
	"luaMR",
	"index",
	{ 0 },
	{ (void *) &mercury_data_builtin__type_ctor_info_int_0 },
	-1,
	0,
	NULL
};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_ls_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_EQUIV_GROUND,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__ls_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__ls_0_0)),
	"luaMR",
	"ls",
	{ 0 },
	{ (void *) &mercury_data_luaMR__type_ctor_info_lua_state_0 },
	-1,
	0,
	NULL
};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__lua_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__lua_0_0)),
	"luaMR",
	"lua",
	{ 0 },
	{ 0 },
	-1,
	0,
	NULL
};
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_string_0;
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_error_type_0;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_string_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_lua_error_0_0[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_error_type_0,
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_string_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_lua_error_0_0 = {
	"lua_error",
	2,
	0,
	MR_SECTAG_NONE,
	0,
	-1,
	0,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_lua_error_0_0,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_lua_error_0_0[] = {
	&mercury_data_luaMR__du_functor_desc_lua_error_0_0

};

const MR_DuPtagLayout mercury_data_luaMR__du_ptag_ordered_lua_error_0[] = {
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_lua_error_0_0,
	-1 }

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_name_ordered_lua_error_0[] = {
	&mercury_data_luaMR__du_functor_desc_lua_error_0_0
};

const MR_Integer mercury_data_luaMR__functor_number_map_lua_error_0[] = {
	0
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_error_0 = {
	0,
	17,
	1,
	MR_TYPECTOR_REP_DU,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__lua_error_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__lua_error_0_0)),
	"luaMR",
	"lua_error",
	{ (void *) mercury_data_luaMR__du_name_ordered_lua_error_0 },
	{ (void *) mercury_data_luaMR__du_ptag_ordered_lua_error_0 },
	1,
	4,
	mercury_data_luaMR__functor_number_map_lua_error_0
};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_state_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__lua_state_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__lua_state_0_0)),
	"luaMR",
	"lua_state",
	{ 0 },
	{ 0 },
	-1,
	0,
	NULL
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_0 = {
	"none",
	0,
	LUA_TNONE
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_1 = {
	"nil_type",
	1,
	LUA_TNIL
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_2 = {
	"number_type",
	2,
	LUA_TNUMBER
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_3 = {
	"boolean_type",
	3,
	LUA_TBOOLEAN
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_4 = {
	"string_type",
	4,
	LUA_TSTRING
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_5 = {
	"lightuserdata_type",
	5,
	LUA_TLIGHTUSERDATA
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_6 = {
	"function_type",
	6,
	LUA_TFUNCTION
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_7 = {
	"table_type",
	7,
	LUA_TTABLE
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_8 = {
	"thread_type",
	8,
	LUA_TTHREAD
};

static const MR_ForeignEnumFunctorDesc mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_9 = {
	"userdata_type",
	9,
	LUA_TUSERDATA
};

const MR_ForeignEnumFunctorDescPtr mercury_data_luaMR__foreign_enum_ordinal_ordered_lua_type_0[] = {
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_0,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_1,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_2,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_3,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_4,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_5,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_6,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_7,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_8,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_9
};

const MR_ForeignEnumFunctorDescPtr mercury_data_luaMR__foreign_enum_name_ordered_lua_type_0[] = {
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_3,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_6,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_5,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_1,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_0,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_2,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_4,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_7,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_8,
	&mercury_data_luaMR__foreign_enum_functor_desc_lua_type_0_9
};

const MR_Integer mercury_data_luaMR__functor_number_map_lua_type_0[] = {
	4,
	3,
	5,
	0,
	6,
	2,
	1,
	7,
	8,
	9
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_type_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN_ENUM,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__lua_type_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__lua_type_0_0)),
	"luaMR",
	"lua_type",
	{ (void *) mercury_data_luaMR__foreign_enum_name_ordered_lua_type_0 },
	{ (void *) mercury_data_luaMR__foreign_enum_ordinal_ordered_lua_type_0 },
	10,
	4,
	mercury_data_luaMR__functor_number_map_lua_type_0
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_0;

static const MR_VA_TypeInfo_Struct2 mercury_data___vti_func_2luaMR__type_ctor_info_lua_0builtin__type_ctor_info_int_0 = {
	&mercury_data_builtin__type_ctor_info_func_0,
	2,
{	(MR_TypeInfo) &mercury_data_luaMR__type_ctor_info_lua_0,
	(MR_TypeInfo) &mercury_data_builtin__type_ctor_info_int_0
}};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_mr_func_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_EQUIV_GROUND,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__mr_func_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__mr_func_0_0)),
	"luaMR",
	"mr_func",
	{ 0 },
	{ (void *) &mercury_data___vti_func_2luaMR__type_ctor_info_lua_0builtin__type_ctor_info_int_0 },
	-1,
	0,
	NULL
};

static const MR_EnumFunctorDesc mercury_data_luaMR__enum_functor_desc_nil_0_0 = {
	"nil",
	0
};

const MR_EnumFunctorDescPtr mercury_data_luaMR__enum_value_ordered_nil_0[] = {
	&mercury_data_luaMR__enum_functor_desc_nil_0_0
};

const MR_EnumFunctorDescPtr mercury_data_luaMR__enum_name_ordered_nil_0[] = {
	&mercury_data_luaMR__enum_functor_desc_nil_0_0
};

const MR_Integer mercury_data_luaMR__functor_number_map_nil_0[] = {
	0
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_nil_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_DUMMY,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__nil_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__nil_0_0)),
	"luaMR",
	"nil",
	{ (void *) mercury_data_luaMR__enum_name_ordered_nil_0 },
	{ (void *) mercury_data_luaMR__enum_value_ordered_nil_0 },
	1,
	4,
	mercury_data_luaMR__functor_number_map_nil_0
};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_ref_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__ref_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__ref_0_0)),
	"luaMR",
	"ref",
	{ 0 },
	{ 0 },
	-1,
	0,
	NULL
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_nil_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_0[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_nil_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_0 = {
	"nil",
	1,
	0,
	MR_SECTAG_NONE,
	1,
	-1,
	0,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_0,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_float_0;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_float_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_1[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_float_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_1 = {
	"number",
	1,
	0,
	MR_SECTAG_NONE,
	2,
	-1,
	1,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_1,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_2[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_int_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_2 = {
	"integer",
	1,
	0,
	MR_SECTAG_NONE,
	3,
	-1,
	2,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_2,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_bool__type_ctor_info_bool_0;
extern const MR_TypeCtorInfo_Struct mercury_data_bool__type_ctor_info_bool_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_3[] = {
	(MR_PseudoTypeInfo) &mercury_data_bool__type_ctor_info_bool_0
};

const MR_DuArgLocn mercury_data_luaMR__field_locns_value_0_3[] = {
	{ 0, 0, 1 },
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_3 = {
	"boolean",
	1,
	0,
	MR_SECTAG_NONE,
	4,
	-1,
	3,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_3,
	NULL,
	mercury_data_luaMR__field_locns_value_0_3,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_4[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_string_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_4 = {
	"string",
	1,
	0,
	MR_SECTAG_NONE,
	5,
	-1,
	4,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_4,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_c_pointer_0;
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_c_pointer_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_5[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_c_pointer_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_5 = {
	"lightuserdata",
	1,
	0,
	MR_SECTAG_NONE,
	6,
	-1,
	5,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_5,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_6[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_lua_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_6 = {
	"thread",
	1,
	0,
	MR_SECTAG_REMOTE_FULL_WORD,
	7,
	0,
	6,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_6,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_c_function_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_7[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_c_function_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_7 = {
	"c_function",
	1,
	0,
	MR_SECTAG_REMOTE_FULL_WORD,
	7,
	1,
	7,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_7,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_8[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_var_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_8 = {
	"var",
	1,
	0,
	MR_SECTAG_REMOTE_FULL_WORD,
	7,
	2,
	8,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_8,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_univ__type_ctor_info_univ_0;
extern const MR_TypeCtorInfo_Struct mercury_data_univ__type_ctor_info_univ_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_9[] = {
	(MR_PseudoTypeInfo) &mercury_data_univ__type_ctor_info_univ_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_9 = {
	"userdata",
	1,
	0,
	MR_SECTAG_REMOTE_FULL_WORD,
	7,
	3,
	9,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_9,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_lua_error_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_value_0_10[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_lua_error_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_value_0_10 = {
	"lua_error",
	1,
	0,
	MR_SECTAG_NONE_DIRECT_ARG,
	0,
	-1,
	10,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_value_0_10,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_0[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_10

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_1[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_0

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_2[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_1

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_3[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_2

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_4[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_3

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_5[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_4

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_6[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_5

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_value_0_7[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_6,
	&mercury_data_luaMR__du_functor_desc_value_0_7,
	&mercury_data_luaMR__du_functor_desc_value_0_8,
	&mercury_data_luaMR__du_functor_desc_value_0_9

};

const MR_DuPtagLayout mercury_data_luaMR__du_ptag_ordered_value_0[] = {
	{ 1, MR_SECTAG_NONE_DIRECT_ARG,
	mercury_data_luaMR__du_stag_ordered_value_0_0,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_1,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_2,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_3,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_4,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_5,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_value_0_6,
	-1 },
	{ 4, MR_SECTAG_REMOTE_FULL_WORD,
	mercury_data_luaMR__du_stag_ordered_value_0_7,
	-1 }

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_name_ordered_value_0[] = {
	&mercury_data_luaMR__du_functor_desc_value_0_3,
	&mercury_data_luaMR__du_functor_desc_value_0_7,
	&mercury_data_luaMR__du_functor_desc_value_0_2,
	&mercury_data_luaMR__du_functor_desc_value_0_5,
	&mercury_data_luaMR__du_functor_desc_value_0_10,
	&mercury_data_luaMR__du_functor_desc_value_0_0,
	&mercury_data_luaMR__du_functor_desc_value_0_1,
	&mercury_data_luaMR__du_functor_desc_value_0_4,
	&mercury_data_luaMR__du_functor_desc_value_0_6,
	&mercury_data_luaMR__du_functor_desc_value_0_9,
	&mercury_data_luaMR__du_functor_desc_value_0_8
};

const MR_Integer mercury_data_luaMR__functor_number_map_value_0[] = {
	5,
	6,
	2,
	0,
	7,
	3,
	8,
	1,
	10,
	9,
	4
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_value_0 = {
	0,
	17,
	8,
	MR_TYPECTOR_REP_DU,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__value_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__value_0_0)),
	"luaMR",
	"value",
	{ (void *) mercury_data_luaMR__du_name_ordered_value_0 },
	{ (void *) mercury_data_luaMR__du_ptag_ordered_value_0 },
	11,
	4,
	mercury_data_luaMR__functor_number_map_value_0
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_value_0;

static const MR_FA_TypeInfo_Struct1 mercury_data_list__ti_list_1luaMR__type_ctor_info_value_0 = {
	&mercury_data_list__type_ctor_info_list_1,
{	(MR_TypeInfo) &mercury_data_luaMR__type_ctor_info_value_0
}};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_values_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_EQUIV_GROUND,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__values_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__values_0_0)),
	"luaMR",
	"values",
	{ 0 },
	{ (void *) &mercury_data_list__ti_list_1luaMR__type_ctor_info_value_0 },
	-1,
	0,
	NULL
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_0[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_int_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_0 = {
	"local",
	1,
	0,
	MR_SECTAG_NONE,
	0,
	-1,
	0,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_0,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_1[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_value_0,
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_var_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_1 = {
	"index",
	2,
	0,
	MR_SECTAG_NONE,
	1,
	-1,
	1,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_1,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_2[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_var_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_2 = {
	"meta",
	1,
	0,
	MR_SECTAG_NONE,
	2,
	-1,
	2,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_2,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};
extern const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_ref_0;

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_3[] = {
	(MR_PseudoTypeInfo) &mercury_data_luaMR__type_ctor_info_ref_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_3 = {
	"ref",
	1,
	0,
	MR_SECTAG_NONE,
	3,
	-1,
	3,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_3,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_4[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_string_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_4 = {
	"global",
	1,
	0,
	MR_SECTAG_NONE,
	4,
	-1,
	4,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_4,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_PseudoTypeInfo mercury_data_luaMR__field_types_var_0_5[] = {
	(MR_PseudoTypeInfo) &mercury_data_builtin__type_ctor_info_string_0
};

static const MR_DuFunctorDesc mercury_data_luaMR__du_functor_desc_var_0_5 = {
	"invalid",
	1,
	0,
	MR_SECTAG_NONE,
	5,
	-1,
	5,
	(MR_PseudoTypeInfo *) mercury_data_luaMR__field_types_var_0_5,
	NULL,
	NULL,
	NULL,
	MR_FUNCTOR_SUBTYPE_NONE,
	0
};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_0[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_0

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_1[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_1

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_2[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_2

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_3[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_3

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_4[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_4

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_stag_ordered_var_0_5[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_5

};

const MR_DuPtagLayout mercury_data_luaMR__du_ptag_ordered_var_0[] = {
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_0,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_1,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_2,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_3,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_4,
	-1 },
	{ 1, MR_SECTAG_NONE,
	mercury_data_luaMR__du_stag_ordered_var_0_5,
	-1 }

};

const MR_DuFunctorDescPtr mercury_data_luaMR__du_name_ordered_var_0[] = {
	&mercury_data_luaMR__du_functor_desc_var_0_4,
	&mercury_data_luaMR__du_functor_desc_var_0_1,
	&mercury_data_luaMR__du_functor_desc_var_0_5,
	&mercury_data_luaMR__du_functor_desc_var_0_0,
	&mercury_data_luaMR__du_functor_desc_var_0_2,
	&mercury_data_luaMR__du_functor_desc_var_0_3
};

const MR_Integer mercury_data_luaMR__functor_number_map_var_0[] = {
	3,
	1,
	4,
	5,
	0,
	2
};
	
const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_var_0 = {
	0,
	17,
	6,
	MR_TYPECTOR_REP_DU,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__var_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__var_0_0)),
	"luaMR",
	"var",
	{ (void *) mercury_data_luaMR__du_name_ordered_var_0 },
	{ (void *) mercury_data_luaMR__du_ptag_ordered_var_0 },
	6,
	4,
	mercury_data_luaMR__functor_number_map_var_0
};

static const MR_FA_TypeInfo_Struct1 mercury_data_list__ti_list_1luaMR__type_ctor_info_var_0 = {
	&mercury_data_list__type_ctor_info_list_1,
{	(MR_TypeInfo) &mercury_data_luaMR__type_ctor_info_var_0
}};

const MR_TypeCtorInfo_Struct mercury_data_luaMR__type_ctor_info_vars_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_EQUIV_GROUND,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___luaMR__vars_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___luaMR__vars_0_0)),
	"luaMR",
	"vars",
	{ 0 },
	{ (void *) &mercury_data_list__ti_list_1luaMR__type_ctor_info_var_0 },
	-1,
	0,
	NULL
};


static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0_1 = {
{
MR_PREDICATE,
"luaMR",
"luaMR",
"lambda4_luaMR_m_991",
4,
0
},
"luaMR",
"luaMR.m",
991,
"10"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0_1 = {
{
MR_PREDICATE,
"luaMR",
"luaMR",
"lambda3_luaMR_m_991",
4,
0
},
"luaMR",
"luaMR.m",
991,
"10"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0_1 = {
{
MR_PREDICATE,
"luaMR",
"luaMR",
"lambda2_luaMR_m_991",
4,
0
},
"luaMR",
"luaMR.m",
991,
"10"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0_1 = {
{
MR_PREDICATE,
"luaMR",
"luaMR",
"lambda_luaMR_m_991",
4,
0
},
"luaMR",
"luaMR.m",
991,
"10"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_3_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda4_luaMR_m_988",
3,
0
},
"luaMR",
"luaMR.m",
988,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_2_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda3_luaMR_m_988",
3,
0
},
"luaMR",
"luaMR.m",
988,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_1_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda2_luaMR_m_988",
3,
0
},
"luaMR",
"luaMR.m",
988,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_nondet_lua_func_1_0_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda_luaMR_m_988",
3,
0
},
"luaMR",
"luaMR.m",
988,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_lua_func_1_1_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda_luaMR_m_977",
3,
0
},
"luaMR",
"luaMR.m",
971,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__fn__luaMR__make_lua_func_1_0_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"lambda_luaMR_m_971",
3,
0
},
"luaMR",
"luaMR.m",
971,
"3"
};

static const MR_UserClosureId
mercury_data__closure_layout__luaMR__local_3_1_1 = {
{
MR_FUNCTION,
"luaMR",
"luaMR",
"pop_one",
2,
0
},
"luaMR",
"luaMR.m",
882,
"17"
};

static const MR_UserClosureId
mercury_data__closure_layout__luaMR__get_4_1_1 = {
{
MR_FUNCTION,
"luaMR.api",
"luaMR.api",
"to_refvar",
3,
0
},
"luaMR",
"Mercury/opts/luaMR.api.opt",
209,
"5"
};

static const MR_UserClosureId
mercury_data__closure_layout__luaMR__get_4_0_1 = {
{
MR_FUNCTION,
"luaMR.api",
"luaMR.api",
"to_refvar",
3,
0
},
"luaMR",
"Mercury/opts/luaMR.api.opt",
209,
"5"
};

#line 571 "luaMR.m"

void luaMR_init(lua_State * L) 
{
	
	int length;
	
#ifdef BEFORE_502
	
	/* Set the Main thread in the registry */
	lua_pushvalue(L, LUA_REGISTRYINDEX);
	lua_pushinteger(L, LUA_RIDX_MAINTHREAD); 
	if(!lua_pushthread(L))
		MR_fatal_error("Must init main thread.");
	lua_settable(L, -3);
	
	lua_pushinteger(L, LUA_RIDX_GLOBALS);
	lua_pushvalue(L, LUA_GLOBALSINDEX);
	lua_settable(L, -3);
 
 	
#endif /* BEFORE_502 */

	/* Add tables to the registry. */
	
	lua_newtable(L);
	luaMR_setregistry(L, LUA_MR_MODULES);
	
	/* Add loader to package.loaders */
	lua_getglobal(L, "package"); 
  lua_getfield(L, -1, "loaders"); 
  length = luaMR_len(L, 1);
	lua_pushinteger(L, length + 1); 
	lua_pushcfunction(L, luaMR_loader); 
	lua_settable(L, -3);
	lua_pop(L, 2);
	
	/* Mark Lua as ready */
	lua_pushboolean(L, 1);
	luaMR_setregistry(L, LUA_MR_READY);
} 


#line 2183 "Mercury/cs/luaMR.c"
#line 616 "luaMR.m"

	/* Check to see if Lua has already been initialized. */
	int luaMR_ready(lua_State * L) {
    int ready;
	
		lua_checkstack(L, 1);
		lua_pushvalue(L, LUA_REGISTRYINDEX);
		lua_pushstring(L, LUA_MR_READY);
		lua_gettable(L, -2);
		ready = lua_toboolean(L, 1);
		lua_remove(L, 1);
		return ready;
	}

#line 2199 "Mercury/cs/luaMR.c"
#line 697 "luaMR.m"


/* Creates a new refrence from the stack */
luaMR_Ref luaMR_newref(lua_State * L, int index) {
  luaMR_Ref new_ref;
	lua_pushvalue(L, index);
	new_ref = MR_GC_NEW(int);
	*new_ref = luaL_ref(L, LUA_REGISTRYINDEX);
	MR_GC_register_finalizer(new_ref, 
		(GC_finalization_proc)luaMR_finalizeref, L);
	return new_ref;
}


/* Push a refrence onto the provided stack */
void luaMR_pushref(lua_State * L, luaMR_Ref ref) {
	if (*ref == LUA_REFNIL) {
		lua_pushnil(L);
	}
	else {
		lua_rawgeti(L, LUA_REGISTRYINDEX, *ref);
	}
}

/* Remove Lua's refrence to the var in the registry */
void luaMR_finalizeref(lua_State * L, luaMR_Ref ref) {
	luaL_unref(L, LUA_REGISTRYINDEX, *ref);
}


#line 2231 "Mercury/cs/luaMR.c"
#line 1054 "luaMR.m"


size_t luaMR_len(lua_State * L, int index) {
	
#ifdef BEFORE_502
	return lua_objlen(L, index);
#else 
	return lua_rawlen(L, index);
#endif /* END BEFORE_502 */
}


#line 2245 "Mercury/cs/luaMR.c"
#line 1084 "luaMR.m"

	void luaMR_getregistry(lua_State * L, const char * k) {
		lua_getfield(L, LUA_REGISTRYINDEX, k);
	}

	void luaMR_setregistry(lua_State * L, const char * k) {
		lua_setfield(L, LUA_REGISTRYINDEX, k);
	}

	int luaMR_getupvalue(lua_State * L, const int id) {
		lua_pushvalue(L, lua_upvalueindex(id));
		if (lua_type(L, -1) == LUA_TNONE) {
			lua_pop(L, 1);
			return 0;
		} else {
			return 1;
		}
	}

	void luaMR_setupvalue(lua_State * L, const int id) {
		lua_replace(L, lua_upvalueindex(id));
	}


#line 2271 "Mercury/cs/luaMR.c"
#line 1140 "luaMR.m"


	/* take the provided module name and attempt to load an apollo module
	passes any additional arguments. */
	int luaMR_loader(lua_State * L) {
		if (lua_isstring(L, 1)) {
			const char * module_name = lua_tostring(L, 1);
			luaMR_getregistry(L, LUA_MR_MODULES);
			lua_getfield(L, 2, module_name);
			return 1;
		}
		return 0;
	}


#line 2288 "Mercury/cs/luaMR.c"
#line 1191 "luaMR.m"


	MR_Word * luaMR_new(MR_Word word) {
		MR_Word * newptr = MR_GC_malloc_uncollectable(sizeof newptr);
		*newptr = word;
		return newptr; 
	}
	
	int luaMR_free(lua_State * L) {
		MR_Word ** ptr = lua_touserdata(L, 1);
		MR_GC_free(*ptr);
		return 0;
	}
		

#line 2305 "Mercury/cs/luaMR.c"


MR_BEGIN_MODULE(luaMR_module0)
	MR_init_entry1(luaMR__new_state_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__new_state_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'new_state'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__new_state_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__new_state_1_0
	MR_OBTAIN_GLOBAL_LOCK("lua_new");
{
#line 134 "Mercury/opts/luaMR.api.opt"

	void * ptr = MR_malloc(sizeof(ptr));
	L = lua_newstate((lua_Alloc)luaMR_alloc, ptr);
	luaL_openlibs(L);
	luaMR_init(L);
	;}
#line 2333 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_new");
	MR_r2 = (MR_Word) L;
#undef	MR_PROC_LABEL
	}
	{
	MR_Word MR_tempr1;
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__luaMR__new_state_1_0
{
#line 45 "Mercury/opts/trail.opt"

    Id = MR_null_choicepoint_id();
;}
#line 2348 "Mercury/cs/luaMR.c"
	MR_tempr1 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__new_state_1_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("lua_state");
{
#line 133 "Mercury/opts/luaMR.state.opt"

	
	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2372 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_state");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_proceed();
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module1)
	MR_init_entry1(fn__luaMR__new_state_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__new_state_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'new_state'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__new_state_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__fn__luaMR__new_state_0_0
	MR_OBTAIN_GLOBAL_LOCK("lua_new");
{
#line 134 "Mercury/opts/luaMR.api.opt"

	void * ptr = MR_malloc(sizeof(ptr));
	L = lua_newstate((lua_Alloc)luaMR_alloc, ptr);
	luaL_openlibs(L);
	luaMR_init(L);
	;}
#line 2411 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_new");
	MR_r2 = (MR_Word) L;
#undef	MR_PROC_LABEL
	}
	{
	MR_Word MR_tempr1;
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__fn__luaMR__new_state_0_0
{
#line 45 "Mercury/opts/trail.opt"

    Id = MR_null_choicepoint_id();
;}
#line 2426 "Mercury/cs/luaMR.c"
	MR_tempr1 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__new_state_0_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("lua_state");
{
#line 133 "Mercury/opts/luaMR.state.opt"

	
	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2450 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_state");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_proceed();
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module2)
	MR_init_entry1(luaMR__init_lua_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__init_lua_3_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'init_lua'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__init_lua_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__init_lua_3_0
	L = (lua_State *) MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("init_lua");
{
#line 541 "luaMR.m"
luaMR_init(L);;}
#line 2485 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("init_lua");
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module3)
	MR_init_entry1(luaMR__init_lua_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__init_lua_2_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'init_lua'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__init_lua_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__init_lua_2_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 2530 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__init_lua_2_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2558 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__init_lua_2_0
	L = (lua_State *) MR_r2;
	MR_save_registers();
	MR_OBTAIN_GLOBAL_LOCK("init_lua");
{
#line 550 "luaMR.m"
luaMR_init(L);;}
#line 2573 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("init_lua");
#ifndef MR_CONSERVATIVE_GC
	MR_restore_registers();
#endif
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_sv(1);
	MR_decr_sp_and_return(2);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module4)
	MR_init_entry1(luaMR__ready_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__ready_1_0);
	MR_init_label1(luaMR__ready_1_0,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ready'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__ready_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__ready_1_0
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	L = (lua_State *) MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("ready");
{
#line 554 "luaMR.m"

	SUCCESS_INDICATOR = luaMR_ready(L);
;}
#line 2617 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ready");
if (!MercurySuccessIndicator) MR_GOTO_LAB(luaMR__ready_1_0_i1);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(luaMR__ready_1_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module5)
	MR_init_entry1(luaMR__ready_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__ready_3_0);
	MR_init_label1(luaMR__ready_3_0,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ready'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__ready_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 2668 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r3 = (MR_Word) L;
	MR_r4 = (MR_Word) I;
	MR_r5 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_0
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_r4;
	T = MR_r5;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2696 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(2));
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_0
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	L = (lua_State *) MR_r3;
	MR_OBTAIN_GLOBAL_LOCK("ready");
{
#line 554 "luaMR.m"

	SUCCESS_INDICATOR = luaMR_ready(L);
;}
#line 2715 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ready");
if (!MercurySuccessIndicator) MR_GOTO_LAB(luaMR__ready_3_0_i3);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_reset_ticket(MR_sv(2), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(2);
	MR_proceed();
MR_def_label(luaMR__ready_3_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(2), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module6)
	MR_init_entry1(luaMR__ready_3_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__ready_3_1);
	MR_init_label1(luaMR__ready_3_1,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ready'/3 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__ready_3_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_1
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 2772 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r3 = (MR_Word) L;
	MR_r4 = (MR_Word) I;
	MR_r5 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_1
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_r4;
	T = MR_r5;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2800 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(2));
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__ready_3_1
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	L = (lua_State *) MR_r3;
	MR_OBTAIN_GLOBAL_LOCK("ready");
{
#line 554 "luaMR.m"

	SUCCESS_INDICATOR = luaMR_ready(L);
;}
#line 2819 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ready");
if (!MercurySuccessIndicator) MR_GOTO_LAB(luaMR__ready_3_1_i3);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_reset_ticket(MR_sv(2), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(2);
	MR_proceed();
MR_def_label(luaMR__ready_3_1,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(2), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module7)
	MR_init_entry1(luaMR__ready_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__ready_2_0);
	MR_init_label1(luaMR__ready_2_0,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ready'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__ready_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_2_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("lua_state");
{
#line 121 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 2875 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_state");
	MR_r3 = (MR_Word) L;
	MR_r4 = (MR_Word) I;
	MR_r5 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ready_2_0
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_r4;
	T = MR_r5;
	MR_OBTAIN_GLOBAL_LOCK("lua_state");
{
#line 133 "Mercury/opts/luaMR.state.opt"

	
	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 2902 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_state");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__ready_2_0
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	L = (lua_State *) MR_r3;
	MR_OBTAIN_GLOBAL_LOCK("ready");
{
#line 554 "luaMR.m"

	SUCCESS_INDICATOR = luaMR_ready(L);
;}
#line 2920 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ready");
if (!MercurySuccessIndicator) MR_GOTO_LAB(luaMR__ready_2_0_i1);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(luaMR__ready_2_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module8)
	MR_init_entry1(fn__f_108_117_97_77_82_95_95_94_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__f_108_117_97_77_82_95_95_94_2_0);
	MR_init_label1(fn__f_108_117_97_77_82_95_95_94_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '^'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__f_108_117_97_77_82_95_95_94_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_sv(1) = MR_r2;
	MR_r2 = MR_r3;
	MR_np_call_localret_ent(fn__luaMR__value_1_0,
		fn__f_108_117_97_77_82_95_95_94_2_0_i2);
MR_def_label(fn__f_108_117_97_77_82_95_95_94_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 1, (MR_Integer) 2);
	MR_tfield(1, MR_r2, 0) = MR_r1;
	MR_tfield(1, MR_r2, 1) = MR_sv(1);
	MR_r1 = MR_r2;
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__valid_var_2_0);

MR_BEGIN_MODULE(luaMR_module9)
	MR_init_entry1(luaMR__valid_var_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__valid_var_3_0);
	MR_init_label2(luaMR__valid_var_3_0,2,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'valid_var'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__valid_var_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__valid_var_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 3009 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__valid_var_3_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 3037 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__valid_var_2_0,
		luaMR__valid_var_3_0_i2);
MR_def_label(luaMR__valid_var_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(luaMR__valid_var_3_0_i1);
	}
	MR_r2 = MR_sv(1);
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(luaMR__valid_var_3_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__var_equal_3_0);

MR_BEGIN_MODULE(luaMR_module10)
	MR_init_entry1(luaMR__var_equal_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__var_equal_4_0);
	MR_init_label2(luaMR__var_equal_4_0,2,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'var_equal'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__var_equal_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_equal_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r3, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 3104 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r3 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_equal_4_0
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 3132 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__var_equal_3_0,
		luaMR__var_equal_4_0_i2);
MR_def_label(luaMR__var_equal_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(luaMR__var_equal_4_0_i1);
	}
	MR_r2 = MR_sv(1);
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(luaMR__var_equal_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(private_builtin__typed_unify_2_1);
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_float_0;
MR_decl_entry(string__char_to_string_2_0);
MR_decl_entry(builtin__dynamic_cast_2_0);
MR_decl_entry(fn__univ__univ_1_0);

MR_BEGIN_MODULE(luaMR_module11)
	MR_init_entry1(fn__luaMR__value_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__value_1_0);
	MR_init_label10(fn__luaMR__value_1_0,3,2,7,6,11,10,15,14,19,18)
	MR_init_label10(fn__luaMR__value_1_0,23,22,27,29,26,32,31,36,35,40)
	MR_init_label6(fn__luaMR__value_1_0,39,44,43,48,47,51)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'value'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__value_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(3));
	MR_sv(1) = MR_r2;
	MR_sv(2) = MR_r1;
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(luaMR, nil));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i3);
MR_def_label(fn__luaMR__value_1_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i2);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 1, (MR_Integer) 1);
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_FLOAT_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i7);
MR_def_label(fn__luaMR__value_1_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i6);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 2, (MR_Integer) 1);
	MR_tfield(2, MR_r1, 0) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_INT_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i11);
MR_def_label(fn__luaMR__value_1_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i10);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	{
	MR_Integer	IntVal;
	MR_Float	FloatVal;
#define	MR_PROC_LABEL	mercury__fn__luaMR__value_1_0
	IntVal = MR_r2;
{
#line 76 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/float.opt"

    FloatVal = IntVal;
;}
#line 3250 "Mercury/cs/luaMR.c"
	MR_r2 = MR_float_to_word(FloatVal);
#undef	MR_PROC_LABEL
	}
	MR_tag_alloc_heap(MR_r1, 2, (MR_Integer) 1);
	MR_tfield(2, MR_r1, 0) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_INT_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i15);
MR_def_label(fn__luaMR__value_1_0,15)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i14);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 3, (MR_Integer) 1);
	MR_tfield(3, MR_r1, 0) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,14)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_BOOL_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i19);
MR_def_label(fn__luaMR__value_1_0,19)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i18);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 4, (MR_Integer) 1);
	MR_tfield(4, MR_r1, 0) = ((MR_Unsigned) MR_r2);
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,18)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_STRING_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i23);
MR_def_label(fn__luaMR__value_1_0,23)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i22);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 5, (MR_Integer) 1);
	MR_tfield(5, MR_r1, 0) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,22)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CHAR_CTOR_ADDR);
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i27);
MR_def_label(fn__luaMR__value_1_0,27)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i26);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = MR_r2;
	MR_np_call_localret_ent(string__char_to_string_2_0,
		fn__luaMR__value_1_0_i29);
MR_def_label(fn__luaMR__value_1_0,29)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 5, (MR_Integer) 1);
	MR_tfield(5, MR_r2, 0) = MR_r1;
	MR_r1 = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,26)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(builtin, c_pointer));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i32);
MR_def_label(fn__luaMR__value_1_0,32)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i31);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 6, (MR_Integer) 1);
	MR_tfield(6, MR_r1, 0) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,31)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(luaMR, lua));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i36);
MR_def_label(fn__luaMR__value_1_0,36)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i35);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 7, (MR_Integer) 2);
	MR_tfield(7, MR_r1, 0) = (MR_Unsigned) 0U;
	MR_tfield(7, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,35)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(luaMR, c_function));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i40);
MR_def_label(fn__luaMR__value_1_0,40)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i39);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 7, (MR_Integer) 2);
	MR_tfield(7, MR_r1, 0) = (MR_Unsigned) 1U;
	MR_tfield(7, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,39)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_0_i44);
MR_def_label(fn__luaMR__value_1_0,44)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i43);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 7, (MR_Integer) 2);
	MR_tfield(7, MR_r1, 0) = (MR_Unsigned) 2U;
	MR_tfield(7, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,43)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	MR_r1 = MR_sv(2);
	MR_r2 = ((MR_Word) MR_CTOR0_ADDR(univ, univ));
	MR_r3 = MR_sv(1);
	MR_np_call_localret_ent(builtin__dynamic_cast_2_0,
		fn__luaMR__value_1_0_i48);
MR_def_label(fn__luaMR__value_1_0,48)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_0_i47);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 7, (MR_Integer) 2);
	MR_tfield(7, MR_r1, 0) = (MR_Unsigned) 3U;
	MR_tfield(7, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_0,47)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_np_call_localret_ent(fn__univ__univ_1_0,
		fn__luaMR__value_1_0_i51);
MR_def_label(fn__luaMR__value_1_0,51)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 7, (MR_Integer) 2);
	MR_tfield(7, MR_r2, 0) = (MR_Unsigned) 3U;
	MR_tfield(7, MR_r2, 1) = MR_r1;
	MR_r1 = MR_r2;
	MR_decr_sp_and_return(4);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_void_0;
MR_decl_entry(univ__type_to_univ_2_2);

MR_BEGIN_MODULE(luaMR_module12)
	MR_init_entry1(fn__luaMR__value_1_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__value_1_1);
	MR_init_label10(fn__luaMR__value_1_1,15,18,21,9,3,24,12,49,33,32)
	MR_init_label5(fn__luaMR__value_1_1,35,6,27,39,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'value'/2 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__value_1_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_r2)),
		MR_LABEL_AP(fn__luaMR__value_1_1_i15) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i18) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i21) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i9) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i3) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i24) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i12) MR_AND
		MR_LABEL_AP(fn__luaMR__value_1_1_i49));
MR_def_label(fn__luaMR__value_1_1,15)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, lua_error));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,18)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, nil));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,21)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_FLOAT_CTOR_ADDR);
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(2, MR_tempr2, 0);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_INT_CTOR_ADDR);
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(3, MR_tempr2, 0);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_BOOL_CTOR_ADDR);
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = ((((MR_Unsigned) MR_tfield(4, MR_tempr2, 0)) >> (MR_Integer) 0) & 1);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,24)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_STRING_CTOR_ADDR);
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(5, MR_tempr2, 0);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,12)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(builtin, c_pointer));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(6, MR_tempr2, 0);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,49)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_r2, 0),0)) {
		MR_GOTO_LAB(fn__luaMR__value_1_1_i27);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_r2, 0),1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_1_i6);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_r2, 0),2)) {
		MR_GOTO_LAB(fn__luaMR__value_1_1_i39);
	}
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_r3 = MR_tfield(7, MR_r2, 1);
	MR_store_ticket(MR_sv(3));
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r3;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(univ, univ));
	MR_r2 = MR_sv(1);
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		fn__luaMR__value_1_1_i33);
MR_def_label(fn__luaMR__value_1_1,33)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_1_i32);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = MR_TRUE;
	MR_decr_sp_and_return(4);
MR_def_label(fn__luaMR__value_1_1,32)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_sv(2);
	MR_sv(2) = ((MR_Word) MR_CTOR0_ADDR(builtin, void));
	MR_r1 = MR_sv(2);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(univ__type_to_univ_2_2,
		fn__luaMR__value_1_1_i35);
MR_def_label(fn__luaMR__value_1_1,35)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__value_1_1_i1);
	}
	MR_r1 = MR_sv(2);
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r2;
	MR_r2 = MR_sv(1);
	MR_r3 = MR_tempr1;
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, c_function));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(7, MR_tempr2, 1);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,27)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, lua));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(7, MR_tempr2, 1);
	MR_np_tailcall_ent(private_builtin__typed_unify_2_1);
	}
MR_def_label(fn__luaMR__value_1_1,39)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tfield(7, MR_tempr2, 1);
	MR_np_tailcall_ent(builtin__dynamic_cast_2_0);
	}
MR_def_label(fn__luaMR__value_1_1,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_decr_sp_and_return(4);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module13)
	MR_init_entry1(fn__luaMR__value_of_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__value_of_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'value_of'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__value_of_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(fn__luaMR__value_1_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__value_equal_3_0);

MR_BEGIN_MODULE(luaMR_module14)
	MR_init_entry1(luaMR__value_equal_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__value_equal_4_0);
	MR_init_label2(luaMR__value_equal_4_0,2,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'value_equal'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__value_equal_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__value_equal_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r3, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 3729 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r3 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__value_equal_4_0
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 3757 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__value_equal_3_0,
		luaMR__value_equal_4_0_i2);
MR_def_label(luaMR__value_equal_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(luaMR__value_equal_4_0_i1);
	}
	MR_r2 = MR_sv(1);
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(luaMR__value_equal_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_succip_word = MR_sv(2);
	MR_decr_sp(2);
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__push_var_2_0);
MR_decl_entry(fn__luaMR__api__to_value_3_0);
MR_decl_entry(luaMR__api__lua_pop_2_0);

MR_BEGIN_MODULE(luaMR_module15)
	MR_init_entry1(luaMR__get_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__get_4_0);
	MR_init_label3(luaMR__get_4_0,2,4,5)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'get'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__get_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__get_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 3826 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__get_4_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 3854 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	MR_sv(2) = MR_r2;
	}
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__get_4_0_i2);
MR_def_label(luaMR__get_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) -1;
	MR_r2 = ((MR_Word) MR_TAG_COMMON(0,6,0));
	MR_r3 = MR_sv(2);
	MR_np_call_localret_ent(fn__luaMR__api__to_value_3_0,
		luaMR__get_4_0_i4);
MR_def_label(luaMR__get_4_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_sv(2);
	MR_sv(2) = MR_r1;
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__get_4_0_i5);
MR_def_label(luaMR__get_4_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module16)
	MR_init_entry1(luaMR__get_4_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__get_4_1);
	MR_init_label3(luaMR__get_4_1,2,4,5)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'get'/4 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__get_4_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__get_4_1
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 3928 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__get_4_1
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 3956 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	MR_sv(2) = MR_r2;
	}
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__get_4_1_i2);
MR_def_label(luaMR__get_4_1,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) -1;
	MR_r2 = ((MR_Word) MR_TAG_COMMON(0,6,1));
	MR_r3 = MR_sv(2);
	MR_np_call_localret_ent(fn__luaMR__api__to_value_3_0,
		luaMR__get_4_1_i4);
MR_def_label(luaMR__get_4_1,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_sv(2);
	MR_sv(2) = MR_r1;
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__get_4_1_i5);
MR_def_label(luaMR__get_4_1,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module17)
	MR_init_entry1(fn__luaMR__get_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__get_3_0);
	MR_init_label1(fn__luaMR__get_3_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'get'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__get_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__get_4_0,
		fn__luaMR__get_3_0_i2);
MR_def_label(fn__luaMR__get_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module18)
	MR_init_entry1(fn__luaMR__get_3_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__get_3_1);
	MR_init_label1(fn__luaMR__get_3_1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'get'/4 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__get_3_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__get_4_1,
		fn__luaMR__get_3_1_i2);
MR_def_label(fn__luaMR__get_3_1,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__push_value_2_0);
MR_decl_entry(luaMR__api__lua_replace_2_0);
MR_decl_entry(luaMR__api__lua_rawset_2_0);
MR_decl_entry(luaMR__api__lua_setmetatable_2_0);
MR_decl_entry(luaMR__api__lua_pushinteger_2_0);
MR_decl_entry(string__append_3_2);
MR_decl_entry(luaMR__api__lua_pushstring_2_0);
MR_decl_entry(fn__f_115_116_114_105_110_103_95_95_43_43_2_0);
MR_decl_entry(exception__throw_1_0);

MR_BEGIN_MODULE(luaMR_module19)
	MR_init_entry1(luaMR__set_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__set_4_0);
	MR_init_label10(luaMR__set_4_0,4,2,8,9,10,11,6,15,16,17)
	MR_init_label10(luaMR__set_4_0,13,22,24,25,21,27,28,19,35,36)
	MR_init_label9(luaMR__set_4_0,37,33,40,41,42,38,46,47,48)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'set'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__set_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(7);
	MR_sv(7) = ((MR_Word) MR_succip);
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__set_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r3, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 4107 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r3 = (MR_Word) L;
	MR_r4 = (MR_Word) I;
	MR_r5 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__set_4_0
	L = (lua_State *) MR_r3;
	I = (MR_ChoicepointId) MR_r4;
	T = MR_r5;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 4135 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r4);
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),0)) {
		MR_GOTO_LAB(luaMR__set_4_0_i2);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r4;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r3;
	MR_sv(4) = MR_tempr1;
	MR_sv(3) = MR_tfield(0, MR_r1, 0);
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i4);
MR_def_label(luaMR__set_4_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_replace_2_0,
		luaMR__set_4_0_i37);
MR_def_label(luaMR__set_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),1)) {
		MR_GOTO_LAB(luaMR__set_4_0_i6);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r4;
	MR_sv(3) = MR_r2;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r3;
	MR_sv(4) = MR_tempr1;
	MR_sv(5) = MR_tfield(1, MR_r1, 0);
	MR_r1 = MR_tfield(1, MR_r1, 1);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__set_4_0_i8);
MR_def_label(luaMR__set_4_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(5);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i9);
MR_def_label(luaMR__set_4_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i10);
MR_def_label(luaMR__set_4_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) -3;
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_rawset_2_0,
		luaMR__set_4_0_i11);
MR_def_label(luaMR__set_4_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__set_4_0_i37);
MR_def_label(luaMR__set_4_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),2)) {
		MR_GOTO_LAB(luaMR__set_4_0_i13);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r4;
	MR_sv(3) = MR_r2;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r3;
	MR_sv(4) = MR_tempr1;
	MR_r1 = MR_tfield(2, MR_r1, 0);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__set_4_0_i15);
MR_def_label(luaMR__set_4_0,15)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i16);
MR_def_label(luaMR__set_4_0,16)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) -2;
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_setmetatable_2_0,
		luaMR__set_4_0_i17);
MR_def_label(luaMR__set_4_0,17)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__set_4_0_i37);
MR_def_label(luaMR__set_4_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),3)) {
		MR_GOTO_LAB(luaMR__set_4_0_i19);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_store_ticket(MR_sv(6));
	MR_sv(1) = MR_r4;
	MR_sv(3) = MR_r2;
	MR_sv(4) = MR_r3;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, ref));
	MR_r2 = ((MR_Word) MR_INT_CTOR_ADDR);
	MR_r3 = MR_tfield(3, MR_tempr1, 0);
	}
	MR_np_call_localret_ent(private_builtin__typed_unify_2_1,
		luaMR__set_4_0_i22);
MR_def_label(luaMR__set_4_0,22)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(luaMR__set_4_0_i21);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_r1 = MR_r2;
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_pushinteger_2_0,
		luaMR__set_4_0_i24);
MR_def_label(luaMR__set_4_0,24)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i25);
MR_def_label(luaMR__set_4_0,25)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Integer	I;
#define	MR_PROC_LABEL	mercury__luaMR__set_4_0
	MR_OBTAIN_GLOBAL_LOCK("registryindex");
{
#line 198 "Mercury/opts/luaMR.api.opt"
I = LUA_REGISTRYINDEX;;}
#line 4297 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("registryindex");
	MR_r1 = I;
#undef	MR_PROC_LABEL
	}
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_rawset_2_0,
		luaMR__set_4_0_i37);
MR_def_label(luaMR__set_4_0,21)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_sv(3) = ((MR_Integer) LUA_ERRRUN);
	MR_sv(4) = ((MR_Word) MR_string_const("luaMR", 5));
	MR_sv(5) = ((MR_Word) MR_string_const(".", 1));
	MR_r1 = ((MR_Word) MR_string_const("predicate \140luaMR.set\'/4", 23));
	MR_r2 = ((MR_Word) MR_string_const(" attempted to set invalid ref.", 30));
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__set_4_0_i27);
MR_def_label(luaMR__set_4_0,27)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(5);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__set_4_0_i28);
MR_def_label(luaMR__set_4_0,28)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(4);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__set_4_0_i48);
MR_def_label(luaMR__set_4_0,19)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),4)) {
		MR_GOTO_LAB(luaMR__set_4_0_i33);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r4;
	MR_sv(3) = MR_r2;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r3;
	MR_sv(4) = MR_tempr1;
	MR_r1 = MR_tfield(4, MR_r1, 0);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pushstring_2_0,
		luaMR__set_4_0_i35);
MR_def_label(luaMR__set_4_0,35)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__push_value_2_0,
		luaMR__set_4_0_i36);
MR_def_label(luaMR__set_4_0,36)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Integer	I;
#define	MR_PROC_LABEL	mercury__luaMR__set_4_0
	MR_OBTAIN_GLOBAL_LOCK("globalindex");
{
#line 68 "Mercury/opts/luaMR.api.opt"
I = LUA_GLOBALSINDEX;;}
#line 4372 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("globalindex");
	MR_r1 = I;
#undef	MR_PROC_LABEL
	}
	MR_r2 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_rawset_2_0,
		luaMR__set_4_0_i37);
MR_def_label(luaMR__set_4_0,37)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_decr_sp_and_return(7);
MR_def_label(luaMR__set_4_0,33)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(6));
	if (MR_INT_NE(MR_tag(MR_r1),5)) {
		MR_GOTO_LAB(luaMR__set_4_0_i38);
	}
	MR_reset_ticket(MR_sv(6), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r4;
	MR_sv(3) = ((MR_Integer) LUA_ERRRUN);
	MR_sv(4) = ((MR_Word) MR_string_const("luaMR", 5));
	MR_sv(5) = ((MR_Word) MR_string_const(".", 1));
	MR_sv(2) = ((MR_Word) MR_string_const("predicate \140luaMR.set\'/4", 23));
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_string_const(" attempted to set invalid var: ", 31));
	MR_r2 = MR_tfield(5, MR_tempr1, 0);
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__set_4_0_i40);
MR_def_label(luaMR__set_4_0,40)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(2);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__set_4_0_i41);
MR_def_label(luaMR__set_4_0,41)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(5);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(fn__f_115_116_114_105_110_103_95_95_43_43_2_0,
		luaMR__set_4_0_i42);
MR_def_label(luaMR__set_4_0,42)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(4);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(fn__f_115_116_114_105_110_103_95_95_43_43_2_0,
		luaMR__set_4_0_i48);
MR_def_label(luaMR__set_4_0,38)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(6), MR_undo);
	MR_discard_ticket();
	MR_sv(1) = MR_r4;
	MR_sv(3) = ((MR_Integer) LUA_ERRRUN);
	MR_sv(4) = ((MR_Word) MR_string_const("luaMR", 5));
	MR_sv(5) = ((MR_Word) MR_string_const(".", 1));
	MR_r1 = ((MR_Word) MR_string_const("predicate \140luaMR.set\'/4", 23));
	MR_r2 = ((MR_Word) MR_string_const(" attempted to set impossible var.", 33));
	MR_np_call_localret_ent(fn__f_115_116_114_105_110_103_95_95_43_43_2_0,
		luaMR__set_4_0_i46);
MR_def_label(luaMR__set_4_0,46)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(5);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(fn__f_115_116_114_105_110_103_95_95_43_43_2_0,
		luaMR__set_4_0_i47);
MR_def_label(luaMR__set_4_0,47)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(4);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(fn__f_115_116_114_105_110_103_95_95_43_43_2_0,
		luaMR__set_4_0_i48);
MR_def_label(luaMR__set_4_0,48)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 2);
	MR_tfield(0, MR_r2, 0) = MR_sv(3);
	MR_tfield(0, MR_r2, 1) = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, lua_error));
	MR_succip_word = MR_sv(7);
	MR_decr_sp(7);
	MR_np_tailcall_ent(exception__throw_1_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__lua_settop_2_0);

MR_BEGIN_MODULE(luaMR_module20)
	MR_init_entry1(luaMR__local_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__local_3_0);
	MR_init_label1(luaMR__local_3_0,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__local_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 4520 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 4548 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 4562 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr2 = Index;
#undef	MR_PROC_LABEL
	}
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_tempr2) + (MR_Unsigned) (MR_Integer) 1);
	MR_tag_alloc_heap(MR_sv(1), 0, (MR_Integer) 1);
	MR_tfield(0, MR_sv(1), 0) = MR_r1;
	MR_sv(2) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_settop_2_0,
		luaMR__local_3_0_i3);
MR_def_label(luaMR__local_3_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__state__update_lua_trail_3_0);

MR_BEGIN_MODULE(luaMR_module21)
	MR_init_entry1(luaMR__local_3_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__local_3_1);
	MR_init_label2(luaMR__local_3_1,3,5)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local'/3 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__local_3_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(5);
	MR_sv(5) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2, MR_tempr3;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_1
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 4621 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_1
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 4637 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr3 = Index;
#undef	MR_PROC_LABEL
	}
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_tempr3) + (MR_Unsigned) (MR_Integer) 1);
	MR_tag_alloc_heap(MR_sv(1), 0, (MR_Integer) 1);
	MR_tfield(0, MR_sv(1), 0) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_sv(3) = MR_tempr1;
	MR_sv(4) = MR_tempr2;
	}
	MR_np_call_localret_ent(luaMR__api__lua_settop_2_0,
		luaMR__local_3_1_i3);
MR_def_label(luaMR__local_3_1,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_3_1
	L = (lua_State *) MR_sv(2);
	I = (MR_ChoicepointId) MR_sv(3);
	T = MR_sv(4);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 4674 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,6,2));
	MR_np_call_localret_ent(luaMR__state__update_lua_trail_3_0,
		luaMR__local_3_1_i5);
MR_def_label(luaMR__local_3_1,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(1);
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(5);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module22)
	MR_init_entry1(fn__luaMR__local_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__local_2_0);
	MR_init_label1(fn__luaMR__local_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__local_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__local_3_0,
		fn__luaMR__local_2_0_i2);
MR_def_label(fn__luaMR__local_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module23)
	MR_init_entry1(fn__luaMR__local_2_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__local_2_1);
	MR_init_label1(fn__luaMR__local_2_1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local'/3 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__local_2_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__local_3_1,
		fn__luaMR__local_2_1_i2);
MR_def_label(fn__luaMR__local_2_1,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module24)
	MR_init_entry1(fn__luaMR__local_table_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__local_table_2_0);
	MR_init_label1(fn__luaMR__local_table_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local_table'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__local_table_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__local_table_3_0,
		fn__luaMR__local_table_2_0_i2);
MR_def_label(fn__luaMR__local_table_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__lua_newtable_1_0);

MR_BEGIN_MODULE(luaMR_module25)
	MR_init_entry1(luaMR__local_table_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__local_table_3_0);
	MR_init_label1(luaMR__local_table_3_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'local_table'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__local_table_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_table_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 4835 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r1 = (MR_Word) L;
	MR_r2 = (MR_Word) I;
	MR_tempr1 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__local_table_3_0
	L = (lua_State *) MR_r1;
	I = (MR_ChoicepointId) MR_r2;
	T = MR_tempr1;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 4863 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r2;
	MR_sv(2) = MR_r1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_newtable_1_0,
		luaMR__local_table_3_0_i2);
MR_def_label(luaMR__local_table_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	{
	MR_Integer	I;
	lua_State *	L;
	MR_Integer	A;
#define	MR_PROC_LABEL	mercury__luaMR__local_table_3_0
	I = (MR_Integer) -1;
	L = (lua_State *) MR_sv(2);
	MR_OBTAIN_GLOBAL_LOCK("absolute");
{
#line 61 "Mercury/opts/luaMR.api.opt"

	A = I > 0 ? I : lua_gettop(L) + 1 + I;;}
#line 4889 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("absolute");
	MR_tempr1 = A;
#undef	MR_PROC_LABEL
	}
	MR_tag_alloc_heap(MR_r1, 0, (MR_Integer) 1);
	MR_tfield(0, MR_r1, 0) = MR_tempr1;
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(3);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module26)
	MR_init_entry1(fn__luaMR__ref_table_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__ref_table_2_0);
	MR_init_label1(fn__luaMR__ref_table_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ref_table'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__ref_table_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__ref_table_3_0,
		fn__luaMR__ref_table_2_0_i2);
MR_def_label(fn__luaMR__ref_table_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module27)
	MR_init_entry1(luaMR__ref_table_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__ref_table_3_0);
	MR_init_label2(luaMR__ref_table_3_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'ref_table'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__ref_table_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ref_table_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r1, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 4974 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r1 = (MR_Word) L;
	MR_r2 = (MR_Word) I;
	MR_tempr1 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__ref_table_3_0
	L = (lua_State *) MR_r1;
	I = (MR_ChoicepointId) MR_r2;
	T = MR_tempr1;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5002 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r2;
	MR_sv(2) = MR_r1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_newtable_1_0,
		luaMR__ref_table_3_0_i2);
MR_def_label(luaMR__ref_table_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	MR_Integer	Index;
	lua_State *	L;
	luaMR_Ref	V;
#define	MR_PROC_LABEL	mercury__luaMR__ref_table_3_0
	Index = (MR_Integer) -1;
	L = (lua_State *) MR_sv(2);
	MR_OBTAIN_GLOBAL_LOCK("lua_toref");
{
#line 168 "Mercury/opts/luaMR.api.opt"
V = (luaMR_Ref)luaMR_newref(L, Index);;}
#line 5027 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_toref");
	MR_tempr1 = (MR_Word) V;
#undef	MR_PROC_LABEL
	}
	MR_tag_alloc_heap(MR_tempr2, 3, (MR_Integer) 1);
	MR_tfield(3, MR_tempr2, 0) = MR_tempr1;
	MR_tempr1 = MR_sv(2);
	MR_sv(2) = MR_tempr2;
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__ref_table_3_0_i4);
MR_def_label(luaMR__ref_table_3_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module28)
	MR_init_entry1(fn__luaMR__make_lua_func_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_lua_func_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_lua_func'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_lua_func_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,2));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module29)
	MR_init_entry1(fn__luaMR__make_lua_func_1_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_lua_func_1_1);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_lua_func'/2 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_lua_func_1_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,3));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module30)
	MR_init_entry1(fn__luaMR__make_nondet_lua_func_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_nondet_lua_func_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_nondet_lua_func'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_nondet_lua_func_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,4));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module31)
	MR_init_entry1(fn__luaMR__make_nondet_lua_func_1_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_nondet_lua_func_1_1);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_nondet_lua_func'/2 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_nondet_lua_func_1_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,5));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module32)
	MR_init_entry1(fn__luaMR__make_nondet_lua_func_1_2);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_nondet_lua_func_1_2);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_nondet_lua_func'/2 mode 2 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_nondet_lua_func_1_2);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,6));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module33)
	MR_init_entry1(fn__luaMR__make_nondet_lua_func_1_3);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__make_nondet_lua_func_1_3);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'make_nondet_lua_func'/2 mode 3 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__make_nondet_lua_func_1_3);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 4);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(5,7));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 1;
	MR_tfield(0, MR_r2, 3) = MR_r1;
	MR_r1 = MR_r2;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(fn__luaMR__api__lua_loadstring_2_0);

MR_BEGIN_MODULE(luaMR_module34)
	MR_init_entry1(luaMR__string_to_func_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__string_to_func_4_0);
	MR_init_label2(luaMR__string_to_func_4_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'string_to_func'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__string_to_func_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__string_to_func_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5250 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__string_to_func_4_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5278 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	MR_sv(2) = MR_r2;
	}
	MR_np_call_localret_ent(fn__luaMR__api__lua_loadstring_2_0,
		luaMR__string_to_func_4_0_i2);
MR_def_label(luaMR__string_to_func_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__luaMR__string_to_func_4_0
	L = (lua_State *) MR_sv(2);
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 5301 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	{
	MR_Integer	Index;
	lua_State *	L;
	luaMR_Ref	V;
#define	MR_PROC_LABEL	mercury__luaMR__string_to_func_4_0
	Index = MR_tempr1;
	L = (lua_State *) MR_sv(2);
	MR_OBTAIN_GLOBAL_LOCK("lua_toref");
{
#line 168 "Mercury/opts/luaMR.api.opt"
V = (luaMR_Ref)luaMR_newref(L, Index);;}
#line 5317 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_toref");
	MR_tempr1 = (MR_Word) V;
#undef	MR_PROC_LABEL
	}
	MR_tag_alloc_heap(MR_tempr2, 3, (MR_Integer) 1);
	MR_tfield(3, MR_tempr2, 0) = MR_tempr1;
	MR_tempr1 = MR_sv(2);
	MR_sv(2) = MR_tempr2;
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		luaMR__string_to_func_4_0_i4);
MR_def_label(luaMR__string_to_func_4_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module35)
	MR_init_entry1(fn__luaMR__string_to_func_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__string_to_func_3_0);
	MR_init_label1(fn__luaMR__string_to_func_3_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'string_to_func'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__string_to_func_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__string_to_func_4_0,
		fn__luaMR__string_to_func_3_0_i2);
MR_def_label(fn__luaMR__string_to_func_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__push_values_3_0);
MR_decl_entry(luaMR__api__lua_call_3_0);
MR_decl_entry(luaMR__api__to_values_3_0);

MR_BEGIN_MODULE(luaMR_module36)
	MR_init_entry1(luaMR__call_lua_func_5_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__call_lua_func_5_0);
	MR_init_label6(luaMR__call_lua_func_5_0,2,3,4,5,7,9)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'call_lua_func'/5 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__call_lua_func_5_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(6);
	MR_sv(6) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2, MR_tempr3;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__call_lua_func_5_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r3, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5414 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_tempr1 = (MR_Word) L;
	MR_tempr2 = (MR_Word) I;
	MR_tempr3 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__call_lua_func_5_0
	L = (lua_State *) MR_tempr1;
	I = (MR_ChoicepointId) MR_tempr2;
	T = MR_tempr3;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5442 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr2);
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__luaMR__call_lua_func_5_0
	L = (lua_State *) MR_tempr1;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 5456 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr3 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr2;
	MR_sv(2) = MR_r2;
	MR_sv(3) = MR_tempr1;
	MR_sv(4) = MR_tempr3;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__call_lua_func_5_0_i2);
MR_def_label(luaMR__call_lua_func_5_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(3);
	MR_np_call_localret_ent(luaMR__api__push_values_3_0,
		luaMR__call_lua_func_5_0_i3);
MR_def_label(luaMR__call_lua_func_5_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Integer	M;
#define	MR_PROC_LABEL	mercury__luaMR__call_lua_func_5_0
	MR_OBTAIN_GLOBAL_LOCK("multret");
{
#line 195 "Mercury/opts/luaMR.api.opt"
M = LUA_MULTRET;;}
#line 5484 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("multret");
	MR_r2 = M;
#undef	MR_PROC_LABEL
	}
	MR_r3 = MR_sv(3);
	MR_np_call_localret_ent(luaMR__api__lua_call_3_0,
		luaMR__call_lua_func_5_0_i4);
MR_def_label(luaMR__call_lua_func_5_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__luaMR__call_lua_func_5_0
	L = (lua_State *) MR_sv(3);
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 5503 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_r3 = Index;
#undef	MR_PROC_LABEL
	}
	MR_r4 = (MR_Integer) ((MR_Unsigned) (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r3) + (MR_Unsigned) (MR_Integer) 1) - (MR_Unsigned) ((MR_Integer) MR_sv(4)));
	MR_store_ticket(MR_sv(5));
	if (MR_INT_NE(MR_r4,0)) {
		MR_GOTO_LAB(luaMR__call_lua_func_5_0_i5);
	}
	MR_reset_ticket(MR_sv(5), MR_commit);
	MR_prune_ticket();
	MR_sv(2) = (MR_Unsigned) 0U;
	MR_r2 = MR_sv(3);
	MR_r1 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_settop_2_0,
		luaMR__call_lua_func_5_0_i9);
MR_def_label(luaMR__call_lua_func_5_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(5), MR_undo);
	MR_discard_ticket();
	MR_r1 = MR_r4;
	MR_r2 = MR_sv(3);
	MR_np_call_localret_ent(luaMR__api__to_values_3_0,
		luaMR__call_lua_func_5_0_i7);
MR_def_label(luaMR__call_lua_func_5_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_sv(2) = MR_r1;
	MR_r2 = MR_sv(3);
	MR_r1 = MR_sv(4);
	MR_np_call_localret_ent(luaMR__api__lua_settop_2_0,
		luaMR__call_lua_func_5_0_i9);
MR_def_label(luaMR__call_lua_func_5_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(6);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module37)
	MR_init_entry1(fn__luaMR__call_lua_func_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__call_lua_func_4_0);
	MR_init_label1(fn__luaMR__call_lua_func_4_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'call_lua_func'/5 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__call_lua_func_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	MR_np_call_localret_ent(luaMR__call_lua_func_5_0,
		fn__luaMR__call_lua_func_4_0_i2);
MR_def_label(fn__luaMR__call_lua_func_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(1);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(luaMR__api__var_type_3_0);

MR_BEGIN_MODULE(luaMR_module38)
	MR_init_entry1(luaMR__var_type_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__var_type_4_0);
	MR_init_label1(luaMR__var_type_4_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'var_type'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__var_type_4_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_type_4_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5616 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_type_4_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5644 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__var_type_3_0,
		luaMR__var_type_4_0_i2);
MR_def_label(luaMR__var_type_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module39)
	MR_init_entry1(luaMR__var_type_4_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__var_type_4_1);
	MR_init_label1(luaMR__var_type_4_1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'var_type'/4 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__var_type_4_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_type_4_1
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5698 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__var_type_4_1
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5726 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__var_type_3_0,
		luaMR__var_type_4_1_i2);
MR_def_label(luaMR__var_type_4_1,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(1);
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module40)
	MR_init_entry1(fn__luaMR__var_type_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__var_type_3_0);
	MR_init_label1(fn__luaMR__var_type_3_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'var_type'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__var_type_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__var_type_3_0
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 74 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5780 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__var_type_3_0
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5808 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__var_type_3_0,
		fn__luaMR__var_type_3_0_i2);
MR_def_label(fn__luaMR__var_type_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(1);
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(2);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module41)
	MR_init_entry1(fn__luaMR__var_type_3_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__var_type_3_1);
	MR_init_label1(fn__luaMR__var_type_3_1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'var_type'/4 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__var_type_3_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__var_type_3_1
	MR_MAYBE_UNBOX_FOREIGN_TYPE(luaMR_lua_state *, MR_r2, S);
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 68 "Mercury/opts/luaMR.state.opt"

	
	L = S->lua;
	I = S->id;
	T = S->trail;
;}
#line 5867 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_r2 = (MR_Word) L;
	MR_tempr1 = (MR_Word) I;
	MR_tempr2 = T;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__var_type_3_1
	L = (lua_State *) MR_r2;
	I = (MR_ChoicepointId) MR_tempr1;
	T = MR_tempr2;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 5895 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_tempr1);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__var_type_3_0,
		fn__luaMR__var_type_3_1_i2);
MR_def_label(fn__luaMR__var_type_3_1,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(1);
	MR_r2 = MR_tempr1;
	MR_decr_sp_and_return(2);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module42)
	MR_init_entry1(luaMR__init_lua_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__init_lua_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'init_lua'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__init_lua_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__luaMR__init_lua_1_0
	L = (lua_State *) MR_r1;
	MR_save_registers();
	MR_OBTAIN_GLOBAL_LOCK("init_lua");
{
#line 550 "luaMR.m"
luaMR_init(L);;}
#line 5944 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("init_lua");
#ifndef MR_CONSERVATIVE_GC
	MR_restore_registers();
#endif
#undef	MR_PROC_LABEL
	}
	MR_decr_sp_and_return(1);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module43)
	MR_init_entry1(fn__luaMR__pop_one_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__pop_one_1_0);
	MR_init_label1(fn__luaMR__pop_one_1_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'pop_one'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__pop_one_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_sv(1) = (MR_Integer) 0;
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = (MR_Integer) 1;
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(luaMR__api__lua_pop_2_0,
		fn__luaMR__pop_one_1_0_i2);
MR_def_label(fn__luaMR__pop_one_1_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_decr_sp_and_return(2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module44)
	MR_init_entry1(fn__luaMR__get_args_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__get_args_2_0);
	MR_init_label1(fn__luaMR__get_args_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'get_args'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__get_args_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(2);
	MR_sv(2) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(1));
	if (MR_INT_NE(MR_r1,1)) {
		MR_GOTO_LAB(fn__luaMR__get_args_2_0_i2);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 1, (MR_Integer) 2);
	MR_tfield(1, MR_r1, 0) = ((MR_Word) MR_TAG_COMMON(0,9,0));
	MR_tfield(1, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(2);
MR_def_label(fn__luaMR__get_args_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	while (1) {
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tag_alloc_heap(MR_tempr1, 0, (MR_Integer) 1);
	MR_tfield(0, MR_tempr1, 0) = MR_r1;
	MR_tag_alloc_heap(MR_tempr2, 1, (MR_Integer) 2);
	MR_tfield(1, MR_tempr2, 0) = MR_tempr1;
	MR_tfield(1, MR_tempr2, 1) = MR_r2;
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r1) - (MR_Unsigned) (MR_Integer) 1);
	MR_r2 = MR_tempr2;
	MR_store_ticket(MR_sv(1));
	if (MR_INT_NE(MR_r1,1))
		continue;
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_tag_alloc_heap(MR_r1, 1, (MR_Integer) 2);
	MR_tfield(1, MR_r1, 0) = ((MR_Word) MR_TAG_COMMON(0,9,0));
	MR_tfield(1, MR_r1, 1) = MR_r2;
	MR_decr_sp_and_return(2);
	}
	break;
	} /* end while */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module45)
	MR_init_entry1(luaMR__return_args_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__return_args_3_0);
	MR_init_label7(luaMR__return_args_3_0,2,6,7,4,8,9,10)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'return_args'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__luaMR__return_args_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(5);
	MR_sv(5) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(4));
	if (MR_INT_NE(MR_r1,0)) {
		MR_GOTO_LAB(luaMR__return_args_3_0_i2);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(5);
MR_def_label(luaMR__return_args_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(4));
	if (MR_INT_EQ(MR_r1,0)) {
		MR_GOTO_LAB(luaMR__return_args_3_0_i4);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_sv(1) = MR_r2;
	MR_sv(2) = MR_tfield(1, MR_r1, 1);
	MR_r1 = MR_tfield(1, MR_r1, 0);
	MR_np_call_localret_ent(luaMR__api__push_var_2_0,
		luaMR__return_args_3_0_i6);
MR_def_label(luaMR__return_args_3_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(2);
	MR_r2 = MR_sv(1);
	MR_np_localcall_lab(luaMR__return_args_3_0,
		luaMR__return_args_3_0_i7);
MR_def_label(luaMR__return_args_3_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r1) + (MR_Unsigned) (MR_Integer) 1);
	MR_decr_sp_and_return(5);
MR_def_label(luaMR__return_args_3_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_sv(1) = ((MR_Integer) LUA_ERRRUN);
	MR_sv(2) = ((MR_Word) MR_string_const("luaMR", 5));
	MR_sv(3) = ((MR_Word) MR_string_const(".", 1));
	MR_r1 = ((MR_Word) MR_string_const("predicate \140luaMR.return_args\'/3", 31));
	MR_r2 = ((MR_Word) MR_string_const(" Invalid list of vars.", 22));
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__return_args_3_0_i8);
MR_def_label(luaMR__return_args_3_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(3);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__return_args_3_0_i9);
MR_def_label(luaMR__return_args_3_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(2);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(string__append_3_2,
		luaMR__return_args_3_0_i10);
MR_def_label(luaMR__return_args_3_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 2);
	MR_tfield(0, MR_r2, 0) = MR_sv(1);
	MR_tfield(0, MR_r2, 1) = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, lua_error));
	MR_succip_word = MR_sv(5);
	MR_decr_sp(5);
	MR_np_tailcall_ent(exception__throw_1_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_declare_entry(mercury__do_call_class_method_1);
MR_decl_entry(univ__type_to_univ_2_1);
MR_decl_entry(string__to_string__value_to_revstrings_prio_6_1);
MR_decl_entry(list__reverse_2_0);
MR_decl_entry(fn__string__append_list_1_0);

MR_BEGIN_MODULE(luaMR_module46)
	MR_init_entry1(fn__luaMR__to_string_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__to_string_1_0);
	MR_init_label7(fn__luaMR__to_string_1_0,3,7,8,9,10,11,12)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'to_string'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__luaMR__to_string_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(8);
	MR_sv(8) = ((MR_Word) MR_succip);
	MR_sv(3) = (MR_Integer) 1;
	MR_sv(1) = (MR_Integer) 1;
	MR_store_ticket(MR_sv(7));
	{
	MR_Integer	Index;
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__fn__luaMR__to_string_1_0
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	Index = MR_sv(3);
	L = (lua_State *) MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("lua_ismruserdata");
{
#line 101 "Mercury/opts/luaMR.api.opt"

	int Top = lua_gettop(L);
	lua_pushvalue(L, Index); /* 1 */	
	if(lua_isuserdata(L, -1) && lua_getmetatable(L, -1)) { /* 2 */ 
		lua_pushstring(L, LUA_MR_USERDATA);
		lua_rawget(L, -2);
		SUCCESS_INDICATOR = lua_toboolean(L, -1); 
		lua_settop(L, Top);
	} else {
		SUCCESS_INDICATOR = 0;
		lua_settop(L, Top);
	}
;}
#line 6199 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_ismruserdata");
if (!MercurySuccessIndicator) MR_GOTO_LAB(fn__luaMR__to_string_1_0_i3);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_reset_ticket(MR_sv(7), MR_commit);
	MR_prune_ticket();
	{
	MR_Word MR_tempr1;
	{
	MR_Integer	Index;
	lua_State *	L;
	MR_Word	V;
#define	MR_PROC_LABEL	mercury__fn__luaMR__to_string_1_0
	Index = MR_sv(3);
	L = (lua_State *) MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("lua_tomruserdata");
{
#line 162 "Mercury/opts/luaMR.api.opt"
V = **(MR_Word **)lua_touserdata(L, Index);;}
#line 6221 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_tomruserdata");
	MR_tempr1 = V;
#undef	MR_PROC_LABEL
	}
	MR_sv(2) = MR_r1;
	MR_r1 = MR_tempr1;
	MR_sv(3) = MR_tfield(0, MR_r1, 1);
	MR_sv(4) = MR_tfield(0, MR_r1, 0);
	MR_sv(5) = (MR_Integer) 1;
	MR_sv(6) = (MR_Unsigned) 0U;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,0));
	MR_r2 = (MR_Integer) 8;
	}
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(fn__luaMR__to_string_1_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_class_method_1),
		mercury__fn__luaMR__to_string_1_0_i8);
MR_def_label(fn__luaMR__to_string_1_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(7), MR_undo);
	MR_discard_ticket();
	{
	MR_Integer	Index;
	lua_State *	L;
	MR_Word	V;
#define	MR_PROC_LABEL	mercury__fn__luaMR__to_string_1_0
	Index = MR_sv(3);
	L = (lua_State *) MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("lua_tocuserdata");
{
#line 157 "Mercury/opts/luaMR.api.opt"
V = (size_t)lua_touserdata(L, Index);;}
#line 6253 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_tocuserdata");
	MR_r2 = V;
#undef	MR_PROC_LABEL
	}
	MR_sv(2) = MR_r1;
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(builtin, c_pointer));
	MR_np_call_localret_ent(univ__type_to_univ_2_1,
		fn__luaMR__to_string_1_0_i7);
MR_def_label(fn__luaMR__to_string_1_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_sv(3) = MR_tfield(0, MR_r1, 1);
	MR_sv(4) = MR_tfield(0, MR_r1, 0);
	MR_sv(5) = (MR_Integer) 1;
	MR_sv(6) = (MR_Unsigned) 0U;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,0));
	MR_r2 = (MR_Integer) 8;
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(fn__luaMR__to_string_1_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_class_method_1),
		mercury__fn__luaMR__to_string_1_0_i8);
MR_def_label(fn__luaMR__to_string_1_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(4);
	MR_r2 = MR_sv(5);
	MR_r4 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_tempr1) + (MR_Unsigned) (MR_Integer) 1);
	MR_r5 = MR_sv(3);
	MR_r6 = MR_sv(6);
	}
	MR_np_call_localret_ent(string__to_string__value_to_revstrings_prio_6_1,
		fn__luaMR__to_string_1_0_i9);
MR_def_label(fn__luaMR__to_string_1_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_STRING_CTOR_ADDR);
	MR_r2 = MR_tempr1;
	}
	MR_np_call_localret_ent(list__reverse_2_0,
		fn__luaMR__to_string_1_0_i10);
MR_def_label(fn__luaMR__to_string_1_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_call_localret_ent(fn__string__append_list_1_0,
		fn__luaMR__to_string_1_0_i11);
MR_def_label(fn__luaMR__to_string_1_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(2);
	MR_np_call_localret_ent(luaMR__api__lua_pushstring_2_0,
		fn__luaMR__to_string_1_0_i12);
MR_def_label(fn__luaMR__to_string_1_0,12)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_decr_sp_and_return(8);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Unify___builtin__c_pointer_0_0);

MR_BEGIN_MODULE(luaMR_module47)
	MR_init_entry1(__Unify___luaMR__c_function_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__c_function_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__c_function_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Compare___builtin__c_pointer_0_0);

MR_BEGIN_MODULE(luaMR_module48)
	MR_init_entry1(__Compare___luaMR__c_function_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__c_function_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__c_function_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module49)
	MR_init_entry1(__Unify___luaMR__error_type_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__error_type_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__error_type_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_r1 == MR_r2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module50)
	MR_init_entry1(__Compare___luaMR__error_type_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__error_type_0_0);
	MR_init_label2(__Compare___luaMR__error_type_0_0,2,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__error_type_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_r3 = MR_r2;
	MR_r2 = MR_r1;
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__error_type_0_0_i2);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__error_type_0_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) <= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__error_type_0_0_i3);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__error_type_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(1);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module51)
	MR_init_entry1(__Unify___luaMR__index_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__index_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__index_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_r1 == MR_r2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module52)
	MR_init_entry1(__Compare___luaMR__index_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__index_0_0);
	MR_init_label2(__Compare___luaMR__index_0_0,2,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__index_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_r3 = MR_r2;
	MR_r2 = MR_r1;
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__index_0_0_i2);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__index_0_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(1));
	if ((MR_r2 != MR_r3)) {
		MR_GOTO_LAB(__Compare___luaMR__index_0_0_i3);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__index_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp(1);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module53)
	MR_init_entry1(__Unify___luaMR__ls_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__ls_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__ls_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module54)
	MR_init_entry1(__Compare___luaMR__ls_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__ls_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__ls_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module55)
	MR_init_entry1(__Unify___luaMR__lua_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__lua_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__lua_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module56)
	MR_init_entry1(__Compare___luaMR__lua_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__lua_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__lua_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module57)
	MR_init_entry1(__Unify___luaMR__lua_error_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__lua_error_0_0);
	MR_init_label2(__Unify___luaMR__lua_error_0_0,3,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__lua_error_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__lua_error_0_0_i3);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_sv(1);
	MR_r1 = MR_tfield(0, MR_tempr1, 0);
	MR_tempr2 = MR_sv(2);
	MR_r2 = MR_tfield(0, MR_tempr2, 0);
	if ((MR_r1 != MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__lua_error_0_0_i1);
	}
	MR_r1 = MR_tfield(0, MR_tempr1, 1);
	MR_r2 = MR_tfield(0, MR_tempr2, 1);
	MR_r1 = (strcmp((char *) ((MR_Word *) MR_r1), (char *) ((MR_Word *) MR_r2)) == 0);
	MR_decr_sp(3);
	MR_proceed();
	}
MR_def_label(__Unify___luaMR__lua_error_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_decr_sp(3);
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(__Unify___luaMR__lua_error_0_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_decr_sp(3);
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module58)
	MR_init_entry1(__Compare___luaMR__lua_error_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__lua_error_0_0);
	MR_init_label7(__Compare___luaMR__lua_error_0_0,5,22,6,10,11,24,12)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__lua_error_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i24);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r2 = MR_tfield(0, MR_sv(2), 1);
	MR_r3 = MR_tfield(0, MR_sv(1), 1);
	MR_store_ticket(MR_sv(3));
	MR_r4 = MR_tfield(0, MR_sv(2), 0);
	MR_r5 = MR_tfield(0, MR_sv(1), 0);
	MR_store_ticket(MR_sv(4));
	if ((((MR_Integer) MR_r5) >= ((MR_Integer) MR_r4))) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i5);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_sv(1) = MR_r3;
	MR_sv(2) = MR_r2;
	MR_store_ticket(MR_sv(4));
	if (MR_INT_EQ(MR_r1,0)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i10);
	}
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_decr_sp(4);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_error_0_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(4));
	if ((MR_r5 != MR_r4)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i6);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_sv(1) = MR_r3;
	MR_sv(2) = MR_r2;
	MR_store_ticket(MR_sv(4));
	if (MR_INT_EQ(MR_r1,0)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i10);
	}
MR_def_label(__Compare___luaMR__lua_error_0_0,22)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_decr_sp(4);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_error_0_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_sv(1) = MR_r3;
	MR_sv(2) = MR_r2;
	MR_store_ticket(MR_sv(4));
	if (MR_INT_NE(MR_r1,0)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i22);
	}
MR_def_label(__Compare___luaMR__lua_error_0_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	{
	MR_Integer	Res;
	MR_String	S1;
	MR_String	S2;
#define	MR_PROC_LABEL	mercury____Compare___luaMR__lua_error_0_0
	S1 = (MR_String) MR_sv(1);
	S2 = (MR_String) MR_sv(2);
{
#line 256 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/private_builtin.opt"

    Res = strcmp(S1, S2);
;}
#line 6752 "Mercury/cs/luaMR.c"
	MR_r2 = Res;
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(3));
	if (MR_INT_GE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i11);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(4);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_error_0_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if (MR_INT_NE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__lua_error_0_0_i12);
	}
MR_def_label(__Compare___luaMR__lua_error_0_0,24)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(4);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_error_0_0,12)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp(4);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module59)
	MR_init_entry1(__Unify___luaMR__lua_state_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__lua_state_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__lua_state_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module60)
	MR_init_entry1(__Compare___luaMR__lua_state_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__lua_state_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__lua_state_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module61)
	MR_init_entry1(__Unify___luaMR__lua_type_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__lua_type_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__lua_type_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_r1 == MR_r2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module62)
	MR_init_entry1(__Compare___luaMR__lua_type_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__lua_type_0_0);
	MR_init_label2(__Compare___luaMR__lua_type_0_0,2,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__lua_type_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_r3 = MR_r2;
	MR_r2 = MR_r1;
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__lua_type_0_0_i2);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_type_0_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) <= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__lua_type_0_0_i3);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___luaMR__lua_type_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp(1);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(private_builtin__builtin_unify_pred_2_0);

MR_BEGIN_MODULE(luaMR_module63)
	MR_init_entry1(__Unify___luaMR__mr_func_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__mr_func_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__mr_func_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(private_builtin__builtin_unify_pred_2_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(builtin__compare_3_0);

MR_BEGIN_MODULE(luaMR_module64)
	MR_init_entry1(__Compare___luaMR__mr_func_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__mr_func_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__mr_func_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,1,0));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(builtin__compare_3_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module65)
	MR_init_entry1(__Unify___luaMR__nil_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__nil_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__nil_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_TRUE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module66)
	MR_init_entry1(__Compare___luaMR__nil_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__nil_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__nil_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 0;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module67)
	MR_init_entry1(__Unify___luaMR__ref_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__ref_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__ref_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module68)
	MR_init_entry1(__Compare___luaMR__ref_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__ref_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__ref_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Unify___univ__univ_0_0);

MR_BEGIN_MODULE(luaMR_module69)
	MR_init_entry1(__Unify___luaMR__value_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__value_0_0);
	MR_init_label10(__Unify___luaMR__value_0_0,3,17,21,23,11,5,25,13,46,7)
	MR_init_label3(__Unify___luaMR__value_0_0,27,35,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__value_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i3);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i17) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i21) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i23) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i11) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i5) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i25) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i13) MR_AND
		MR_LABEL_AP(__Unify___luaMR__value_0_0_i46));
MR_def_label(__Unify___luaMR__value_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(__Unify___luaMR__value_0_0,17)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),0)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___luaMR__lua_error_0_0);
MR_def_label(__Unify___luaMR__value_0_0,21)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (((MR_Integer) MR_tag(MR_sv(2))) == (MR_Integer) 1);
	MR_decr_sp_and_return(4);
MR_def_label(__Unify___luaMR__value_0_0,23)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),2)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_tfield(2, MR_sv(1), 0);
	MR_tempr2 = MR_tfield(2, MR_sv(2), 0);
	MR_r1 = (MR_word_to_float(MR_tempr1) == MR_word_to_float(MR_tempr2));
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Unify___luaMR__value_0_0,11)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),3)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_tfield(3, MR_sv(1), 0);
	MR_tempr2 = MR_tfield(3, MR_sv(2), 0);
	MR_r1 = (MR_tempr1 == MR_tempr2);
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Unify___luaMR__value_0_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),4)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = ((((MR_Unsigned) MR_tfield(4, MR_sv(1), 0)) >> (MR_Integer) 0) & 1);
	MR_r2 = ((((MR_Unsigned) MR_tfield(4, MR_sv(2), 0)) >> (MR_Integer) 0) & 1);
	MR_r1 = (MR_r1 == MR_r2);
	MR_decr_sp_and_return(4);
MR_def_label(__Unify___luaMR__value_0_0,25)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),5)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_tfield(5, MR_sv(1), 0);
	MR_tempr2 = MR_tfield(5, MR_sv(2), 0);
	MR_r1 = (strcmp((char *) ((MR_Word *) MR_tempr1), (char *) ((MR_Word *) MR_tempr2)) == 0);
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Unify___luaMR__value_0_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),6)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_tfield(6, MR_sv(1), 0);
	MR_r2 = MR_tfield(6, MR_sv(2), 0);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
MR_def_label(__Unify___luaMR__value_0_0,46)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_sv(1);
	MR_r2 = MR_tfield(7, MR_tempr1, 0);
	if (MR_INT_EQ(MR_r2,0)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i27);
	}
	if (MR_INT_EQ(MR_r2,1)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i7);
	}
	if (MR_INT_EQ(MR_r2,2)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i35);
	}
	if (MR_RTAGS_TESTR(MR_sv(2),7,3)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_tfield(7, MR_tempr1, 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___univ__univ_0_0);
	}
MR_def_label(__Unify___luaMR__value_0_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_RTAGS_TESTR(MR_sv(2),7,1)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
MR_def_label(__Unify___luaMR__value_0_0,27)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_RTAGS_TESTR(MR_sv(2),7,0)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
MR_def_label(__Unify___luaMR__value_0_0,35)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_RTAGS_TESTR(MR_sv(2),7,2)) {
		MR_GOTO_LAB(__Unify___luaMR__value_0_0_i1);
	}
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___luaMR__var_0_0);
MR_def_label(__Unify___luaMR__value_0_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Compare___univ__univ_0_0);

MR_BEGIN_MODULE(luaMR_module70)
	MR_init_entry1(__Compare___luaMR__value_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__value_0_0);
	MR_init_label10(__Compare___luaMR__value_0_0,99,271,121,128,141,142,150,151,152,166)
	MR_init_label10(__Compare___luaMR__value_0_0,52,56,57,76,5,7,8,29,167,176)
	MR_init_label10(__Compare___luaMR__value_0_0,177,291,178,192,77,82,98,266,236,30)
	MR_init_label9(__Compare___luaMR__value_0_0,51,33,193,214,203,237,258,312,249)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__value_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i291);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i99) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i121) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i142) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i52) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i5) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i167) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i77) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i266));
MR_def_label(__Compare___luaMR__value_0_0,99)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___luaMR__lua_error_0_0);
MR_def_label(__Compare___luaMR__value_0_0,271)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,121)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i128) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i141));
MR_def_label(__Compare___luaMR__value_0_0,128)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,141)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,142)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i150) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i166));
MR_def_label(__Compare___luaMR__value_0_0,150)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_tfield(2, MR_sv(1), 0);
	MR_r3 = MR_tfield(2, MR_sv(2), 0);
	MR_store_ticket(MR_sv(3));
	if ((MR_word_to_float(MR_r2) >= MR_word_to_float(MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i151);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,151)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if ((MR_word_to_float(MR_r2) <= MR_word_to_float(MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i152);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,152)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,166)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,52)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i56) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i76));
MR_def_label(__Compare___luaMR__value_0_0,56)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_tfield(3, MR_sv(1), 0);
	MR_r3 = MR_tfield(3, MR_sv(2), 0);
	MR_store_ticket(MR_sv(3));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i57);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,57)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if ((MR_r2 != MR_r3)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i178);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,76)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i7) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i29));
MR_def_label(__Compare___luaMR__value_0_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = ((((MR_Unsigned) MR_tfield(4, MR_sv(2), 0)) >> (MR_Integer) 0) & 1);
	MR_r3 = ((((MR_Unsigned) MR_tfield(4, MR_sv(1), 0)) >> (MR_Integer) 0) & 1);
	MR_store_ticket(MR_sv(3));
	if ((((MR_Integer) MR_r3) >= ((MR_Integer) MR_r2))) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i8);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if ((MR_r3 != MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i178);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,29)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,167)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i176) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i192));
MR_def_label(__Compare___luaMR__value_0_0,176)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_tfield(5, MR_sv(1), 0);
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_tfield(5, MR_sv(2), 0);
	{
	MR_Integer	Res;
	MR_String	S1;
	MR_String	S2;
#define	MR_PROC_LABEL	mercury____Compare___luaMR__value_0_0
	S1 = (MR_String) MR_r2;
	S2 = (MR_String) MR_tempr1;
{
#line 256 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/private_builtin.opt"

    Res = strcmp(S1, S2);
;}
#line 7486 "Mercury/cs/luaMR.c"
	MR_r2 = Res;
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(3));
	if (MR_INT_GE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i177);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Compare___luaMR__value_0_0,177)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if (MR_INT_NE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i178);
	}
MR_def_label(__Compare___luaMR__value_0_0,291)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,178)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,192)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,77)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i82) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i98));
MR_def_label(__Compare___luaMR__value_0_0,82)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(6, MR_sv(1), 0);
	MR_r2 = MR_tfield(6, MR_sv(2), 0);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
MR_def_label(__Compare___luaMR__value_0_0,98)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,266)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(1), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i193);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(1), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i30);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(1), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i237);
	}
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i236));
MR_def_label(__Compare___luaMR__value_0_0,236)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___univ__univ_0_0);
MR_def_label(__Compare___luaMR__value_0_0,30)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i51));
MR_def_label(__Compare___luaMR__value_0_0,51)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i33);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,33)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___luaMR__c_function_0_0);
MR_def_label(__Compare___luaMR__value_0_0,193)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i214));
MR_def_label(__Compare___luaMR__value_0_0,214)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i203);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i312);
	}
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,203)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___luaMR__lua_0_0);
MR_def_label(__Compare___luaMR__value_0_0,237)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i312) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i271) MR_AND
		MR_LABEL_AP(__Compare___luaMR__value_0_0_i258));
MR_def_label(__Compare___luaMR__value_0_0,258)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),0)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),1)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i271);
	}
	if (MR_INT_EQ(MR_tfield(7, MR_sv(2), 0),2)) {
		MR_GOTO_LAB(__Compare___luaMR__value_0_0_i249);
	}
MR_def_label(__Compare___luaMR__value_0_0,312)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(4);
MR_def_label(__Compare___luaMR__value_0_0,249)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(7, MR_sv(1), 1);
	MR_r2 = MR_tfield(7, MR_sv(2), 1);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Compare___luaMR__var_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(builtin__unify_2_0);

MR_BEGIN_MODULE(luaMR_module71)
	MR_init_entry1(__Unify___luaMR__values_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__values_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__values_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,1));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(builtin__unify_2_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module72)
	MR_init_entry1(__Compare___luaMR__values_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__values_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__values_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,1));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(builtin__compare_3_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module73)
	MR_init_entry1(__Unify___luaMR__var_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__var_0_0);
	MR_init_label9(__Unify___luaMR__var_0_0,3,15,7,9,17,21,5,13,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__var_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i3);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i15) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i7) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i17) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i21) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i13));
MR_def_label(__Unify___luaMR__var_0_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = MR_TRUE;
	MR_decr_sp_and_return(4);
MR_def_label(__Unify___luaMR__var_0_0,15)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),0)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_tfield(0, MR_sv(1), 0);
	MR_tempr2 = MR_tfield(0, MR_sv(2), 0);
	MR_r1 = (MR_tempr1 == MR_tempr2);
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Unify___luaMR__var_0_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),1)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_sv(1);
	MR_sv(1) = MR_tfield(1, MR_tempr1, 1);
	MR_tempr2 = MR_sv(2);
	MR_sv(2) = MR_tfield(1, MR_tempr2, 1);
	MR_r1 = MR_tfield(1, MR_tempr1, 0);
	MR_r2 = MR_tfield(1, MR_tempr2, 0);
	}
	MR_np_call_localret_ent(__Unify___luaMR__value_0_0,
		__Unify___luaMR__var_0_0_i9);
MR_def_label(__Unify___luaMR__var_0_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(4);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i3);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i15) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i7) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i17) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i21) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i13));
MR_def_label(__Unify___luaMR__var_0_0,17)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),2)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	MR_r1 = MR_tfield(2, MR_sv(1), 0);
	MR_r2 = MR_tfield(2, MR_sv(2), 0);
	MR_succip_word = MR_sv(4);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i3);
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i15) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i7) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i17) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i21) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Unify___luaMR__var_0_0_i13));
MR_def_label(__Unify___luaMR__var_0_0,21)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),3)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	MR_r1 = MR_tfield(3, MR_sv(1), 0);
	MR_r2 = MR_tfield(3, MR_sv(2), 0);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
MR_def_label(__Unify___luaMR__var_0_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),4)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	MR_r1 = MR_tfield(4, MR_sv(1), 0);
	MR_r2 = MR_tfield(4, MR_sv(2), 0);
	MR_r1 = (strcmp((char *) ((MR_Word *) MR_r1), (char *) ((MR_Word *) MR_r2)) == 0);
	MR_decr_sp_and_return(4);
MR_def_label(__Unify___luaMR__var_0_0,13)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(2)),5)) {
		MR_GOTO_LAB(__Unify___luaMR__var_0_0_i1);
	}
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_tfield(5, MR_sv(1), 0);
	MR_tempr2 = MR_tfield(5, MR_sv(2), 0);
	MR_r1 = (strcmp((char *) ((MR_Word *) MR_tempr1), (char *) ((MR_Word *) MR_tempr2)) == 0);
	MR_decr_sp_and_return(4);
	}
MR_def_label(__Unify___luaMR__var_0_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_decr_sp_and_return(4);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module74)
	MR_init_entry1(__Compare___luaMR__var_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__var_0_0);
	MR_init_label10(__Compare___luaMR__var_0_0,63,69,24,27,29,30,81,87,96,103)
	MR_init_label10(__Compare___luaMR__var_0_0,5,7,8,14,44,46,48,49,125,50)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__var_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(5);
	MR_sv(5) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i125);
	}
	MR_sv(2) = MR_r1;
	MR_sv(1) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i63) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i24) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i81) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i96) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i44));
MR_def_label(__Compare___luaMR__var_0_0,63)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_NE(MR_tag(MR_sv(1)),0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i14);
	}
	MR_r2 = MR_tfield(0, MR_sv(2), 0);
	MR_r3 = MR_tfield(0, MR_sv(1), 0);
	MR_store_ticket(MR_sv(3));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i69);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,69)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if ((MR_r2 != MR_r3)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i50);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,24)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i27) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14));
MR_def_label(__Compare___luaMR__var_0_0,27)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2, MR_tempr3, MR_tempr4;
	MR_tempr1 = MR_tfield(1, MR_sv(2), 1);
	MR_tempr2 = MR_tfield(1, MR_sv(1), 1);
	MR_store_ticket(MR_sv(3));
	MR_tempr3 = MR_sv(2);
	MR_sv(2) = MR_tempr2;
	MR_tempr4 = MR_sv(1);
	MR_sv(1) = MR_tempr1;
	MR_r1 = MR_tfield(1, MR_tempr3, 0);
	MR_r2 = MR_tfield(1, MR_tempr4, 0);
	}
	MR_np_call_localret_ent(__Compare___luaMR__value_0_0,
		__Compare___luaMR__var_0_0_i29);
MR_def_label(__Compare___luaMR__var_0_0,29)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_store_ticket(MR_sv(4));
	if (MR_INT_NE(MR_r1,0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i30);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(5);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i125);
	}
	MR_sv(2) = MR_r1;
	MR_sv(1) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i63) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i24) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i81) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i96) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i44));
MR_def_label(__Compare___luaMR__var_0_0,30)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,81)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i87) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14));
MR_def_label(__Compare___luaMR__var_0_0,87)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(2, MR_sv(2), 0);
	MR_r2 = MR_tfield(2, MR_sv(1), 0);
	MR_succip_word = MR_sv(5);
	MR_store_ticket(MR_sv(3));
	if ((MR_r1 == MR_r2)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i125);
	}
	MR_sv(2) = MR_r1;
	MR_sv(1) = MR_r2;
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(2))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i63) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i24) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i81) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i96) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i5) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i44));
MR_def_label(__Compare___luaMR__var_0_0,96)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i103) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14));
MR_def_label(__Compare___luaMR__var_0_0,103)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_tfield(3, MR_sv(2), 0);
	MR_r2 = MR_tfield(3, MR_sv(1), 0);
	MR_succip_word = MR_sv(5);
	MR_decr_sp(5);
	MR_np_tailcall_ent(__Compare___luaMR__ref_0_0);
MR_def_label(__Compare___luaMR__var_0_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_COMPUTED_GOTO(((MR_Unsigned) MR_tag(MR_sv(1))),
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i46) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i7) MR_AND
		MR_LABEL_AP(__Compare___luaMR__var_0_0_i14));
MR_def_label(__Compare___luaMR__var_0_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_tfield(4, MR_sv(2), 0);
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_tfield(4, MR_sv(1), 0);
	{
	MR_Integer	Res;
	MR_String	S1;
	MR_String	S2;
#define	MR_PROC_LABEL	mercury____Compare___luaMR__var_0_0
	S1 = (MR_String) MR_r2;
	S2 = (MR_String) MR_tempr1;
{
#line 256 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/private_builtin.opt"

    Res = strcmp(S1, S2);
;}
#line 8104 "Mercury/cs/luaMR.c"
	MR_r2 = Res;
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(3));
	if (MR_INT_GE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i8);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(5);
	}
MR_def_label(__Compare___luaMR__var_0_0,8)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if (MR_INT_NE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i50);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,14)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,44)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_EQ(MR_tag(MR_sv(1)),5)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i48);
	}
MR_def_label(__Compare___luaMR__var_0_0,46)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,48)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_tfield(5, MR_sv(2), 0);
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_tfield(5, MR_sv(1), 0);
	{
	MR_Integer	Res;
	MR_String	S1;
	MR_String	S2;
#define	MR_PROC_LABEL	mercury____Compare___luaMR__var_0_0
	S1 = (MR_String) MR_r2;
	S2 = (MR_String) MR_tempr1;
{
#line 256 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/private_builtin.opt"

    Res = strcmp(S1, S2);
;}
#line 8160 "Mercury/cs/luaMR.c"
	MR_r2 = Res;
#undef	MR_PROC_LABEL
	}
	MR_store_ticket(MR_sv(3));
	if (MR_INT_GE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i49);
	}
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp_and_return(5);
	}
MR_def_label(__Compare___luaMR__var_0_0,49)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(3));
	if (MR_INT_NE(MR_r2,0)) {
		MR_GOTO_LAB(__Compare___luaMR__var_0_0_i50);
	}
MR_def_label(__Compare___luaMR__var_0_0,125)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(5);
MR_def_label(__Compare___luaMR__var_0_0,50)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(3), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp_and_return(5);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module75)
	MR_init_entry1(__Unify___luaMR__vars_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___luaMR__vars_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___luaMR__vars_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,2));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(builtin__unify_2_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module76)
	MR_init_entry1(__Compare___luaMR__vars_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___luaMR__vars_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___luaMR__vars_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1, MR_tempr2;
	MR_tempr1 = MR_r1;
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,0,2));
	MR_tempr2 = MR_r2;
	MR_r2 = MR_tempr1;
	MR_r3 = MR_tempr2;
	MR_np_tailcall_ent(builtin__compare_3_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(fn__luaMR__state__current_id_0_0);
MR_declare_entry(mercury__do_call_closure_2);

MR_BEGIN_MODULE(luaMR_module77)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0);
	MR_init_label3(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0,2,3,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_lua_func__971__1'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8287 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(3) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0_i2);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_sv(2) = MR_r1;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0_i3);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0
	L = (lua_State *) MR_sv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8327 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__971__1_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_r2;
	MR_r2 = MR_sv(3);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module78)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0);
	MR_init_label4(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0,3,4,5,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_lua_func__977__1'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(5);
	MR_sv(5) = ((MR_Word) MR_succip);
	MR_store_ticket(MR_sv(4));
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8379 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(3) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0_i3);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_sv(2) = MR_r1;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0
	L = (lua_State *) MR_sv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8419 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_sv(1);
	MR_r2 = MR_sv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0_i5);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO_LAB(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0_i2);
	}
	MR_reset_ticket(MR_sv(4), MR_commit);
	MR_prune_ticket();
	MR_r1 = MR_r3;
	MR_r2 = MR_sv(3);
	MR_succip_word = MR_sv(5);
	MR_decr_sp(5);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_lua_func__977__1_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(4), MR_undo);
	MR_discard_ticket();
	MR_r1 = (MR_Integer) 0;
	MR_decr_sp_and_return(5);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_declare_entry(MR_do_fail);

MR_BEGIN_MODULE(luaMR_module79)
	MR_init_entry1(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0);
	MR_init_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0,1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__pred__make_nondet_lua_func__991__1'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_mkframe("pred luaMR.IntroducedFrom__pred__make_nondet_lua_func__991__1/4-0", 3,
		MR_ENTRY(MR_do_fail));
	MR_fv(1) = MR_r1;
	MR_fv(2) = MR_r2;
	MR_fv(3) = MR_r3;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0_i1);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0
	L = (lua_State *) MR_fv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8499 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_fv(1);
	MR_r2 = MR_fv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0_i2);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_r2;
	MR_succeed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(fn__solutions__solutions_1_1);

MR_BEGIN_MODULE(luaMR_module80)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0);
	MR_init_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_nondet_lua_func__988__1'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8548 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0_i2);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 6);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(10,0));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__1_4_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 3;
	MR_tfield(0, MR_r2, 3) = MR_sv(1);
	MR_tfield(0, MR_r2, 4) = MR_r1;
	MR_tfield(0, MR_r2, 5) = MR_sv(2);
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_np_call_localret_ent(fn__solutions__solutions_1_1,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__1_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(3);
	MR_decr_sp(3);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module81)
	MR_init_entry1(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0);
	MR_init_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0,1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__pred__make_nondet_lua_func__991__2'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_mkframe("pred luaMR.IntroducedFrom__pred__make_nondet_lua_func__991__2/4-0", 3,
		MR_ENTRY(MR_do_fail));
	MR_fv(1) = MR_r1;
	MR_fv(2) = MR_r2;
	MR_fv(3) = MR_r3;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0_i1);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0
	L = (lua_State *) MR_fv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8629 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_fv(1);
	MR_r2 = MR_fv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0_i2);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (!(MR_r1)) {
		MR_GOTO(MR_ENTRY(MR_do_fail));
	}
	MR_r1 = MR_r3;
	MR_succeed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module82)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0);
	MR_init_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_nondet_lua_func__988__2'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8680 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0_i2);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 6);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(10,1));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__2_4_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 3;
	MR_tfield(0, MR_r2, 3) = MR_sv(1);
	MR_tfield(0, MR_r2, 4) = MR_r1;
	MR_tfield(0, MR_r2, 5) = MR_sv(2);
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_np_call_localret_ent(fn__solutions__solutions_1_1,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__2_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(3);
	MR_decr_sp(3);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module83)
	MR_init_entry1(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0);
	MR_init_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0,1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__pred__make_nondet_lua_func__991__3'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_mkframe("pred luaMR.IntroducedFrom__pred__make_nondet_lua_func__991__3/4-0", 3,
		MR_ENTRY(MR_do_fail));
	MR_fv(1) = MR_r1;
	MR_fv(2) = MR_r2;
	MR_fv(3) = MR_r3;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0_i1);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0
	L = (lua_State *) MR_fv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8761 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_fv(1);
	MR_r2 = MR_fv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0_i2);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_r2;
	MR_succeed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module84)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0);
	MR_init_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_nondet_lua_func__988__3'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8809 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0_i2);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 6);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(10,2));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__3_4_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 3;
	MR_tfield(0, MR_r2, 3) = MR_sv(1);
	MR_tfield(0, MR_r2, 4) = MR_r1;
	MR_tfield(0, MR_r2, 5) = MR_sv(2);
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_np_call_localret_ent(fn__solutions__solutions_1_1,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__3_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(3);
	MR_decr_sp(3);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module85)
	MR_init_entry1(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0);
	MR_init_label2(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0,1,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__pred__make_nondet_lua_func__991__4'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_mkframe("pred luaMR.IntroducedFrom__pred__make_nondet_lua_func__991__4/4-0", 3,
		MR_ENTRY(MR_do_fail));
	MR_fv(1) = MR_r1;
	MR_fv(2) = MR_r2;
	MR_fv(3) = MR_r3;
	MR_np_call_localret_ent(fn__luaMR__state__current_id_0_0,
		luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0_i1);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0
	L = (lua_State *) MR_fv(3);
	I = (MR_ChoicepointId) MR_r1;
	T = (MR_Unsigned) 0U;
	MR_OBTAIN_GLOBAL_LOCK("ls");
{
#line 80 "Mercury/opts/luaMR.state.opt"



	luaMR_lua_state * new = MR_GC_NEW(luaMR_lua_state);
	new->lua = L;
	new->id = I;
	new->trail = T;
	S = new;
;}
#line 8890 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("ls");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r3);
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_fv(1);
	MR_r2 = MR_fv(2);
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0));
	MR_noprof_call_localret(MR_ENTRY(mercury__do_call_closure_2),
		mercury__luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0_i2);
MR_def_label(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_r2;
	MR_succeed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(luaMR_module86)
	MR_init_entry1(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0);
	MR_init_label2(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0,2,4)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'IntroducedFrom__func__make_nondet_lua_func__988__4'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1;
	{
	lua_State *	L;
	MR_Integer	Index;
#define	MR_PROC_LABEL	mercury__fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0
	L = (lua_State *) MR_r2;
	MR_OBTAIN_GLOBAL_LOCK("lua_gettop");
{
#line 81 "Mercury/opts/luaMR.api.opt"
Index = lua_gettop(L); ;}
#line 8938 "Mercury/cs/luaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_gettop");
	MR_tempr1 = Index;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_r1 = MR_tempr1;
	MR_r2 = (MR_Unsigned) 0U;
	}
	MR_np_call_localret_ent(fn__luaMR__get_args_2_0,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0_i2);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r2, 0, (MR_Integer) 6);
	MR_tfield(0, MR_r2, 0) = ((MR_Word) MR_COMMON(10,3));
	MR_tfield(0, MR_r2, 1) = ((MR_Word) MR_ENTRY_AP(luaMR__IntroducedFrom__pred__make_nondet_lua_func__991__4_4_0));
	MR_tfield(0, MR_r2, 2) = (MR_Integer) 3;
	MR_tfield(0, MR_r2, 3) = MR_sv(1);
	MR_tfield(0, MR_r2, 4) = MR_r1;
	MR_tfield(0, MR_r2, 5) = MR_sv(2);
	MR_r1 = ((MR_Word) MR_CTOR0_ADDR(luaMR, var));
	MR_np_call_localret_ent(fn__solutions__solutions_1_1,
		fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0_i4);
MR_def_label(fn__luaMR__IntroducedFrom__func__make_nondet_lua_func__988__4_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r2 = MR_sv(2);
	MR_succip_word = MR_sv(3);
	MR_decr_sp(3);
	MR_np_tailcall_ent(luaMR__return_args_3_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_declare_entry(mercury__fn__luaMR__to_string_1_0);

MR_Integer
luaMR_tostring(lua_State * Mercury__argument1);

MR_Integer
luaMR_tostring(lua_State * Mercury__argument1)
{
#if MR_NUM_REAL_REGS > 0
	MR_Word c_regs[MR_NUM_REAL_REGS];
#endif
#if MR_THREAD_SAFE
	MR_bool must_finalize_engine;
#endif
#if MR_DEEP_PROFILING
	MR_CallSiteDynList **saved_cur_callback;
	MR_CallSiteDynamic *saved_cur_csd;
#endif
	MR_Integer return_value;

	MR_save_regs_to_mem(c_regs);
#if MR_THREAD_SAFE
	must_finalize_engine = MR_init_thread(MR_use_now);
#endif
#if MR_DEEP_PROFILING
	saved_cur_callback = MR_current_callback_site;
	saved_cur_csd = MR_current_call_site_dynamic;
	MR_setup_callback(MR_ENTRY(mercury__fn__luaMR__to_string_1_0));
#endif
	MR_restore_registers();
	MR_MAYBE_BOX_FOREIGN_TYPE(lua_State *, Mercury__argument1, MR_r1);
	MR_save_transient_registers();
	(void) MR_call_engine(MR_ENTRY(mercury__fn__luaMR__to_string_1_0), MR_FALSE);
	MR_restore_transient_registers();
#if MR_DEEP_PROFILING
	MR_current_call_site_dynamic = saved_cur_csd;
	MR_current_callback_site = saved_cur_callback;
#endif
	return_value = MR_r1;
#if MR_THREAD_SAFE
	if (must_finalize_engine) {
		 MR_finalize_thread_engine();
	}
#endif
	MR_restore_regs_from_mem(c_regs);
	return return_value;
}


static void mercury__luaMR_maybe_bunch_0(void)
{
	luaMR_module0();
	luaMR_module1();
	luaMR_module2();
	luaMR_module3();
	luaMR_module4();
	luaMR_module5();
	luaMR_module6();
	luaMR_module7();
	luaMR_module8();
	luaMR_module9();
	luaMR_module10();
	luaMR_module11();
	luaMR_module12();
	luaMR_module13();
	luaMR_module14();
	luaMR_module15();
	luaMR_module16();
	luaMR_module17();
	luaMR_module18();
	luaMR_module19();
	luaMR_module20();
	luaMR_module21();
	luaMR_module22();
	luaMR_module23();
	luaMR_module24();
	luaMR_module25();
	luaMR_module26();
	luaMR_module27();
	luaMR_module28();
	luaMR_module29();
	luaMR_module30();
	luaMR_module31();
	luaMR_module32();
	luaMR_module33();
	luaMR_module34();
	luaMR_module35();
	luaMR_module36();
	luaMR_module37();
	luaMR_module38();
	luaMR_module39();
}

static void mercury__luaMR_maybe_bunch_1(void)
{
	luaMR_module40();
	luaMR_module41();
	luaMR_module42();
	luaMR_module43();
	luaMR_module44();
	luaMR_module45();
	luaMR_module46();
	luaMR_module47();
	luaMR_module48();
	luaMR_module49();
	luaMR_module50();
	luaMR_module51();
	luaMR_module52();
	luaMR_module53();
	luaMR_module54();
	luaMR_module55();
	luaMR_module56();
	luaMR_module57();
	luaMR_module58();
	luaMR_module59();
	luaMR_module60();
	luaMR_module61();
	luaMR_module62();
	luaMR_module63();
	luaMR_module64();
	luaMR_module65();
	luaMR_module66();
	luaMR_module67();
	luaMR_module68();
	luaMR_module69();
	luaMR_module70();
	luaMR_module71();
	luaMR_module72();
	luaMR_module73();
	luaMR_module74();
	luaMR_module75();
	luaMR_module76();
	luaMR_module77();
	luaMR_module78();
	luaMR_module79();
}

static void mercury__luaMR_maybe_bunch_2(void)
{
	luaMR_module80();
	luaMR_module81();
	luaMR_module82();
	luaMR_module83();
	luaMR_module84();
	luaMR_module85();
	luaMR_module86();
}

/* suppress gcc -Wmissing-decls warnings */
void mercury__luaMR__init(void);
void mercury__luaMR__init_type_tables(void);
void mercury__luaMR__init_debugger(void);
#ifdef MR_DEEP_PROFILING
void mercury__luaMR__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);
#endif
#ifdef MR_RECORD_TERM_SIZES
void mercury__luaMR__init_complexity_procs(void);
#endif
#ifdef MR_THREADSCOPE
void mercury__luaMR__init_threadscope_string_table(void);
#endif
const char *mercury__luaMR__grade_check(void);

void mercury__luaMR__init(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	mercury__luaMR_maybe_bunch_0();
	mercury__luaMR_maybe_bunch_1();
	mercury__luaMR_maybe_bunch_2();
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_c_function_0,
		luaMR__c_function_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_error_type_0,
		luaMR__error_type_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_index_0,
		luaMR__index_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_ls_0,
		luaMR__ls_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_lua_0,
		luaMR__lua_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_lua_error_0,
		luaMR__lua_error_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_lua_state_0,
		luaMR__lua_state_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_lua_type_0,
		luaMR__lua_type_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_mr_func_0,
		luaMR__mr_func_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_nil_0,
		luaMR__nil_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_ref_0,
		luaMR__ref_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_value_0,
		luaMR__value_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_values_0,
		luaMR__values_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_var_0,
		luaMR__var_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_luaMR__type_ctor_info_vars_0,
		luaMR__vars_0_0);
	mercury__luaMR__init_debugger();
}

void mercury__luaMR__init_type_tables(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_c_function_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_error_type_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_index_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_ls_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_lua_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_lua_error_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_lua_state_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_lua_type_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_mr_func_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_nil_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_ref_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_value_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_values_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_var_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_luaMR__type_ctor_info_vars_0);
	}
}


void mercury__luaMR__init_debugger(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}

#ifdef MR_DEEP_PROFILING

void mercury__luaMR__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)
{
	MR_write_out_module_proc_reps_start(procrep_fp, &mercury_data__module_layout__luaMR);
	MR_write_out_module_proc_reps_end(procrep_fp);
}

#endif

#ifdef MR_RECORD_TERM_SIZES

void mercury__luaMR__init_complexity_procs(void)
{
}

#endif

#ifdef MR_THREADSCOPE

void mercury__luaMR__init_threadscope_string_table(void)
{
}

#endif

// Ensure everything is compiled with the same grade.
const char *mercury__luaMR__grade_check(void)
{
    return &MR_GRADE_VAR;
}
