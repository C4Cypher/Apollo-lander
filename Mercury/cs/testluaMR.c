/*
** Automatically generated from `testluaMR.m'
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
INIT mercury__testluaMR__init
ENDINIT
*/

#define MR_ALLOW_RESET
#include "mercury_imp.h"
#line 28 "Mercury/cs/testluaMR.c"
#include "array.mh"

#line 31 "Mercury/cs/testluaMR.c"
#line 32 "Mercury/cs/testluaMR.c"
#include "benchmarking.mh"

#line 35 "Mercury/cs/testluaMR.c"
#line 36 "Mercury/cs/testluaMR.c"
#include "bitmap.mh"

#line 39 "Mercury/cs/testluaMR.c"
#line 40 "Mercury/cs/testluaMR.c"
#include "builtin.mh"

#line 43 "Mercury/cs/testluaMR.c"
#line 44 "Mercury/cs/testluaMR.c"
#include "char.mh"

#line 47 "Mercury/cs/testluaMR.c"
#line 48 "Mercury/cs/testluaMR.c"
#include "construct.mh"

#line 51 "Mercury/cs/testluaMR.c"
#line 52 "Mercury/cs/testluaMR.c"
#include "dir.mh"

#line 55 "Mercury/cs/testluaMR.c"
#line 56 "Mercury/cs/testluaMR.c"
#include "exception.mh"

#line 59 "Mercury/cs/testluaMR.c"
#line 60 "Mercury/cs/testluaMR.c"
#include "float.mh"

#line 63 "Mercury/cs/testluaMR.c"
#line 64 "Mercury/cs/testluaMR.c"
#include "int.mh"

#line 67 "Mercury/cs/testluaMR.c"
#line 68 "Mercury/cs/testluaMR.c"
#include "int16.mh"

#line 71 "Mercury/cs/testluaMR.c"
#line 72 "Mercury/cs/testluaMR.c"
#include "int32.mh"

#line 75 "Mercury/cs/testluaMR.c"
#line 76 "Mercury/cs/testluaMR.c"
#include "int64.mh"

#line 79 "Mercury/cs/testluaMR.c"
#line 80 "Mercury/cs/testluaMR.c"
#include "int8.mh"

#line 83 "Mercury/cs/testluaMR.c"
#line 84 "Mercury/cs/testluaMR.c"
#include "io.mh"

#line 87 "Mercury/cs/testluaMR.c"
#line 88 "Mercury/cs/testluaMR.c"
#include "luaMR.mh"

#line 91 "Mercury/cs/testluaMR.c"
#line 92 "Mercury/cs/testluaMR.c"
#include "math.mh"

#line 95 "Mercury/cs/testluaMR.c"
#line 96 "Mercury/cs/testluaMR.c"
#include "pretty_printer.mh"

#line 99 "Mercury/cs/testluaMR.c"
#line 100 "Mercury/cs/testluaMR.c"
#include "private_builtin.mh"

#line 103 "Mercury/cs/testluaMR.c"
#line 104 "Mercury/cs/testluaMR.c"
#include "rtti_implementation.mh"

#line 107 "Mercury/cs/testluaMR.c"
#line 108 "Mercury/cs/testluaMR.c"
#include "stm_builtin.mh"

#line 111 "Mercury/cs/testluaMR.c"
#line 112 "Mercury/cs/testluaMR.c"
#include "store.mh"

#line 115 "Mercury/cs/testluaMR.c"
#line 116 "Mercury/cs/testluaMR.c"
#include "string.mh"

#line 119 "Mercury/cs/testluaMR.c"
#line 120 "Mercury/cs/testluaMR.c"
#include "table_builtin.mh"

#line 123 "Mercury/cs/testluaMR.c"
#line 124 "Mercury/cs/testluaMR.c"
#include "testluaMR.mh"

#line 127 "Mercury/cs/testluaMR.c"
#line 128 "Mercury/cs/testluaMR.c"
#include "time.mh"

#line 131 "Mercury/cs/testluaMR.c"
#line 132 "Mercury/cs/testluaMR.c"
#include "trail.mh"

#line 135 "Mercury/cs/testluaMR.c"
#line 136 "Mercury/cs/testluaMR.c"
#include "type_desc.mh"

#line 139 "Mercury/cs/testluaMR.c"
#line 140 "Mercury/cs/testluaMR.c"
#include "uint.mh"

#line 143 "Mercury/cs/testluaMR.c"
#line 144 "Mercury/cs/testluaMR.c"
#include "uint16.mh"

#line 147 "Mercury/cs/testluaMR.c"
#line 148 "Mercury/cs/testluaMR.c"
#include "uint32.mh"

#line 151 "Mercury/cs/testluaMR.c"
#line 152 "Mercury/cs/testluaMR.c"
#include "uint64.mh"

#line 155 "Mercury/cs/testluaMR.c"
#line 156 "Mercury/cs/testluaMR.c"
#include "uint8.mh"

#line 159 "Mercury/cs/testluaMR.c"
#line 160 "Mercury/cs/testluaMR.c"
#include "version_array.mh"

#line 163 "Mercury/cs/testluaMR.c"
#line 164 "Mercury/cs/testluaMR.c"
#include "luaMR.api.mh"

#line 167 "Mercury/cs/testluaMR.c"
#line 168 "Mercury/cs/testluaMR.c"
#include "luaMR.state.mh"

#line 171 "Mercury/cs/testluaMR.c"
#line 172 "Mercury/cs/testluaMR.c"
#ifndef TESTLUAMR_DECL_GUARD
#define TESTLUAMR_DECL_GUARD

#line 176 "Mercury/cs/testluaMR.c"
#line 177 "Mercury/cs/testluaMR.c"

#endif
#line 180 "Mercury/cs/testluaMR.c"

#ifdef _MSC_VER
#define MR_STATIC_LINKAGE extern
#else
#define MR_STATIC_LINKAGE static
#endif


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_0 {
	MR_Word * f1[3];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_0 mercury_common_0[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_1 {
	MR_Word * f1[4];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_1 mercury_common_1[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_2 {
	MR_Word * f1[5];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_2 mercury_common_2[];
MR_decl_label1(main_2_0, 2)
MR_def_extern_entry(main_2_0)

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__stream__arity2__io__output_stream__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_io__type_ctor_info_output_stream_0;
extern const MR_TypeCtorInfo_Struct mercury_data_io__type_ctor_info_state_0;
static const struct mercury_type_0 mercury_common_0[1] =
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
static const struct mercury_type_1 mercury_common_1[1] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__output__arity2__io__output_stream__arity0__io__state__arity0__,
MR_TAG_COMMON(0,0,0),
MR_CTOR0_ADDR(io, output_stream),
MR_IO_CTOR_ADDR
}
},
};

extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__string__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_string_0;
extern const MR_BaseTypeclassInfo mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__character__arity0__io__state__arity0__[];
extern const MR_TypeCtorInfo_Struct mercury_data_builtin__type_ctor_info_character_0;
static const struct mercury_type_2 mercury_common_2[2] =
{
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__string__arity0__io__state__arity0__,
MR_TAG_COMMON(0,1,0),
MR_CTOR0_ADDR(io, output_stream),
MR_STRING_CTOR_ADDR,
MR_IO_CTOR_ADDR
}
},
{
{
(MR_Word *) &mercury_data_base_typeclass_info_stream__writer__arity3__io__output_stream__arity0__character__arity0__io__state__arity0__,
MR_TAG_COMMON(0,1,0),
MR_CTOR0_ADDR(io, output_stream),
MR_CHAR_CTOR_ADDR,
MR_IO_CTOR_ADDR
}
},
};



MR_decl_entry(luaMR__string_to_func_4_0);
MR_decl_entry(luaMR__call_lua_func_5_0);

MR_BEGIN_MODULE(testluaMR_module0)
	MR_init_entry1(main_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__main_2_0);
	MR_init_label1(main_2_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'main'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__main_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_sv(1) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	lua_State *	L;
#define	MR_PROC_LABEL	mercury__main_2_0
	MR_OBTAIN_GLOBAL_LOCK("lua_new");
{
#line 134 "Mercury/opts/luaMR.api.opt"

	void * ptr = MR_malloc(sizeof(ptr));
	L = lua_newstate((lua_Alloc)luaMR_alloc, ptr);
	luaL_openlibs(L);
	luaMR_init(L);
	;}
#line 314 "Mercury/cs/testluaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_new");
	MR_tempr1 = (MR_Word) L;
#undef	MR_PROC_LABEL
	}
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__main_2_0
{
#line 45 "Mercury/opts/trail.opt"

    Id = MR_null_choicepoint_id();
;}
#line 327 "Mercury/cs/testluaMR.c"
	MR_tempr2 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	{
	lua_State *	L;
	MR_ChoicepointId	I;
	MR_Word	T;
	luaMR_lua_state *	S;
#define	MR_PROC_LABEL	mercury__main_2_0
	L = (lua_State *) MR_tempr1;
	I = (MR_ChoicepointId) MR_tempr2;
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
#line 351 "Mercury/cs/testluaMR.c"
	MR_RELEASE_GLOBAL_LOCK("lua_state");
	MR_MAYBE_BOX_FOREIGN_TYPE(luaMR_lua_state *, S, MR_r2);
#undef	MR_PROC_LABEL
	}
	MR_r1 = ((MR_Word) MR_string_const("print \"Hello World!\"", 20));
	}
	MR_np_call_localret_ent(luaMR__string_to_func_4_0,
		main_2_0_i2);
MR_def_label(main_2_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r2;
	MR_r2 = (MR_Unsigned) 0U;
	MR_r3 = MR_tempr1;
	MR_succip_word = MR_sv(1);
	MR_decr_sp(1);
	MR_np_tailcall_ent(luaMR__call_lua_func_5_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

static void mercury__testluaMR_maybe_bunch_0(void)
{
	testluaMR_module0();
}

/* suppress gcc -Wmissing-decls warnings */
void mercury__testluaMR__init(void);
void mercury__testluaMR__init_type_tables(void);
void mercury__testluaMR__init_debugger(void);
#ifdef MR_DEEP_PROFILING
void mercury__testluaMR__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);
#endif
#ifdef MR_RECORD_TERM_SIZES
void mercury__testluaMR__init_complexity_procs(void);
#endif
#ifdef MR_THREADSCOPE
void mercury__testluaMR__init_threadscope_string_table(void);
#endif
const char *mercury__testluaMR__grade_check(void);

void mercury__testluaMR__init(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	mercury__testluaMR_maybe_bunch_0();
	mercury__testluaMR__init_debugger();
}

void mercury__testluaMR__init_type_tables(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}


void mercury__testluaMR__init_debugger(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}

#ifdef MR_DEEP_PROFILING

void mercury__testluaMR__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)
{
	MR_write_out_module_proc_reps_start(procrep_fp, &mercury_data__module_layout__testluaMR);
	MR_write_out_module_proc_reps_end(procrep_fp);
}

#endif

#ifdef MR_RECORD_TERM_SIZES

void mercury__testluaMR__init_complexity_procs(void)
{
}

#endif

#ifdef MR_THREADSCOPE

void mercury__testluaMR__init_threadscope_string_table(void)
{
}

#endif

// Ensure everything is compiled with the same grade.
const char *mercury__testluaMR__grade_check(void)
{
    return &MR_GRADE_VAR;
}
