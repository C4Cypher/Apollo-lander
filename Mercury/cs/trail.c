/*
** Automatically generated from `trail.m'
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
INIT mercury__trail__init
ENDINIT
*/

#define MR_ALLOW_RESET
#include "mercury_imp.h"
#line 28 "Mercury/cs/trail.c"
#include "array.mh"

#line 31 "Mercury/cs/trail.c"
#line 32 "Mercury/cs/trail.c"
#include "benchmarking.mh"

#line 35 "Mercury/cs/trail.c"
#line 36 "Mercury/cs/trail.c"
#include "bitmap.mh"

#line 39 "Mercury/cs/trail.c"
#line 40 "Mercury/cs/trail.c"
#include "builtin.mh"

#line 43 "Mercury/cs/trail.c"
#line 44 "Mercury/cs/trail.c"
#include "char.mh"

#line 47 "Mercury/cs/trail.c"
#line 48 "Mercury/cs/trail.c"
#include "construct.mh"

#line 51 "Mercury/cs/trail.c"
#line 52 "Mercury/cs/trail.c"
#include "dir.mh"

#line 55 "Mercury/cs/trail.c"
#line 56 "Mercury/cs/trail.c"
#include "exception.mh"

#line 59 "Mercury/cs/trail.c"
#line 60 "Mercury/cs/trail.c"
#include "float.mh"

#line 63 "Mercury/cs/trail.c"
#line 64 "Mercury/cs/trail.c"
#include "int.mh"

#line 67 "Mercury/cs/trail.c"
#line 68 "Mercury/cs/trail.c"
#include "int16.mh"

#line 71 "Mercury/cs/trail.c"
#line 72 "Mercury/cs/trail.c"
#include "int32.mh"

#line 75 "Mercury/cs/trail.c"
#line 76 "Mercury/cs/trail.c"
#include "int64.mh"

#line 79 "Mercury/cs/trail.c"
#line 80 "Mercury/cs/trail.c"
#include "int8.mh"

#line 83 "Mercury/cs/trail.c"
#line 84 "Mercury/cs/trail.c"
#include "io.mh"

#line 87 "Mercury/cs/trail.c"
#line 88 "Mercury/cs/trail.c"
#include "math.mh"

#line 91 "Mercury/cs/trail.c"
#line 92 "Mercury/cs/trail.c"
#include "pretty_printer.mh"

#line 95 "Mercury/cs/trail.c"
#line 96 "Mercury/cs/trail.c"
#include "private_builtin.mh"

#line 99 "Mercury/cs/trail.c"
#line 100 "Mercury/cs/trail.c"
#include "rtti_implementation.mh"

#line 103 "Mercury/cs/trail.c"
#line 104 "Mercury/cs/trail.c"
#include "stm_builtin.mh"

#line 107 "Mercury/cs/trail.c"
#line 108 "Mercury/cs/trail.c"
#include "store.mh"

#line 111 "Mercury/cs/trail.c"
#line 112 "Mercury/cs/trail.c"
#include "string.mh"

#line 115 "Mercury/cs/trail.c"
#line 116 "Mercury/cs/trail.c"
#include "table_builtin.mh"

#line 119 "Mercury/cs/trail.c"
#line 120 "Mercury/cs/trail.c"
#include "time.mh"

#line 123 "Mercury/cs/trail.c"
#line 124 "Mercury/cs/trail.c"
#include "trail.mh"

#line 127 "Mercury/cs/trail.c"
#line 128 "Mercury/cs/trail.c"
#include "type_desc.mh"

#line 131 "Mercury/cs/trail.c"
#line 132 "Mercury/cs/trail.c"
#include "uint.mh"

#line 135 "Mercury/cs/trail.c"
#line 136 "Mercury/cs/trail.c"
#include "uint16.mh"

#line 139 "Mercury/cs/trail.c"
#line 140 "Mercury/cs/trail.c"
#include "uint32.mh"

#line 143 "Mercury/cs/trail.c"
#line 144 "Mercury/cs/trail.c"
#include "uint64.mh"

#line 147 "Mercury/cs/trail.c"
#line 148 "Mercury/cs/trail.c"
#include "uint8.mh"

#line 151 "Mercury/cs/trail.c"
#line 152 "Mercury/cs/trail.c"
#include "version_array.mh"

#line 155 "Mercury/cs/trail.c"
#line 156 "Mercury/cs/trail.c"
#ifndef TRAIL_DECL_GUARD
#define TRAIL_DECL_GUARD

#line 160 "Mercury/cs/trail.c"
#line 155 "trail.m"

    #define MR_copy_fake_regs(src, dest)                                \
        do {                                                            \
            MR_memcpy(dest, src, sizeof(MR_Word) * MR_MAX_FAKE_REG);    \
        } while(0)

    extern void
    ML_call_trail_closure_save_regs(void *pred, MR_untrail_reason reason);

    extern void
    ML_call_trail_closure_on_backtrack(void *pred, MR_untrail_reason reason);

#line 174 "Mercury/cs/trail.c"
#line 175 "Mercury/cs/trail.c"

#endif
#line 178 "Mercury/cs/trail.c"

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


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_4 {
	MR_Word * f1[2];
	MR_Integer f2;
	MR_Word * f3[3];
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_4 mercury_common_4[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_5 {
	MR_Unsigned f1;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_5 mercury_common_5[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_6 {
	MR_Integer f1;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_6 mercury_common_6[];


#ifdef MR_MSVC
#pragma pack(push, MR_BYTES_PER_WORD)
#endif
struct mercury_type_3 {
	MR_String f1;
	MR_Integer f2;
};
#ifdef MR_MSVC
#pragma pack(pop)
#endif
MR_STATIC_LINKAGE const struct mercury_type_3 mercury_vector_common_3_0[];

extern const MR_TypeCtorInfo_Struct
	mercury_data_trail__type_ctor_info_choicepoint_id_0,
	mercury_data_trail__type_ctor_info_untrail_reason_0;
MR_decl_label1(trail__choicepoint_newer_2_0, 1)
MR_decl_label1(trail__debug_trail_1_0, 2)
MR_decl_label2(trail__debug_trail_pred_3_0, 2,3)
MR_decl_label6(trail__debug_trail_print_5_0, 2,5,6,7,9,10)
MR_decl_label5(trail__reason_name_2_0, 3,4,5,6,7)
MR_decl_label4(trail__reason_name_2_1, 3,4,5,1)
MR_decl_label2(__Compare___trail__untrail_reason_0_0, 2,3)
MR_def_extern_entry(trail__reason_name_2_0)
MR_def_extern_entry(trail__reason_name_2_1)
MR_def_extern_entry(trail__trail_closure_1_0)
MR_def_extern_entry(trail__trail_closure_io_3_0)
MR_def_extern_entry(trail__trail_closure_on_backtrack_1_0)
MR_def_extern_entry(trail__trail_closure_on_backtrack_io_3_0)
MR_def_extern_entry(fn__trail__current_choicepoint_id_0_0)
MR_def_extern_entry(fn__trail__null_choicepoint_id_0_0)
MR_def_extern_entry(trail__choicepoint_newer_2_0)
MR_def_extern_entry(fn__trail__choicepoint_id_to_int_1_0)
MR_def_extern_entry(trail__debug_trail_1_0)
MR_decl_static(trail__call_pred_3_0)
MR_decl_static(trail__call_trail_closure_4_0)
MR_decl_static(trail__debug_trail_pred_3_0)
MR_decl_static(trail__debug_trail_print_5_0)
MR_def_extern_entry(__Unify___trail__choicepoint_id_0_0)
MR_def_extern_entry(__Compare___trail__choicepoint_id_0_0)
MR_def_extern_entry(__Unify___trail__untrail_reason_0_0)
MR_def_extern_entry(__Compare___trail__untrail_reason_0_0)

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

static const MR_UserClosureId
mercury_data__closure_layout__trail__debug_trail_1_0_1;
extern const MR_TypeCtorInfo_Struct mercury_data_io__type_ctor_info_output_stream_0;
extern const MR_TypeCtorInfo_Struct mercury_data_trail__type_ctor_info_choicepoint_id_0;
extern const MR_TypeCtorInfo_Struct mercury_data_trail__type_ctor_info_untrail_reason_0;
static const struct mercury_type_4 mercury_common_4[1] =
{
{
{
(MR_Word *) &mercury_data__closure_layout__trail__debug_trail_1_0_1,
((MR_Word *) (MR_Integer) 0)
},
3,
{
MR_CTOR0_ADDR(io, output_stream),
MR_CTOR0_ADDR(trail, choicepoint_id),
MR_CTOR0_ADDR(trail, untrail_reason)
}
},
};

static const struct mercury_type_5 mercury_common_5[1] =
{
{
(((MR_Unsigned) (MR_Integer) 1) << (MR_Integer) 1)
},
};

static const struct mercury_type_6 mercury_common_6[2] =
{
{
10
},
{
0
},
};

static const struct mercury_type_3 mercury_vector_common_3_0[6] =
{
{
MR_string_const("commit", 6),
((MR_Integer) MR_commit)
},
{
MR_string_const("exception", 9),
((MR_Integer) MR_exception)
},
{
MR_string_const("gc", 2),
((MR_Integer) MR_gc)
},
{
MR_string_const("retry", 5),
((MR_Integer) MR_retry)
},
{
MR_string_const("solve", 5),
((MR_Integer) MR_solve)
},
{
MR_string_const("undo", 4),
((MR_Integer) MR_undo)
},
};

const MR_TypeCtorInfo_Struct mercury_data_trail__type_ctor_info_choicepoint_id_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___trail__choicepoint_id_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___trail__choicepoint_id_0_0)),
	"trail",
	"choicepoint_id",
	{ 0 },
	{ 0 },
	-1,
	0,
	NULL
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_0 = {
	"untrail_undo",
	0,
	MR_undo
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_1 = {
	"untrail_exception",
	1,
	MR_exception
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_2 = {
	"untrail_retry",
	2,
	MR_retry
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_3 = {
	"untrail_commit",
	3,
	MR_commit
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_4 = {
	"untrail_solve",
	4,
	MR_solve
};

static const MR_ForeignEnumFunctorDesc mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_5 = {
	"untrail_gc",
	5,
	MR_gc
};

const MR_ForeignEnumFunctorDescPtr mercury_data_trail__foreign_enum_ordinal_ordered_untrail_reason_0[] = {
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_0,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_1,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_2,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_3,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_4,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_5
};

const MR_ForeignEnumFunctorDescPtr mercury_data_trail__foreign_enum_name_ordered_untrail_reason_0[] = {
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_3,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_1,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_5,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_2,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_4,
	&mercury_data_trail__foreign_enum_functor_desc_untrail_reason_0_0
};

const MR_Integer mercury_data_trail__functor_number_map_untrail_reason_0[] = {
	5,
	1,
	3,
	0,
	4,
	2
};
	
const MR_TypeCtorInfo_Struct mercury_data_trail__type_ctor_info_untrail_reason_0 = {
	0,
	17,
	-1,
	MR_TYPECTOR_REP_FOREIGN_ENUM,
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Unify___trail__untrail_reason_0_0)),
	MR_MAYBE_STATIC_CODE(MR_ENTRY_AP(__Compare___trail__untrail_reason_0_0)),
	"trail",
	"untrail_reason",
	{ (void *) mercury_data_trail__foreign_enum_name_ordered_untrail_reason_0 },
	{ (void *) mercury_data_trail__foreign_enum_ordinal_ordered_untrail_reason_0 },
	6,
	4,
	mercury_data_trail__functor_number_map_untrail_reason_0
};


static const MR_UserClosureId
mercury_data__closure_layout__trail__debug_trail_1_0_1 = {
{
MR_PREDICATE,
"trail",
"trail",
"debug_trail_pred",
3,
0
},
"trail",
"trail.m",
279,
"10"
};

#line 168 "trail.m"

    void ML_call_trail_closure_save_regs(void *pred, MR_untrail_reason reason)
    {
        MR_Word     saved_regs[MR_MAX_FAKE_REG];

        /*
        ** The current implementation of trailing does not preserve live
        ** (real or fake) registers across calls to MR_reset_ticket.  Since
        ** the called Mercury code is likely to modify these, we better make
        ** a copy here and restore them afterwards.
        */
        MR_save_registers();
        MR_copy_fake_regs(MR_fake_reg, saved_regs);
        ML_call_trail_closure((MR_Word) pred, reason);
        MR_copy_fake_regs(saved_regs, MR_fake_reg);
        MR_restore_registers();
    }

    void ML_call_trail_closure_on_backtrack(void *pred,
        MR_untrail_reason reason)
    {
        MR_Word     saved_regs[MR_MAX_FAKE_REG];

        switch(reason) {
            case MR_undo:       /* Fall through. */
            case MR_exception:  /* Fall through. */
            case MR_retry:
                /*
                ** See comment in ML_call_trail_closure_save_regs, above.
                */
                MR_save_registers();
                MR_copy_fake_regs(MR_fake_reg, saved_regs);
                ML_call_pred((MR_Word) pred);
                MR_copy_fake_regs(saved_regs, MR_fake_reg);
                MR_restore_registers();
                break;

            case MR_solve:  /* Fall through */
            case MR_commit: 
                break;

            default:
                MR_fatal_error("trail.m: unknown MR_untrail_reason");
        }
    }

#line 581 "Mercury/cs/trail.c"


MR_BEGIN_MODULE(trail_module0)
	MR_init_entry1(trail__reason_name_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__reason_name_2_0);
	MR_init_label5(trail__reason_name_2_0,3,4,5,6,7)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'reason_name'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__reason_name_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if ((((MR_Integer) MR_r1) != ((MR_Integer) MR_commit))) {
		MR_GOTO_LAB(trail__reason_name_2_0_i3);
	}
	MR_r1 = ((MR_Word) MR_string_const("commit", 6));
	MR_proceed();
MR_def_label(trail__reason_name_2_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if ((((MR_Integer) MR_r1) != ((MR_Integer) MR_exception))) {
		MR_GOTO_LAB(trail__reason_name_2_0_i4);
	}
	MR_r1 = ((MR_Word) MR_string_const("exception", 9));
	MR_proceed();
MR_def_label(trail__reason_name_2_0,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if ((((MR_Integer) MR_r1) != ((MR_Integer) MR_gc))) {
		MR_GOTO_LAB(trail__reason_name_2_0_i5);
	}
	MR_r1 = ((MR_Word) MR_string_const("gc", 2));
	MR_proceed();
MR_def_label(trail__reason_name_2_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if ((((MR_Integer) MR_r1) != ((MR_Integer) MR_retry))) {
		MR_GOTO_LAB(trail__reason_name_2_0_i6);
	}
	MR_r1 = ((MR_Word) MR_string_const("retry", 5));
	MR_proceed();
MR_def_label(trail__reason_name_2_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if ((((MR_Integer) MR_r1) != ((MR_Integer) MR_solve))) {
		MR_GOTO_LAB(trail__reason_name_2_0_i7);
	}
	MR_r1 = ((MR_Word) MR_string_const("solve", 5));
	MR_proceed();
MR_def_label(trail__reason_name_2_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_string_const("undo", 4));
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module1)
	MR_init_entry1(trail__reason_name_2_1);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__reason_name_2_1);
	MR_init_label4(trail__reason_name_2_1,3,4,5,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'reason_name'/2 mode 1 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__reason_name_2_1);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r3 = (MR_Integer) 0;
	MR_r4 = (MR_Integer) 5;
MR_def_label(trail__reason_name_2_1,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	while (1) {
	if ((((MR_Integer) MR_r3) > ((MR_Integer) MR_r4))) {
		MR_GOTO_LAB(trail__reason_name_2_1_i1);
	}
	{
	MR_Word MR_tempr1;
	MR_tempr1 = ((MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r3) + (MR_Unsigned) ((MR_Integer) MR_r4)) / (MR_Integer) 2);
	MR_r5 = MR_tempr1;
	MR_r6 = MR_strcmp(((MR_Word *) MR_r1), ((MR_Word *) ((MR_Word *) &mercury_vector_common_3_0)[(MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_tempr1) * (MR_Unsigned) (MR_Integer) 2)]));
	if (MR_INT_GE(MR_r6,0)) {
		MR_GOTO_LAB(trail__reason_name_2_1_i4);
	}
	MR_r4 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_tempr1) - (MR_Unsigned) (MR_Integer) 1);
	continue;
	}
	break;
	} /* end while */
MR_def_label(trail__reason_name_2_1,4)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	if (MR_INT_LE(MR_r6,0)) {
		MR_GOTO_LAB(trail__reason_name_2_1_i5);
	}
	MR_r3 = (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r5) + (MR_Unsigned) (MR_Integer) 1);
	MR_GOTO_LAB(trail__reason_name_2_1_i3);
MR_def_label(trail__reason_name_2_1,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = ((MR_Word) &MR_tfield(0, (MR_Word *) &mercury_vector_common_3_0, (MR_Integer) ((MR_Unsigned) ((MR_Integer) MR_r5) * (MR_Unsigned) (MR_Integer) 2)));
	MR_r2 = MR_tfield(0, MR_tempr1, 1);
	MR_r1 = MR_TRUE;
	MR_proceed();
	}
MR_def_label(trail__reason_name_2_1,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module2)
	MR_init_entry1(trail__trail_closure_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__trail_closure_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'trail_closure'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__trail_closure_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word	Pred;
#define	MR_PROC_LABEL	mercury__trail__trail_closure_1_0
	Pred = MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("trail_closure");
{
#line 129 "trail.m"

    MR_trail_function(ML_call_trail_closure_save_regs, (void *) Pred);
;}
#line 727 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("trail_closure");
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module3)
	MR_init_entry1(trail__trail_closure_io_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__trail_closure_io_3_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'trail_closure_io'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__trail_closure_io_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word	Pred;
#define	MR_PROC_LABEL	mercury__trail__trail_closure_io_3_0
	Pred = MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("trail_closure_io");
{
#line 136 "trail.m"

    MR_trail_function(ML_call_trail_closure_save_regs, (void *) Pred);
;}
#line 762 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("trail_closure_io");
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module4)
	MR_init_entry1(trail__trail_closure_on_backtrack_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__trail_closure_on_backtrack_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'trail_closure_on_backtrack'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__trail_closure_on_backtrack_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word	Pred;
#define	MR_PROC_LABEL	mercury__trail__trail_closure_on_backtrack_1_0
	Pred = MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("trail_closure_on_backtrack");
{
#line 143 "trail.m"

    MR_trail_function(ML_call_trail_closure_on_backtrack, (void *) Pred);
;}
#line 797 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("trail_closure_on_backtrack");
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module5)
	MR_init_entry1(trail__trail_closure_on_backtrack_io_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__trail_closure_on_backtrack_io_3_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'trail_closure_on_backtrack_io'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__trail_closure_on_backtrack_io_3_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word	Pred;
#define	MR_PROC_LABEL	mercury__trail__trail_closure_on_backtrack_io_3_0
	Pred = MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("trail_closure_on_backtrack_io");
{
#line 151 "trail.m"

    MR_trail_function(ML_call_trail_closure_on_backtrack, (void *) Pred);
;}
#line 832 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("trail_closure_on_backtrack_io");
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module6)
	MR_init_entry1(fn__trail__current_choicepoint_id_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__trail__current_choicepoint_id_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'current_choicepoint_id'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__trail__current_choicepoint_id_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__fn__trail__current_choicepoint_id_0_0
	MR_OBTAIN_GLOBAL_LOCK("current_choicepoint_id");
{
#line 247 "trail.m"

    Id = MR_current_choicepoint_id();
;}
#line 866 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("current_choicepoint_id");
	MR_r1 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module7)
	MR_init_entry1(fn__trail__null_choicepoint_id_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__trail__null_choicepoint_id_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'null_choicepoint_id'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__trail__null_choicepoint_id_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__fn__trail__null_choicepoint_id_0_0
{
#line 254 "trail.m"

    Id = MR_null_choicepoint_id();
;}
#line 900 "Mercury/cs/trail.c"
	MR_r1 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module8)
	MR_init_entry1(trail__choicepoint_newer_2_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__choicepoint_newer_2_0);
	MR_init_label1(trail__choicepoint_newer_2_0,1)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'choicepoint_newer'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__choicepoint_newer_2_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_ChoicepointId	A;
	MR_ChoicepointId	B;
#define	MR_PROC_LABEL	mercury__trail__choicepoint_newer_2_0
	MR_bool MercurySuccessIndicator;
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MercurySuccessIndicator
	A = (MR_ChoicepointId) MR_r1;
	B = (MR_ChoicepointId) MR_r2;
{
#line 261 "trail.m"

    SUCCESS_INDICATOR = MR_choicepoint_newer(A, B);
;}
#line 940 "Mercury/cs/trail.c"
if (!MercurySuccessIndicator) MR_GOTO_LAB(trail__choicepoint_newer_2_0_i1);
#undef SUCCESS_INDICATOR
#define SUCCESS_INDICATOR MR_r1
#undef	MR_PROC_LABEL
	}
	MR_r1 = MR_TRUE;
	MR_proceed();
MR_def_label(trail__choicepoint_newer_2_0,1)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_FALSE;
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module9)
	MR_init_entry1(fn__trail__choicepoint_id_to_int_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__fn__trail__choicepoint_id_to_int_1_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'choicepoint_id_to_int'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__fn__trail__choicepoint_id_to_int_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_ChoicepointId	CP;
	MR_Integer	N;
#define	MR_PROC_LABEL	mercury__fn__trail__choicepoint_id_to_int_1_0
	CP = (MR_ChoicepointId) MR_r1;
{
#line 268 "trail.m"

    N = (MR_Integer) CP;
;}
#line 982 "Mercury/cs/trail.c"
	MR_r1 = N;
#undef	MR_PROC_LABEL
	}
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module10)
	MR_init_entry1(trail__debug_trail_1_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__debug_trail_1_0);
	MR_init_label1(trail__debug_trail_1_0,2)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'debug_trail'/1 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury__trail__debug_trail_1_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(3);
	MR_sv(3) = ((MR_Word) MR_succip);
	{
	MR_ChoicepointId	Id;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_1_0
	MR_OBTAIN_GLOBAL_LOCK("current_choicepoint_id");
{
#line 247 "trail.m"

    Id = MR_current_choicepoint_id();
;}
#line 1019 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("current_choicepoint_id");
	MR_r3 = (MR_Word) Id;
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r3;
	MR_r2 = ((MR_Word) MR_string_const("setup", 5));
	MR_np_call_localret_ent(trail__debug_trail_print_5_0,
		trail__debug_trail_1_0_i2);
MR_def_label(trail__debug_trail_1_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_tag_alloc_heap(MR_r1, 0, (MR_Integer) 5);
	MR_tfield(0, MR_r1, 0) = ((MR_Word) MR_COMMON(4,0));
	MR_tfield(0, MR_r1, 1) = ((MR_Word) MR_ENTRY_AP(trail__debug_trail_pred_3_0));
	MR_tfield(0, MR_r1, 2) = (MR_Integer) 2;
	MR_tfield(0, MR_r1, 3) = MR_sv(1);
	MR_tfield(0, MR_r1, 4) = MR_sv(2);
	{
	MR_Word	Pred;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_1_0
	Pred = MR_r1;
	MR_OBTAIN_GLOBAL_LOCK("trail_closure");
{
#line 129 "trail.m"

    MR_trail_function(ML_call_trail_closure_save_regs, (void *) Pred);
;}
#line 1047 "Mercury/cs/trail.c"
	MR_RELEASE_GLOBAL_LOCK("trail_closure");
#undef	MR_PROC_LABEL
	}
	MR_decr_sp_and_return(3);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_declare_entry(mercury__do_call_closure_1);

MR_BEGIN_MODULE(trail_module11)
	MR_init_entry1(trail__call_pred_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__call_pred_3_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'call_pred'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(trail__call_pred_3_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(trail__call_pred_3_0));
	MR_np_tailcall_ent(do_call_closure_1);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_declare_entry(mercury__do_call_closure_2);

MR_BEGIN_MODULE(trail_module12)
	MR_init_entry1(trail__call_trail_closure_4_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__call_trail_closure_4_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'call_trail_closure'/4 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(trail__call_trail_closure_4_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_set_prof_ho_caller_proc(MR_ENTRY_AP(trail__call_trail_closure_4_0));
	MR_np_tailcall_ent(do_call_closure_2);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(builtin__impure_true_0_0);

MR_BEGIN_MODULE(trail_module13)
	MR_init_entry1(trail__debug_trail_pred_3_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__debug_trail_pred_3_0);
	MR_init_label2(trail__debug_trail_pred_3_0,2,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'debug_trail_pred'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(trail__debug_trail_pred_3_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_sv(3) = MR_r3;
	MR_np_call_localret_ent(builtin__impure_true_0_0,
		trail__debug_trail_pred_3_0_i2);
MR_def_label(trail__debug_trail_pred_3_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(3);
	MR_np_call_localret_ent(trail__reason_name_2_0,
		trail__debug_trail_pred_3_0_i3);
MR_def_label(trail__debug_trail_pred_3_0,3)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MR_Word MR_tempr1;
	MR_tempr1 = MR_r1;
	MR_r1 = MR_sv(1);
	MR_r2 = MR_tempr1;
	MR_r3 = MR_sv(2);
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(trail__debug_trail_print_5_0);
	}
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(io__throw_on_error_4_0);
MR_decl_entry(string__format__format_string_component_5_0);
MR_decl_entry(string__format__format_signed_int_component_5_0);
MR_decl_entry(io__write_string_4_0);

MR_BEGIN_MODULE(trail_module14)
	MR_init_entry1(trail__debug_trail_print_5_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury__trail__debug_trail_print_5_0);
	MR_init_label6(trail__debug_trail_print_5_0,2,5,6,7,9,10)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for 'debug_trail_print'/5 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_def_static(trail__debug_trail_print_5_0)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(4);
	MR_sv(4) = ((MR_Word) MR_succip);
	{
	MR_Word MR_tempr1, MR_tempr2;
	{
	MR_ChoicepointId	CP;
	MR_Integer	N;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_print_5_0
	CP = (MR_ChoicepointId) MR_r3;
{
#line 268 "trail.m"

    N = (MR_Integer) CP;
;}
#line 1183 "Mercury/cs/trail.c"
	MR_tempr1 = N;
#undef	MR_PROC_LABEL
	}
	{
	MercuryFilePtr	Stream;
	MR_String	Message;
	MR_Integer	Error;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_print_5_0
	Stream = (MercuryFilePtr) MR_r1;
	Message = (MR_String) ((MR_Word) MR_string_const("TRAIL: ", 7));
{
#line 938 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/io.opt"

    const char *s = Message;
    if (ML_fprintf(Stream, "%s", s) < 0) {
        Error = errno;
    } else {
        Error = 0;
        while (*s) {
            if (*s++ == '\n') {
                MR_line_number(*Stream)++;
            }
        }
    }
;}
#line 1209 "Mercury/cs/trail.c"
	MR_MAYBE_BOX_FOREIGN_TYPE(MR_Integer, Error, MR_tempr2);
#undef	MR_PROC_LABEL
	}
	MR_sv(1) = MR_r1;
	MR_sv(2) = MR_r2;
	MR_sv(3) = MR_tempr1;
	MR_r1 = MR_tempr2;
	MR_r2 = ((MR_Word) MR_string_const("error writing to output file: ", 30));
	}
	MR_np_call_localret_ent(io__throw_on_error_4_0,
		trail__debug_trail_print_5_0_i2);
MR_def_label(trail__debug_trail_print_5_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,5,0));
	MR_r2 = ((MR_Word) MR_TAG_COMMON(1,6,0));
	MR_r3 = (MR_Unsigned) 0U;
	MR_r4 = MR_sv(2);
	MR_np_call_localret_ent(string__format__format_string_component_5_0,
		trail__debug_trail_print_5_0_i5);
MR_def_label(trail__debug_trail_print_5_0,5)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MercuryFilePtr	Stream;
	MR_String	Message;
	MR_Integer	Error;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_print_5_0
	Stream = (MercuryFilePtr) MR_sv(1);
	Message = (MR_String) MR_r1;
{
#line 938 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/io.opt"

    const char *s = Message;
    if (ML_fprintf(Stream, "%s", s) < 0) {
        Error = errno;
    } else {
        Error = 0;
        while (*s) {
            if (*s++ == '\n') {
                MR_line_number(*Stream)++;
            }
        }
    }
;}
#line 1253 "Mercury/cs/trail.c"
	MR_MAYBE_BOX_FOREIGN_TYPE(MR_Integer, Error, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_r2 = ((MR_Word) MR_string_const("error writing to output file: ", 30));
	MR_np_call_localret_ent(io__throw_on_error_4_0,
		trail__debug_trail_print_5_0_i6);
MR_def_label(trail__debug_trail_print_5_0,6)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MercuryFilePtr	Stream;
	MR_String	Message;
	MR_Integer	Error;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_print_5_0
	Stream = (MercuryFilePtr) MR_sv(1);
	Message = (MR_String) ((MR_Word) MR_string_const(" ", 1));
{
#line 938 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/io.opt"

    const char *s = Message;
    if (ML_fprintf(Stream, "%s", s) < 0) {
        Error = errno;
    } else {
        Error = 0;
        while (*s) {
            if (*s++ == '\n') {
                MR_line_number(*Stream)++;
            }
        }
    }
;}
#line 1284 "Mercury/cs/trail.c"
	MR_MAYBE_BOX_FOREIGN_TYPE(MR_Integer, Error, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_r2 = ((MR_Word) MR_string_const("error writing to output file: ", 30));
	MR_np_call_localret_ent(io__throw_on_error_4_0,
		trail__debug_trail_print_5_0_i7);
MR_def_label(trail__debug_trail_print_5_0,7)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = ((MR_Word) MR_TAG_COMMON(0,6,1));
	MR_r2 = (MR_Unsigned) 0U;
	MR_r3 = (MR_Unsigned) 0U;
	MR_r4 = MR_sv(3);
	MR_np_call_localret_ent(string__format__format_signed_int_component_5_0,
		trail__debug_trail_print_5_0_i9);
MR_def_label(trail__debug_trail_print_5_0,9)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	{
	MercuryFilePtr	Stream;
	MR_String	Message;
	MR_Integer	Error;
#define	MR_PROC_LABEL	mercury__trail__debug_trail_print_5_0
	Stream = (MercuryFilePtr) MR_sv(1);
	Message = (MR_String) MR_r1;
{
#line 938 "C:/cygwin/usr/local/mercury/lib/mercury/ints/asm_fast.gc.tr/Mercury/opts/io.opt"

    const char *s = Message;
    if (ML_fprintf(Stream, "%s", s) < 0) {
        Error = errno;
    } else {
        Error = 0;
        while (*s) {
            if (*s++ == '\n') {
                MR_line_number(*Stream)++;
            }
        }
    }
;}
#line 1323 "Mercury/cs/trail.c"
	MR_MAYBE_BOX_FOREIGN_TYPE(MR_Integer, Error, MR_r1);
#undef	MR_PROC_LABEL
	}
	MR_r2 = ((MR_Word) MR_string_const("error writing to output file: ", 30));
	MR_np_call_localret_ent(io__throw_on_error_4_0,
		trail__debug_trail_print_5_0_i10);
MR_def_label(trail__debug_trail_print_5_0,10)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = MR_sv(1);
	MR_r2 = ((MR_Word) MR_string_const("\n", 1));
	MR_succip_word = MR_sv(4);
	MR_decr_sp(4);
	MR_np_tailcall_ent(io__write_string_4_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Unify___builtin__c_pointer_0_0);

MR_BEGIN_MODULE(trail_module15)
	MR_init_entry1(__Unify___trail__choicepoint_id_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___trail__choicepoint_id_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___trail__choicepoint_id_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Unify___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_entry(__Compare___builtin__c_pointer_0_0);

MR_BEGIN_MODULE(trail_module16)
	MR_init_entry1(__Compare___trail__choicepoint_id_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___trail__choicepoint_id_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___trail__choicepoint_id_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_np_tailcall_ent(__Compare___builtin__c_pointer_0_0);
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module17)
	MR_init_entry1(__Unify___trail__untrail_reason_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Unify___trail__untrail_reason_0_0);
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Unify__'/2 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Unify___trail__untrail_reason_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_r1 = (MR_r1 == MR_r2);
	MR_proceed();
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE


MR_BEGIN_MODULE(trail_module18)
	MR_init_entry1(__Compare___trail__untrail_reason_0_0);
	MR_INIT_PROC_LAYOUT_ADDR(mercury____Compare___trail__untrail_reason_0_0);
	MR_init_label2(__Compare___trail__untrail_reason_0_0,2,3)
MR_BEGIN_CODE

/*-------------------------------------------------------------------------*/
/* code for '__Compare__'/3 mode 0 */
#ifdef MR_maybe_local_thread_engine_base
	#undef MR_maybe_local_thread_engine_base
	#define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif
MR_define_entry(mercury____Compare___trail__untrail_reason_0_0);
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_incr_sp(1);
	MR_r3 = MR_r2;
	MR_r2 = MR_r1;
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) >= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___trail__untrail_reason_0_0_i2);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 1;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___trail__untrail_reason_0_0,2)
	MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE
	MR_reset_ticket(MR_sv(1), MR_undo);
	MR_discard_ticket();
	MR_store_ticket(MR_sv(1));
	if ((((MR_Integer) MR_r2) <= ((MR_Integer) MR_r3))) {
		MR_GOTO_LAB(__Compare___trail__untrail_reason_0_0_i3);
	}
	MR_reset_ticket(MR_sv(1), MR_commit);
	MR_prune_ticket();
	MR_r1 = (MR_Integer) 2;
	MR_decr_sp(1);
	MR_proceed();
MR_def_label(__Compare___trail__untrail_reason_0_0,3)
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

MR_declare_static(mercury__trail__call_pred_3_0);

void
ML_call_pred(MR_Word Mercury__argument1);

void
ML_call_pred(MR_Word Mercury__argument1)
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

	MR_save_regs_to_mem(c_regs);
#if MR_THREAD_SAFE
	must_finalize_engine = MR_init_thread(MR_use_now);
#endif
#if MR_DEEP_PROFILING
	saved_cur_callback = MR_current_callback_site;
	saved_cur_csd = MR_current_call_site_dynamic;
	MR_setup_callback(MR_ENTRY(mercury__trail__call_pred_3_0));
#endif
	MR_restore_registers();
	MR_r1 = Mercury__argument1;
	MR_save_transient_registers();
	(void) MR_call_engine(MR_ENTRY(mercury__trail__call_pred_3_0), MR_FALSE);
	MR_restore_transient_registers();
#if MR_DEEP_PROFILING
	MR_current_call_site_dynamic = saved_cur_csd;
	MR_current_callback_site = saved_cur_callback;
#endif
#if MR_THREAD_SAFE
	if (must_finalize_engine) {
		 MR_finalize_thread_engine();
	}
#endif
	MR_restore_regs_from_mem(c_regs);
}


MR_declare_static(mercury__trail__call_trail_closure_4_0);

void
ML_call_trail_closure(MR_Word Mercury__argument1, MR_Word Mercury__argument2);

void
ML_call_trail_closure(MR_Word Mercury__argument1, MR_Word Mercury__argument2)
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

	MR_save_regs_to_mem(c_regs);
#if MR_THREAD_SAFE
	must_finalize_engine = MR_init_thread(MR_use_now);
#endif
#if MR_DEEP_PROFILING
	saved_cur_callback = MR_current_callback_site;
	saved_cur_csd = MR_current_call_site_dynamic;
	MR_setup_callback(MR_ENTRY(mercury__trail__call_trail_closure_4_0));
#endif
	MR_restore_registers();
	MR_r1 = Mercury__argument1;
	MR_r2 = Mercury__argument2;
	MR_save_transient_registers();
	(void) MR_call_engine(MR_ENTRY(mercury__trail__call_trail_closure_4_0), MR_FALSE);
	MR_restore_transient_registers();
#if MR_DEEP_PROFILING
	MR_current_call_site_dynamic = saved_cur_csd;
	MR_current_callback_site = saved_cur_callback;
#endif
#if MR_THREAD_SAFE
	if (must_finalize_engine) {
		 MR_finalize_thread_engine();
	}
#endif
	MR_restore_regs_from_mem(c_regs);
}


static void mercury__trail_maybe_bunch_0(void)
{
	trail_module0();
	trail_module1();
	trail_module2();
	trail_module3();
	trail_module4();
	trail_module5();
	trail_module6();
	trail_module7();
	trail_module8();
	trail_module9();
	trail_module10();
	trail_module11();
	trail_module12();
	trail_module13();
	trail_module14();
	trail_module15();
	trail_module16();
	trail_module17();
	trail_module18();
}

/* suppress gcc -Wmissing-decls warnings */
void mercury__trail__init(void);
void mercury__trail__init_type_tables(void);
void mercury__trail__init_debugger(void);
#ifdef MR_DEEP_PROFILING
void mercury__trail__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp);
#endif
#ifdef MR_RECORD_TERM_SIZES
void mercury__trail__init_complexity_procs(void);
#endif
#ifdef MR_THREADSCOPE
void mercury__trail__init_threadscope_string_table(void);
#endif
const char *mercury__trail__grade_check(void);

void mercury__trail__init(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	mercury__trail_maybe_bunch_0();
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_trail__type_ctor_info_choicepoint_id_0,
		trail__choicepoint_id_0_0);
	MR_INIT_TYPE_CTOR_INFO(
		mercury_data_trail__type_ctor_info_untrail_reason_0,
		trail__untrail_reason_0_0);
	mercury__trail__init_debugger();
}

void mercury__trail__init_type_tables(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
	{
		MR_register_type_ctor_info(
		&mercury_data_trail__type_ctor_info_choicepoint_id_0);
	}
	{
		MR_register_type_ctor_info(
		&mercury_data_trail__type_ctor_info_untrail_reason_0);
	}
}


void mercury__trail__init_debugger(void)
{
	static MR_bool done = MR_FALSE;
	if (done) {
		return;
	}
	done = MR_TRUE;
}

#ifdef MR_DEEP_PROFILING

void mercury__trail__write_out_proc_statics(FILE *deep_fp, FILE *procrep_fp)
{
	MR_write_out_module_proc_reps_start(procrep_fp, &mercury_data__module_layout__trail);
	MR_write_out_module_proc_reps_end(procrep_fp);
}

#endif

#ifdef MR_RECORD_TERM_SIZES

void mercury__trail__init_complexity_procs(void)
{
}

#endif

#ifdef MR_THREADSCOPE

void mercury__trail__init_threadscope_string_table(void)
{
}

#endif

// Ensure everything is compiled with the same grade.
const char *mercury__trail__grade_check(void)
{
    return &MR_GRADE_VAR;
}
