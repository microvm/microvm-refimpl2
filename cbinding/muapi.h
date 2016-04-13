#ifndef __MUAPI_H__
#define __MUAPI_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

// MuValue and MuXxxValue type are opaque handles to values in the Mu type
// system.
//
// The actual values are held by MuCtx. MuValue opaquely refers to one such
// value. Copies of MuValue values refer to the same value. A MuValue instance
// can only be used in the MuCtx holding it.
//
// Values of subtypes can be cast to/from their abstract parents using the type
// cast expression in C, similar to casting one pointer to another.

// abstract value types
typedef void *MuValue;              // Any Mu value
typedef void *MuSeqValue;           // array or vector
typedef void *MuGenRefValue;        // ref, iref, funcref, threadref, stackref, framecursorref

// concrete value types
typedef void *MuIntValue;           // int<n>
typedef void *MuFloatValue;         // float
typedef void *MuDoubleValue;        // double
typedef void *MuRefValue;           // ref<T>
typedef void *MuIRefValue;          // iref<T>
typedef void *MuStructValue;        // struct<...>
typedef void *MuArrayValue;         // array<T l>
typedef void *MuVectorValue;        // vector<T l>
typedef void *MuFuncRefValue;       // funcref<sig>
typedef void *MuThreadRefValue;     // threadref
typedef void *MuStackRefValue;      // stackref
typedef void *MuFCRefValue;         // framecursorref
typedef void *MuTagRef64Value;      // tagref64
typedef void *MuUPtrValue;          // uptr
typedef void *MuUFPValue;           // ufuncptr

// Identifiers and names of Mu
typedef uint32_t MuID;
typedef char *MuName;

// Convenient types for the void* type and the void(*)() type in C
typedef void *MuCPtr;
typedef void (*MuCFP)();

// Result of a trap handler
typedef int MuTrapHandlerResult;

// Used by new_thread
typedef int MuHowToResume;

// Values or MuTrapHandlerResult
#define MU_THREAD_EXIT          0x00
// Values or MuTrapHandlerResult and muHowToResume
#define MU_REBIND_PASS_VALUES   0x01
#define MU_REBIND_THROW_EXC     0x02

// Used by MuTrapHandler
typedef void (*MuValuesFreer)(MuValue *values, MuCPtr freerdata);

// Declare the types here because they are used in the following signatures.
typedef struct MuVM MuVM;
typedef struct MuCtx MuCtx;

// Signature of the trap handler
typedef void (*MuTrapHandler)(
        // iutput parameters
        MuCtx *ctx,
        MuThreadRefValue thread,
        MuStackRefValue stack,
        int wpid,
        // output parameters
        MuTrapHandlerResult *result,
        MuStackRefValue *new_stack,
        MuValue **values,
        int *nvalues,
        MuValuesFreer *freer,
        MuCPtr *freerdata,
        MuRefValue *exception,
        // input parameter (userdata)
        MuCPtr userdata);

// Memory orders
typedef int MuMemOrd;

// Values of MuMemOrd
#define MU_NOT_ATOMIC  0x00
#define MU_RELAXED     0x01
#define MU_CONSUME     0x02
#define MU_ACQUIRE     0x03
#define MU_RELEASE     0x04
#define MU_ACQ_REL     0x05
#define MU_SEQ_CST     0x06

// Operations for the atomicrmw API function
typedef int MuAtomicRMWOp;

// Values of MuAtomicRMWOp
#define MU_XCHG        0x00
#define MU_ADD         0x01
#define MU_SUB         0x02
#define MU_AND         0x03
#define MU_NAND        0x04
#define MU_OR          0x05
#define MU_XOR         0x06
#define MU_MAX         0x07
#define MU_MIN         0x08
#define MU_UMAX        0x09
#define MU_UMIN        0x0A

// Calling conventions.
typedef int MuCallConv;

#define MU_DEFAULT     0x00
// Concrete Mu implementations may define more calling conventions.

// NOTE: MuVM and MuCtx are structures with many function pointers. This
// approach loosens the coupling between the client module and the Mu
// implementation.  At compile time, the client does not need to link against
// any dynamic libraries. At run time, more than one Mu implementations can be
// used by the same client.

// A handle and method lists of a micro VM
struct MuVM {
    void *header;   // Refer to internal stuff

    // Create context
    MuCtx*  (*new_context)(MuVM *mvm);
    
    // Convert between IDs and names
    MuID    (*id_of  )(MuVM *mvm, MuName name);
    MuName  (*name_of)(MuVM *mvm, MuID id);

    // Set handlers
    void    (*set_trap_handler      )(MuVM *mvm, MuTrapHandler trap_handler, MuCPtr userdata);

    // Proprietary API: let the micro VM execute
    void    (*execute)(MuVM *mvm);
    int*    (*get_mu_error_ptr)(MuVM *mvm);
};

// A local context. It can only be used by one thread at a time. It holds many
// states which are typically held by a Mu thread, such as object references,
// local heap allocation pool, and an object-pinning set. It also holds many Mu
// values and expose them to the client as opaque handles (MuValue and its
// subtypes).
struct MuCtx {
    void *header;   // Refer to internal stuff

    // Convert between IDs and names
    MuID        (*id_of  )(MuCtx *ctx, MuName name);
    MuName      (*name_of)(MuCtx *ctx, MuID id);

    // Close the current context, releasing all resources
    void        (*close_context)(MuCtx *ctx);

    // Load bundles and HAIL scripts
    void        (*load_bundle)(MuCtx *ctx, char *buf, int sz);
    void        (*load_hail  )(MuCtx *ctx, char *buf, int sz);

    // Convert from C values to Mu values
    MuIntValue      (*handle_from_sint8 )(MuCtx *ctx, int8_t   num, int len);
    MuIntValue      (*handle_from_uint8 )(MuCtx *ctx, uint8_t  num, int len);
    MuIntValue      (*handle_from_sint16)(MuCtx *ctx, int16_t  num, int len);
    MuIntValue      (*handle_from_uint16)(MuCtx *ctx, uint16_t num, int len);
    MuIntValue      (*handle_from_sint32)(MuCtx *ctx, int32_t  num, int len);
    MuIntValue      (*handle_from_uint32)(MuCtx *ctx, uint32_t num, int len);
    MuIntValue      (*handle_from_sint64)(MuCtx *ctx, int64_t  num, int len);
    MuIntValue      (*handle_from_uint64)(MuCtx *ctx, uint64_t num, int len);
    MuFloatValue    (*handle_from_float )(MuCtx *ctx, float    num);
    MuDoubleValue   (*handle_from_double)(MuCtx *ctx, double   num);
    MuUPtrValue     (*handle_from_ptr   )(MuCtx *ctx, MuID mu_type, MuCPtr ptr);
    MuUFPValue      (*handle_from_fp    )(MuCtx *ctx, MuID mu_type, MuCFP fp);

    // Convert from Mu values to C values
    int8_t      (*handle_to_sint8 )(MuCtx *ctx, MuIntValue    opnd);
    uint8_t     (*handle_to_uint8 )(MuCtx *ctx, MuIntValue    opnd);
    int16_t     (*handle_to_sint16)(MuCtx *ctx, MuIntValue    opnd);
    uint16_t    (*handle_to_uint16)(MuCtx *ctx, MuIntValue    opnd);
    int32_t     (*handle_to_sint32)(MuCtx *ctx, MuIntValue    opnd);
    uint32_t    (*handle_to_uint32)(MuCtx *ctx, MuIntValue    opnd);
    int64_t     (*handle_to_sint64)(MuCtx *ctx, MuIntValue    opnd);
    uint64_t    (*handle_to_uint64)(MuCtx *ctx, MuIntValue    opnd);
    float       (*handle_to_float )(MuCtx *ctx, MuFloatValue  opnd);
    double      (*handle_to_double)(MuCtx *ctx, MuDoubleValue opnd);
    MuCPtr      (*handle_to_ptr   )(MuCtx *ctx, MuUPtrValue   opnd);
    MuCFP       (*handle_to_fp    )(MuCtx *ctx, MuUFPValue    opnd);

    // Make MuValue instances from Mu global SSA variables
    MuValue         (*handle_from_const )(MuCtx *ctx, MuID id);
    MuIRefValue     (*handle_from_global)(MuCtx *ctx, MuID id);
    MuFuncRefValue  (*handle_from_func  )(MuCtx *ctx, MuID id);
    MuValue         (*handle_from_expose)(MuCtx *ctx, MuID id);

    // Delete the value held by the MuCtx, making it unusable, but freeing up
    // the resource.
    void        (*delete_value)(MuCtx *ctx, MuValue opnd);

    // Compare reference or general reference types.
    // EQ. Available for ref, iref, funcref, threadref and stackref.
    int         (*ref_eq )(MuCtx *ctx, MuGenRefValue lhs, MuGenRefValue rhs);
    // ULT. Available for iref only.
    int         (*ref_ult)(MuCtx *ctx, MuIRefValue lhs, MuIRefValue rhs);

    // Manipulate Mu values of the struct<...> type
    MuValue         (*extract_value)(MuCtx *ctx, MuStructValue str, int index);
    MuStructValue   (*insert_value )(MuCtx *ctx, MuStructValue str, int index, MuValue newval);

    // Manipulate Mu values of the array or vector type
    // str can be MuArrayValue or MuVectorValue
    MuValue     (*extract_element)(MuCtx *ctx, MuSeqValue str, MuIntValue index);
    MuSeqValue  (*insert_element )(MuCtx *ctx, MuSeqValue str, MuIntValue index, MuValue newval);

    // Heap allocation
    MuRefValue  (*new_fixed )(MuCtx *ctx, MuID mu_type);
    MuRefValue  (*new_hybrid)(MuCtx *ctx, MuID mu_type, MuIntValue length);

    // Change the T or sig in ref<T>, iref<T> or func<sig>
    MuGenRefValue   (*refcast)(MuCtx *ctx, MuGenRefValue opnd, MuID new_type);

    // Memory addressing
    MuIRefValue     (*get_iref           )(MuCtx *ctx, MuRefValue opnd);
    MuIRefValue     (*get_field_iref     )(MuCtx *ctx, MuIRefValue opnd, int field);
    MuIRefValue     (*get_elem_iref      )(MuCtx *ctx, MuIRefValue opnd, MuIntValue index);
    MuIRefValue     (*shift_iref         )(MuCtx *ctx, MuIRefValue opnd, MuIntValue offset);
    MuIRefValue     (*get_var_part_iref  )(MuCtx *ctx, MuIRefValue opnd);

    // Memory accessing
    MuValue     (*load     )(MuCtx *ctx, MuMemOrd ord, MuIRefValue loc);
    void        (*store    )(MuCtx *ctx, MuMemOrd ord, MuIRefValue loc, MuValue newval);
    MuValue     (*cmpxchg  )(MuCtx *ctx, MuMemOrd ord_succ, MuMemOrd ord_fail,
                        int weak, MuIRefValue loc, MuValue expected, MuValue desired,
                        int *is_succ);
    MuValue     (*atomicrmw)(MuCtx *ctx, MuMemOrd ord, MuAtomicRMWOp op,
                        MuIRefValue loc, MuValue opnd);
    void        (*fence    )(MuCtx *ctx, MuMemOrd ord);

    // Thread and stack creation and stack destruction
    MuStackRefValue     (*new_stack )(MuCtx *ctx, MuFuncRefValue func);
    MuThreadRefValue    (*new_thread)(MuCtx *ctx, MuStackRefValue stack,
                            MuHowToResume htr, MuValue *vals, int nvals, MuRefValue exc);
    void                (*kill_stack)(MuCtx *ctx, MuStackRefValue stack);

    // Frame cursor operations
    MuFCRefValue    (*new_cursor  )(MuCtx *ctx, MuStackRefValue stack);
    void            (*next_frame  )(MuCtx *ctx, MuFCRefValue cursor);
    MuFCRefValue    (*copy_cursor )(MuCtx *ctx, MuFCRefValue cursor);
    void            (*close_cursor)(MuCtx *ctx, MuFCRefValue cursor);

    // Stack introspection
    MuID        (*cur_func       )(MuCtx *ctx, MuFCRefValue cursor);
    MuID        (*cur_func_ver   )(MuCtx *ctx, MuFCRefValue cursor);
    MuID        (*cur_inst       )(MuCtx *ctx, MuFCRefValue cursor);
    void        (*dump_keepalives)(MuCtx *ctx, MuFCRefValue cursor, MuValue *results);
    
    // On-stack replacement
    void        (*pop_frames_to)(MuCtx *ctx, MuFCRefValue cursor);
    void        (*push_frame   )(MuCtx *ctx, MuStackRefValue stack, MuFuncRefValue func);

    // 64-bit tagged reference operations
    int             (*tr64_is_fp   )(MuCtx *ctx, MuTagRef64Value value);
    int             (*tr64_is_int  )(MuCtx *ctx, MuTagRef64Value value);
    int             (*tr64_is_ref  )(MuCtx *ctx, MuTagRef64Value value);
    MuDoubleValue   (*tr64_to_fp   )(MuCtx *ctx, MuTagRef64Value value);
    MuIntValue      (*tr64_to_int  )(MuCtx *ctx, MuTagRef64Value value);
    MuRefValue      (*tr64_to_ref  )(MuCtx *ctx, MuTagRef64Value value);
    MuIntValue      (*tr64_to_tag  )(MuCtx *ctx, MuTagRef64Value value);
    MuTagRef64Value (*tr64_from_fp )(MuCtx *ctx, MuDoubleValue value);
    MuTagRef64Value (*tr64_from_int)(MuCtx *ctx, MuIntValue value);
    MuTagRef64Value (*tr64_from_ref)(MuCtx *ctx, MuRefValue ref, MuIntValue tag);

    // Watchpoint operations
    void        (*enable_watchpoint )(MuCtx *ctx, int wpid);
    void        (*disable_watchpoint)(MuCtx *ctx, int wpid);

    // Mu memory pinning, usually object pinning
    MuUPtrValue (*pin  )(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue
    void        (*unpin)(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue

    // Expose Mu functions as native callable things, usually function pointers
    MuValue     (*expose  )(MuCtx *ctx, MuFuncRefValue func, MuCallConv call_conv, MuIntValue cookie);
    void        (*unexpose)(MuCtx *ctx, MuCallConv call_conv, MuValue value);
};

#ifdef __cplusplus
}
#endif

#endif // __MUAPI_H__
