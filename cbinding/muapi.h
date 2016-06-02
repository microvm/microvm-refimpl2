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
typedef MuValue MuIntValue;           // int<n>
typedef MuValue MuFloatValue;         // float
typedef MuValue MuDoubleValue;        // double
typedef MuValue MuUPtrValue;          // uptr
typedef MuValue MuUFPValue;           // ufuncptr

typedef MuSeqValue MuStructValue;     // struct<...>
typedef MuSeqValue MuArrayValue;      // array<T l>
typedef MuSeqValue MuVectorValue;     // vector<T l>

typedef MuGenRefValue MuRefValue;           // ref<T>
typedef MuGenRefValue MuIRefValue;          // iref<T>
typedef MuGenRefValue MuTagRef64Value;      // tagref64
typedef MuGenRefValue MuFuncRefValue;       // funcref<sig>
typedef MuGenRefValue MuThreadRefValue;     // threadref
typedef MuGenRefValue MuStackRefValue;      // stackref
typedef MuGenRefValue MuFCRefValue;         // framecursorref
typedef MuGenRefValue MuIRNodeRefValue;     // irnoderef

// Subtypes of MuIRNodeRefValue. These are used in the IR Builder API.

// Shorter aliases
typedef MuIRNodeRefValue MuIRNode;      // All IR Nodes

typedef MuIRNode MuBundleNode;          // Bundle
typedef MuIRNode MuChildNode;           // All children of bundle
typedef MuChildNode MuTypeNode;             // Type
typedef MuChildNode MuFuncSigNode;          // Function signature
typedef MuChildNode MuVarNode;              // Variable
typedef MuVarNode MuGlobalVarNode;          // Global variable
typedef MuGlobalVarNode MuConstNode;            // Constant
typedef MuGlobalVarNode MuGlobalNode;           // Global cell
typedef MuGlobalVarNode MuFuncNode;             // Function
typedef MuGlobalVarNode MuExpFuncNode;          // Exposed function
typedef MuVarNode MuLocalVarNode;           // Local variable
typedef MuLocalVarNode MuNorParamNode;          // Normal parameter
typedef MuLocalVarNode MuExcParamNode;          // Exception parameter
typedef MuLocalVarNode MuInstResNode;           // Instruction result
typedef MuChildNode MuFuncVerNode;          // Function version
typedef MuChildNode MuBBNode;               // Basic block
typedef MuChildNode MuInstNode;             // Instruction (itself, not result)

// Identifiers and names of Mu
typedef uint32_t MuID;
typedef char *MuName;

// Convenient types for the void* type and the void(*)() type in C
typedef void *MuCPtr;
typedef void (*MuCFP)();

// Watchpoint ID
typedef uint32_t MuWPID;

// Super type for numerical flags used by Mu.
typedef uint32_t MuFlag;

// Result of a trap handler
typedef MuFlag MuTrapHandlerResult;

// Used by new_thread
typedef MuFlag MuHowToResume;

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
        MuWPID wpid,
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

// Destination kinds
typedef MuFlag MuDestKind;
#define MU_DEST_NORMAL      0x01
#define MU_DEST_EXCEPT      0x02
#define MU_DEST_TRUE        0x03
#define MU_DEST_FALSE       0x04
#define MU_DEST_DEFAULT     0x04
#define MU_DEST_DISABLED    0x05
#define MU_DEST_ENABLED     0x06

// Binary operators
typedef MuFlag MuBinOptr;
#define MU_BINOP_ADD    0x01
#define MU_BINOP_SUB    0x02
#define MU_BINOP_MUL    0x03
#define MU_BINOP_SDIV   0x04
#define MU_BINOP_SREM   0x05
#define MU_BINOP_UDIV   0x06
#define MU_BINOP_UREM   0x07
#define MU_BINOP_SHL    0x08
#define MU_BINOP_LSHR   0x09
#define MU_BINOP_ASHR   0x0A
#define MU_BINOP_AND    0x0B
#define MU_BINOP_OR     0x0C
#define MU_BINOP_XOR    0x0D
#define MU_BINOP_FADD   0xB0
#define MU_BINOP_FSUB   0xB1
#define MU_BINOP_FMUL   0xB2
#define MU_BINOP_FDIV   0xB3
#define MU_BINOP_FREM   0xB4

// Comparing operators
typedef MuFlag MuCmpOptr;
#define MU_CMP_EQ       0x20
#define MU_CMP_NE       0x21
#define MU_CMP_SGE      0x22
#define MU_CMP_SGT      0x23
#define MU_CMP_SLE      0x24
#define MU_CMP_SLT      0x25
#define MU_CMP_UGE      0x26
#define MU_CMP_UGT      0x27
#define MU_CMP_ULE      0x28
#define MU_CMP_ULT      0x29
#define MU_CMP_FFALSE   0xC0
#define MU_CMP_FTRUE    0xC1
#define MU_CMP_FUNO     0xC2
#define MU_CMP_FUEQ     0xC3
#define MU_CMP_FUNE     0xC4
#define MU_CMP_FUGT     0xC5
#define MU_CMP_FUGE     0xC6
#define MU_CMP_FULT     0xC7
#define MU_CMP_FULE     0xC8
#define MU_CMP_FORD     0xC9
#define MU_CMP_FOEQ     0xCA
#define MU_CMP_FONE     0xCB
#define MU_CMP_FOGT     0xCC
#define MU_CMP_FOGE     0xCD
#define MU_CMP_FOLT     0xCE
#define MU_CMP_FOLE     0xCF

// Conversion operators
typedef MuFlag MuConvOptr;
#define MU_CONV_TRUNC   0x30
#define MU_CONV_ZEXT    0x31
#define MU_CONV_SEXT    0x32
#define MU_CONV_FPTRUNC 0x33
#define MU_CONV_FPEXT   0x34
#define MU_CONV_FPTOUI  0x35
#define MU_CONV_FPTOSI  0x36
#define MU_CONV_UITOFP  0x37
#define MU_CONV_SITOFP  0x38
#define MU_CONV_BITCAST 0x39
#define MU_CONV_REFCAST 0x3A
#define MU_CONV_PTRCAST 0x3B

// Memory orders
typedef MuFlag MuMemOrd;
#define MU_ORD_NOT_ATOMIC   0x00
#define MU_ORD_RELAXED      0x01
#define MU_ORD_CONSUME      0x02
#define MU_ORD_ACQUIRE      0x03
#define MU_ORD_RELEASE      0x04
#define MU_ORD_ACQ_REL      0x05
#define MU_ORD_SEQ_CST      0x06

// Operations for the atomicrmw API function
typedef MuFlag MuAtomicRMWOp;
#define MU_ARMW_XCHG    0x00
#define MU_ARMW_ADD     0x01
#define MU_ARMW_SUB     0x02
#define MU_ARMW_AND     0x03
#define MU_ARMW_NAND    0x04
#define MU_ARMW_OR      0x05
#define MU_ARMW_XOR     0x06
#define MU_ARMW_MAX     0x07
#define MU_ARMW_MIN     0x08
#define MU_ARMW_UMAX    0x09
#define MU_ARMW_UMIN    0x0A

// Calling conventions.
typedef MuFlag MuCallConv;
#define MU_CC_DEFAULT   0x00
// Concrete Mu implementations may define more calling conventions.

// Common instructions.
typedef MuFlag MuCommInst;

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
// local heap allocation pool, and an object-pinning set. It also holds many Munew_
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
                            MuRefValue threadlocal,
                            MuHowToResume htr, MuValue *vals, int nvals,
                            MuRefValue exc);
    void                (*kill_stack)(MuCtx *ctx, MuStackRefValue stack);

    // Thread-local object reference
    void        (*set_threadlocal)(MuCtx *ctx, MuThreadRefValue thread,
                    MuRefValue threadlocal);
    MuRefValue  (*get_threadlocal)(MuCtx *ctx, MuThreadRefValue thread);

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
    void        (*enable_watchpoint )(MuCtx *ctx, MuWPID wpid);
    void        (*disable_watchpoint)(MuCtx *ctx, MuWPID wpid);

    // Mu memory pinning, usually object pinning
    MuUPtrValue (*pin  )(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue
    void        (*unpin)(MuCtx *ctx, MuValue loc);      // loc is either MuRefValue or MuIRefValue

    // Expose Mu functions as native callable things, usually function pointers
    MuValue     (*expose  )(MuCtx *ctx, MuFuncRefValue func, MuCallConv call_conv, MuIntValue cookie);
    void        (*unexpose)(MuCtx *ctx, MuCallConv call_conv, MuValue value);

    /// IR Builder API from here on.

    // Create bundle
    MuBundleNode    (*new_bundle)(MuCtx *ctx);

    // Load a bundle built from an IR Builder into the micro VM
    void        (*load_bundle_from_node )(MuCtx *ctx, MuBundleNode b);
    // Call this function before the node is loaded to release all of its resources.
    void        (*abort_bundle_node     )(MuCtx *ctx, MuBundleNode b);

    // Get a MuChildNode that refers to an existing node that has the "id".
    // The returned handle is only usable within the bundle "b".
    // Can only get top-level definitions.
    MuChildNode (*get_node  )(MuCtx *ctx, MuBundleNode b, MuID id);

    // Get the ID of the IR node "node".
    MuID        (*get_id    )(MuCtx *ctx, MuChildNode node);

    // Set the name of the IR node. MuName is '\0' terminated char*.
    void        (*set_name  )(MuCtx *ctx, MuChildNode node, MuName name);

    /// Create top-level definitions. When created, they are added to the bundle "b".
    
    // Create types
    MuTypeNode  (*new_type_int      )(MuCtx *ctx, MuBundleNode b, int len);
    MuTypeNode  (*new_type_float    )(MuCtx *ctx, MuBundleNode b);
    MuTypeNode  (*new_type_double   )(MuCtx *ctx, MuBundleNode b);
    MuTypeNode  (*new_type_uptr     )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_uptr     )(MuCtx *ctx, MuTypeNode uptr, MuTypeNode ty);
    MuTypeNode  (*new_type_ufuncptr )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_ufuncptr )(MuCtx *ctx, MuTypeNode ufuncptr, MuFuncSigNode sig);

    MuTypeNode  (*new_type_struct   )(MuCtx *ctx, MuBundleNode b, MuTypeNode *fieldtys, int nfieldtys);
    MuTypeNode  (*new_type_hybrid   )(MuCtx *ctx, MuBundleNode b, MuTypeNode *fixedtys, int nfixedtys, MuTypeNode varty);
    MuTypeNode  (*new_type_array    )(MuCtx *ctx, MuBundleNode b, MuTypeNode elemty, uint64_t len);
    MuTypeNode  (*new_type_vector   )(MuCtx *ctx, MuBundleNode b, MuTypeNode elemty, uint64_t len);
    MuTypeNode  (*new_type_void     )(MuCtx *ctx, MuBundleNode b);

    MuTypeNode  (*new_type_ref      )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_ref      )(MuCtx *ctx, MuTypeNode ref, MuTypeNode ty);
    MuTypeNode  (*new_type_iref     )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_iref     )(MuCtx *ctx, MuTypeNode iref, MuTypeNode ty);
    MuTypeNode  (*new_type_weakref  )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_weakref  )(MuCtx *ctx, MuTypeNode weakref, MuTypeNode ty);
    MuTypeNode  (*new_type_funcref  )(MuCtx *ctx, MuBundleNode b);
    void        (*set_type_funcref  )(MuCtx *ctx, MuTypeNode funcref, MuFuncSigNode sig);
    MuTypeNode  (*new_type_tagref64 )(MuCtx *ctx, MuBundleNode b);

    MuTypeNode  (*new_type_threadref     )(MuCtx *ctx, MuBundleNode b);
    MuTypeNode  (*new_type_stackref      )(MuCtx *ctx, MuBundleNode b);
    MuTypeNode  (*new_type_framecursorref)(MuCtx *ctx, MuBundleNode b);
    MuTypeNode  (*new_type_irnoderef     )(MuCtx *ctx, MuBundleNode b);

    // Create function signatures
    MuFuncSigNode   (*new_funcsig   )(MuCtx *ctx, MuBundleNode b, MuTypeNode *paramtys, int nparamtys, MuTypeNode *rettys, int nrettys);

    // Create constants
    // new_const_int works for int<n>, uptr and ufuncptr.
    MuConstNode (*new_const_int     )(MuCtx *ctx, MuBundleNode b, MuTypeNode ty, uint64_t value);
    // new_const_int_ex works for int<n> with n > 64. The number is segmented into 64-bit words, lower word first.
    MuConstNode (*new_const_int_ex  )(MuCtx *ctx, MuBundleNode b, MuTypeNode ty, uint64_t *values, int nvalues);
    // TODO: There is only one 'float' type and one 'double' type. Theoretically the 'ty' param is unnecessary
    // It is just added to mirror the text form. Eliminate them when we are ready to change the text form.
    MuConstNode (*new_const_float   )(MuCtx *ctx, MuBundleNode b, MuTypeNode ty, float value);
    MuConstNode (*new_const_double  )(MuCtx *ctx, MuBundleNode b, MuTypeNode ty, double value);
    // new_const_null works for all general reference types, but not uptr or ufuncptr.
    MuConstNode (*new_const_null    )(MuCtx *ctx, MuBundleNode b);
    // new_const_seq works for structs, arrays and vectors. Constants are non-recursive, so there is no populate_list_const.
    MuConstNode (*new_const_seq     )(MuCtx *ctx, MuBundleNode b, MuConstNode *elems, int nelems);
    
    // Create global cell
    MuGlobalNode (*new_global_cell  )(MuCtx *ctx, MuBundleNode b, MuTypeNode ty);

    // Create function
    MuFuncNode  (*new_func          )(MuCtx *ctx, MuBundleNode b, MuFuncSigNode sig);

    // Create function version
    // In order to declare and define a new function, the client must create
    // both a funciton node and a function version node.
    MuFuncVerNode   (*new_func_ver  )(MuCtx *ctx, MuBundleNode b, MuFuncNode func);

    // Create exposed function
    MuExpFuncNode   (*new_exp_func  )(MuCtx *ctx, MuBundleNode b, MuFuncNode func, MuCallConv callconv, MuConstNode cookie);

    /// Create CFG
    
    // Create basic block and add it to function version "fv". excparam can be NULL.
    MuBBNode        (*new_bb        )(MuCtx *ctx, MuFuncVerNode fv);

    // Basic block parameters and instruction results are appended to the basic
    // block or instruction.

    // Create a normal parameter node and add it to the basic block "bb".
    MuNorParamNode  (*new_nor_param )(MuCtx *ctx, MuBBNode bb, MuTypeNode ty);

    // Create an exception parameter node and add it to the basic block "bb".
    MuExcParamNode  (*new_exc_param )(MuCtx *ctx, MuBBNode bb);

    // Create an intruction result. It becomes the next result of "inst".
    MuInstResNode   (*new_inst_res  )(MuCtx *ctx, MuInstNode inst);

    /// Create common clauses for instructions.

    // Create a destination claues and add it to instruction "inst".
    void    (*add_dest      )(MuCtx *ctx, MuInstNode inst, MuDestKind kind, MuBBNode dest, MuVarNode *vars, int nvars);

    // Create a destination claues and add it to instruction "inst".
    void    (*add_keepalives)(MuCtx *ctx, MuInstNode inst, MuLocalVarNode *vars, int nvars);
    
    /// Create instructions. Instructions are appended to the basic block "bb".

    MuInstNode  (*new_binop         )(MuCtx *ctx, MuBBNode bb, MuBinOptr  optr,    MuTypeNode ty,      MuVarNode  opnd1, MuVarNode opnd2);
    MuInstNode  (*new_cmp           )(MuCtx *ctx, MuBBNode bb, MuCmpOptr  optr,    MuTypeNode ty,      MuVarNode  opnd1, MuVarNode opnd2);
    MuInstNode  (*new_conv          )(MuCtx *ctx, MuBBNode bb, MuConvOptr optr,    MuTypeNode from_ty, MuTypeNode to_ty, MuVarNode opnd);
    MuInstNode  (*new_select        )(MuCtx *ctx, MuBBNode bb, MuTypeNode cond_ty, MuTypeNode opnd_ty, MuVarNode  cond,  MuVarNode if_true, MuVarNode if_false);

    MuInstNode  (*new_branch        )(MuCtx *ctx, MuBBNode bb);
    MuInstNode  (*new_branch2       )(MuCtx *ctx, MuBBNode bb, MuVarNode cond);
    MuInstNode  (*new_switch        )(MuCtx *ctx, MuBBNode bb, MuVarNode opnd);
    void        (*add_switch_dest   )(MuCtx *ctx, MuInstNode sw, MuConstNode key, MuBBNode dest, MuVarNode *vars, int nvars);

    MuInstNode  (*new_call          )(MuCtx *ctx, MuBBNode bb, MuFuncSigNode sig, MuVarNode callee, MuVarNode *args, int nargs);
    MuInstNode  (*new_tailcall      )(MuCtx *ctx, MuBBNode bb, MuFuncSigNode sig, MuVarNode callee, MuVarNode *args, int nargs);
    MuInstNode  (*new_ret           )(MuCtx *ctx, MuBBNode bb, MuVarNode *rvs, int nrvs);
    MuInstNode  (*new_throw         )(MuCtx *ctx, MuBBNode bb, MuVarNode exc);

    MuInstNode  (*new_extractvalue  )(MuCtx *ctx, MuBBNode bb, MuTypeNode strty, int index,         MuVarNode opnd);
    MuInstNode  (*new_insertvalue   )(MuCtx *ctx, MuBBNode bb, MuTypeNode strty, int index,         MuVarNode opnd, MuVarNode newval);
    MuInstNode  (*new_extractelement)(MuCtx *ctx, MuBBNode bb, MuTypeNode seqty, MuTypeNode indty,  MuVarNode opnd, MuVarNode index);
    MuInstNode  (*new_insertelement )(MuCtx *ctx, MuBBNode bb, MuTypeNode seqty, MuTypeNode indty,  MuVarNode opnd, MuVarNode index, MuVarNode newval);
    MuInstNode  (*new_shufflevector )(MuCtx *ctx, MuBBNode bb, MuTypeNode vecty, MuTypeNode maskty, MuVarNode vec1, MuVarNode vec2,  MuVarNode mask);

    MuInstNode  (*new_new           )(MuCtx *ctx, MuBBNode bb, MuTypeNode allocty);
    MuInstNode  (*new_newhybrid     )(MuCtx *ctx, MuBBNode bb, MuTypeNode allocty, MuTypeNode lenty, MuVarNode length);
    MuInstNode  (*new_alloca        )(MuCtx *ctx, MuBBNode bb, MuTypeNode allocty);
    MuInstNode  (*new_allocahybrid  )(MuCtx *ctx, MuBBNode bb, MuTypeNode allocty, MuTypeNode lenty, MuVarNode length);

    MuInstNode  (*new_getiref       )(MuCtx *ctx, MuBBNode bb, MuTypeNode refty, MuVarNode opnd);
    MuInstNode  (*new_getfieldiref  )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuTypeNode refty, int index, MuVarNode opnd);
    MuInstNode  (*new_getelemiref   )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuTypeNode refty, MuTypeNode indty, MuVarNode opnd, MuVarNode index);
    MuInstNode  (*new_shiftiref     )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuTypeNode refty, MuTypeNode offty, MuVarNode opnd, MuVarNode offset);
    MuInstNode  (*new_getvarpartiref)(MuCtx *ctx, MuBBNode bb, int is_ptr, MuTypeNode refty, MuVarNode opnd);

    MuInstNode  (*new_load          )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuMemOrd ord, MuTypeNode refty,   MuVarNode loc);
    MuInstNode  (*new_store         )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuMemOrd ord, MuTypeNode refty,   MuVarNode loc,     MuVarNode newval);
    MuInstNode  (*new_cmpxchg       )(MuCtx *ctx, MuBBNode bb, int is_ptr, int is_weak,  MuMemOrd ord_succ,  MuMemOrd ord_fail, MuTypeNode refty, MuVarNode loc, MuVarNode expected, MuVarNode desired);
    MuInstNode  (*new_atomicrmw     )(MuCtx *ctx, MuBBNode bb, int is_ptr, MuMemOrd ord, MuAtomicRMWOp optr, MuVarNode loc,     MuVarNode opnd);
    MuInstNode  (*new_fence         )(MuCtx *ctx, MuBBNode bb, MuMemOrd ord);
    
    MuInstNode  (*new_trap          )(MuCtx *ctx, MuBBNode bb, MuTypeNode *rettys, int nrettys);
    MuInstNode  (*new_watchpoint    )(MuCtx *ctx, MuBBNode bb, MuWPID wpid, MuTypeNode *rettys, int nrettys);
    MuInstNode  (*new_wpbranch      )(MuCtx *ctx, MuBBNode bb, MuWPID wpid);

    MuInstNode  (*new_ccall         )(MuCtx *ctx, MuBBNode bb, MuCallConv callconv, MuTypeNode callee_ty, MuFuncSigNode sig, MuVarNode callee, MuVarNode *args, int nargs);

    MuInstNode  (*new_newthread     )(MuCtx *ctx, MuBBNode bb, MuVarNode stack, MuVarNode threadlocal);
    MuInstNode  (*new_swapstack_ret )(MuCtx *ctx, MuBBNode bb, MuVarNode swappee, MuTypeNode *tys, int ntys);
    MuInstNode  (*new_swapstack_kill)(MuCtx *ctx, MuBBNode bb, MuVarNode swappee);

    void        (*set_newstack_pass_values)(MuCtx *ctx, MuInstNode inst, MuTypeNode tys, MuVarNode *vars, int nvars);
    void        (*set_newstack_throw_exc  )(MuCtx *ctx, MuInstNode inst, MuVarNode exc);

    MuInstNode  (*new_comminst      )(MuCtx *ctx, MuBBNode bb, MuCommInst opcode,
                                        MuFlag        *flags, int nflags,
                                        MuTypeNode    *tys,   int ntys,
                                        MuFuncSigNode *sigs,  int nsigs,
                                        MuVarNode     *args,  int nargs);
};

// Common instruction opcodes
#define MU_CI_UVM_NEW_STACK               0X201
#define MU_CI_UVM_KILL_STACK              0X202
#define MU_CI_UVM_THREAD_EXIT             0X203
#define MU_CI_UVM_CURRENT_STACK           0X204
#define MU_CI_UVM_SET_THREADLOCAL         0X205
#define MU_CI_UVM_GET_THREADLOCAL         0X206
#define MU_CI_UVM_TR64_IS_FP              0X211
#define MU_CI_UVM_TR64_IS_INT             0X212
#define MU_CI_UVM_TR64_IS_REF             0X213
#define MU_CI_UVM_TR64_FROM_FP            0X214
#define MU_CI_UVM_TR64_FROM_INT           0X215
#define MU_CI_UVM_TR64_FROM_REF           0X216
#define MU_CI_UVM_TR64_TO_FP              0X217
#define MU_CI_UVM_TR64_TO_INT             0X218
#define MU_CI_UVM_TR64_TO_REF             0X219
#define MU_CI_UVM_TR64_TO_TAG             0X21A
#define MU_CI_UVM_FUTEX_WAIT              0X220
#define MU_CI_UVM_FUTEX_WAIT_TIMEOUT      0X221
#define MU_CI_UVM_FUTEX_WAKE              0X222
#define MU_CI_UVM_FUTEX_CMP_REQUEUE       0X223
#define MU_CI_UVM_KILL_DEPENDENCY         0X230
#define MU_CI_UVM_NATIVE_PIN              0X240
#define MU_CI_UVM_NATIVE_UNPIN            0X241
#define MU_CI_UVM_NATIVE_EXPOSE           0X242
#define MU_CI_UVM_NATIVE_UNEXPOSE         0X243
#define MU_CI_UVM_NATIVE_GET_COOKIE       0X244
#define MU_CI_UVM_META_ID_OF              0X250
#define MU_CI_UVM_META_NAME_OF            0X251
#define MU_CI_UVM_META_LOAD_BUNDLE        0X252
#define MU_CI_UVM_META_LOAD_HAIL          0X253
#define MU_CI_UVM_META_NEW_CURSOR         0X254
#define MU_CI_UVM_META_NEXT_FRAME         0X255
#define MU_CI_UVM_META_COPY_CURSOR        0X256
#define MU_CI_UVM_META_CLOSE_CURSOR       0X257
#define MU_CI_UVM_META_CUR_FUNC           0X258
#define MU_CI_UVM_META_CUR_FUNC_VER       0X259
#define MU_CI_UVM_META_CUR_INST           0X25A
#define MU_CI_UVM_META_DUMP_KEEPALIVES    0X25B
#define MU_CI_UVM_META_POP_FRAMES_TO      0X25C
#define MU_CI_UVM_META_PUSH_FRAME         0X25D
#define MU_CI_UVM_META_ENABLE_WATCHPOINT  0X25E
#define MU_CI_UVM_META_DISABLE_WATCHPOINT 0X25F
#define MU_CI_UVM_META_SET_TRAP_HANDLER   0X260

#ifdef __cplusplus
}
#endif

#endif // __MUAPI_H__
