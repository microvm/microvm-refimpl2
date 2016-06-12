#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>

#include <muapi.h>

#define MU_ASSERT_EQUALS(a, b, f) if (!(a == b)) { \
    muprintf("%s (%" f ") is not equal to %s (%" f ")\n", #a, a, #b, b); \
    return false; \
}

#define MU_ASSERT_EQUALS_TRAP(a, b, f) if (!(a == b)) { \
    muprintf("%s (%" f ") is not equal to %s (%" f ")\n", #a, a, #b, b); \
    exit(1); \
}

#define ID(name) ctx->id_of(ctx, name)
#define NAME(id) ctx->name_of(ctx, id)

#define muprintf(fmt, ...) printf("[C:%d:%s] " fmt, __LINE__, __func__, ## __VA_ARGS__)

bool test_basic(MuVM *mvm, int theID, char *theName) {
    muprintf("header: %p\n", mvm->header);
    muprintf("new_context: %p\n", mvm->new_context);
    muprintf("id_of: %p\n", mvm->id_of);
    muprintf("name_of: %p\n", mvm->name_of);
    muprintf("set_trap_handler: %p\n", mvm->set_trap_handler);

    int id = mvm->id_of(mvm, theName);
    muprintf("id = %d\n", id);
    if (id != theID) {
        muprintf("ID %d is not equal to %d\n", id, theID);
        return false;
    }

    char *name = mvm->name_of(mvm, theID);
    muprintf("name = %s\n", name);
    if (strcmp(name, theName) != 0) {
        muprintf("name %s is not equal to %s\n", name, theName);
        return false;
    }

    return true;
}

bool test_with_ctx(MuVM *mvm, int theID, char *theName) {
    MuCtx *ctx = mvm->new_context(mvm);

    int id = ctx->id_of(ctx, theName);
    muprintf("id = %d\n", id);
    if (id != theID) {
        muprintf("ID %d is not equal to %d\n", id, theID);
        return false;
    }

    char *name = ctx->name_of(ctx, theID);
    muprintf("name = %s\n", name);
    if (strcmp(name, theName) != 0) {
        muprintf("name %s is not equal to %s\n", name, theName);
        return false;
    }

    ctx->close_context(ctx);

    return true;
}

bool test_basic_conv(MuVM *mvm) {
    MuCtx *ctx = mvm->new_context(mvm);

    int8_t   p8  =  0x7f;
    int8_t   n8  = -0x80;
    uint8_t  u8  =  0xffU;
    uint8_t  r8  =  0x5aU;
    int16_t  p16 =  0x7fff;
    int16_t  n16 = -0x8000;
    uint16_t u16 =  0xffffU;
    uint16_t r16 =  0x55aaU;
    int32_t  p32 =  0x7fffffff;
    int32_t  n32 = -0x80000000;
    uint32_t u32 =  0xffffffffU;
    uint32_t r32 =  0x5555aaaaU;
    int64_t  p64 =  0x7fffffffffffffffL;
    int64_t  n64 = -0x8000000000000000L;
    uint64_t u64 =  0xffffffffffffffffUL;
    uint64_t r64 =  0x55555555aaaaaaaaUL;

    MuIntValue vp8  = ctx->handle_from_sint8 (ctx, p8 , 8 );
    MuIntValue vn8  = ctx->handle_from_sint8 (ctx, n8 , 8 );
    MuIntValue vu8  = ctx->handle_from_uint8 (ctx, u8 , 8 );
    MuIntValue vr8  = ctx->handle_from_uint8 (ctx, r8 , 8 );
    MuIntValue vp16 = ctx->handle_from_sint16(ctx, p16, 16);
    MuIntValue vn16 = ctx->handle_from_sint16(ctx, n16, 16);
    MuIntValue vu16 = ctx->handle_from_uint16(ctx, u16, 16);
    MuIntValue vr16 = ctx->handle_from_uint16(ctx, r16, 16);
    MuIntValue vp32 = ctx->handle_from_sint32(ctx, p32, 32);
    MuIntValue vn32 = ctx->handle_from_sint32(ctx, n32, 32);
    MuIntValue vu32 = ctx->handle_from_uint32(ctx, u32, 32);
    MuIntValue vr32 = ctx->handle_from_uint32(ctx, r32, 32);
    MuIntValue vp64 = ctx->handle_from_sint64(ctx, p64, 64);
    MuIntValue vn64 = ctx->handle_from_sint64(ctx, n64, 64);
    MuIntValue vu64 = ctx->handle_from_uint64(ctx, u64, 64);
    MuIntValue vr64 = ctx->handle_from_uint64(ctx, r64, 64);

    int8_t   bp8  = ctx->handle_to_sint8 (ctx, vp8 );
    int8_t   bn8  = ctx->handle_to_sint8 (ctx, vn8 );
    uint8_t  bu8  = ctx->handle_to_uint8 (ctx, vu8 );
    uint8_t  br8  = ctx->handle_to_uint8 (ctx, vr8 );
    int16_t  bp16 = ctx->handle_to_sint16(ctx, vp16);
    int16_t  bn16 = ctx->handle_to_sint16(ctx, vn16);
    uint16_t bu16 = ctx->handle_to_uint16(ctx, vu16);
    uint16_t br16 = ctx->handle_to_uint16(ctx, vr16);
    int32_t  bp32 = ctx->handle_to_sint32(ctx, vp32);
    int32_t  bn32 = ctx->handle_to_sint32(ctx, vn32);
    uint32_t bu32 = ctx->handle_to_uint32(ctx, vu32);
    uint32_t br32 = ctx->handle_to_uint32(ctx, vr32);
    int64_t  bp64 = ctx->handle_to_sint64(ctx, vp64);
    int64_t  bn64 = ctx->handle_to_sint64(ctx, vn64);
    uint64_t bu64 = ctx->handle_to_uint64(ctx, vu64);
    uint64_t br64 = ctx->handle_to_uint64(ctx, vr64);

    MU_ASSERT_EQUALS(p8 , bp8 , PRIi8 )
    MU_ASSERT_EQUALS(n8 , bn8 , PRIi8 )
    MU_ASSERT_EQUALS(u8 , bu8 , PRIu8 )
    MU_ASSERT_EQUALS(r8 , br8 , PRIu8 )
    MU_ASSERT_EQUALS(p16, bp16, PRIi16)
    MU_ASSERT_EQUALS(n16, bn16, PRIi16)
    MU_ASSERT_EQUALS(u16, bu16, PRIu16)
    MU_ASSERT_EQUALS(r16, br16, PRIu16)
    MU_ASSERT_EQUALS(p32, bp32, PRIi32)
    MU_ASSERT_EQUALS(n32, bn32, PRIi32)
    MU_ASSERT_EQUALS(u32, bu32, PRIu32)
    MU_ASSERT_EQUALS(r32, br32, PRIu32)
    MU_ASSERT_EQUALS(p64, bp64, PRIi64)
    MU_ASSERT_EQUALS(n64, bn64, PRIi64)
    MU_ASSERT_EQUALS(u64, bu64, PRIu64)
    MU_ASSERT_EQUALS(r64, br64, PRIu64)

    MuIntValue sep8t64 = ctx->handle_from_sint8(ctx, p8, 64);
    MuIntValue sen8t64 = ctx->handle_from_sint8(ctx, n8, 64);
    MuIntValue zeu8t64 = ctx->handle_from_uint8(ctx, u8, 64);
    MuIntValue zer8t64 = ctx->handle_from_uint8(ctx, r8, 64);

    uint64_t bsep8t64 = ctx->handle_to_uint64(ctx, sep8t64);
    uint64_t bsen8t64 = ctx->handle_to_uint64(ctx, sen8t64);
    uint64_t bzeu8t64 = ctx->handle_to_uint64(ctx, zeu8t64);
    uint64_t bzer8t64 = ctx->handle_to_uint64(ctx, zer8t64);

    MU_ASSERT_EQUALS(bsep8t64, 0x000000000000007fULL, PRIx64);
    MU_ASSERT_EQUALS(bsen8t64, 0xffffffffffffff80ULL, PRIx64);
    MU_ASSERT_EQUALS(bzeu8t64, 0x00000000000000ffULL, PRIx64);
    MU_ASSERT_EQUALS(bzer8t64, 0x000000000000005aULL, PRIx64);

    float  f = 3.14f;
    double d = 6.28;
        
    MuFloatValue  hf = ctx->handle_from_float (ctx, f);
    MuDoubleValue hd = ctx->handle_from_double(ctx, d);

    float  bf = ctx->handle_to_float (ctx, hf);
    double bd = ctx->handle_to_double(ctx, hd);

    MU_ASSERT_EQUALS(bf, 3.14f, "f" );
    MU_ASSERT_EQUALS(bd, 6.28 , "lf");

    MuCPtr p = (MuCPtr)&f;
    MuCFP fp = (MuCFP)test_basic_conv;

    MuID id_ptrvoid = ctx->id_of(ctx, "@ptrvoid");
    MuID id_fpv_v   = ID("@fpv_v");

    MuUPtrValue hp = ctx->handle_from_ptr(ctx, id_ptrvoid, p );
    MuUFPValue hfp = ctx->handle_from_fp (ctx, id_fpv_v,   fp);

    MuCPtr bp = ctx->handle_to_ptr(ctx, hp );
    MuCFP bfp = ctx->handle_to_fp (ctx, hfp);

    MU_ASSERT_EQUALS(bp , p , "p");
    MU_ASSERT_EQUALS(bfp, fp, "p");

    ctx->delete_value(ctx, zer8t64);

    ctx->close_context(ctx);

    return true;
}

bool test_global_vars(MuVM *mvm, int64_t(*the_plus_one_fp)(int64_t)) {
    MuCtx *ctx = mvm->new_context(mvm);

    MuIntValue  hI64_7 = ctx->handle_from_const(ctx, ID("@I64_7"));
    int64_t I64_7 = ctx->handle_to_sint64(ctx, hI64_7);

    MU_ASSERT_EQUALS(I64_7, 7LL, PRId64);

    MuIRefValue     hg_i64           = ctx->handle_from_global(ctx, ID("@g_i64"));
    MuFuncRefValue  hplus_one        = ctx->handle_from_func  (ctx, ID("@plus_one"));
    MuUFPValue      hplus_one_native = ctx->handle_from_expose(ctx, ID("@plus_one_native"));

    int64_t (*plus_one_fp)(int64_t) = (int64_t(*)(int64_t))(ctx->handle_to_fp(ctx, hplus_one_native));

    MU_ASSERT_EQUALS(plus_one_fp, the_plus_one_fp, "p");

    // NOTE: cannot call it now. In this refimpl, Mu must call native first.
    // This test case is run by ScalaTest, not Mu IR.

    //int64_t result = plus_one_fp(42LL);

    //MU_ASSERT_EQUALS(result, 43LL, PRId64);

    ctx->close_context(ctx);

    return true;
}

struct simple_context {
    int magic;
};

void malloc_freer(MuValue *values, MuCPtr freerdata) {
    free(values);
}

void nop_freer(MuValue *values, MuCPtr freerdata) {
}

void simple_trap_handler(MuCtx *ctx, MuThreadRefValue thread,
        MuStackRefValue stack, MuWPID wpid, MuTrapHandlerResult *result,
        MuStackRefValue *new_stack, MuValue **values, int *nvalues,
        MuValuesFreer *freer, MuCPtr *freerdata, MuRefValue *exception,
        MuCPtr userdata) {

    muprintf("Hi! I am the native trap handler!\n");

    struct simple_context *userctx = (struct simple_context*)userdata;
    int magic = userctx->magic;
    muprintf("My magic is %d\n", magic);
    MU_ASSERT_EQUALS_TRAP(magic, 42, "d");

    muprintf("I am going to introspect the stack.\n");

    MuFCRefValue *cursor = ctx->new_cursor(ctx, stack);
    MuID fid  = ctx->cur_func(ctx, cursor);
    muprintf("Function: %d: %s\n", fid, NAME(fid));
    MuID fvid = ctx->cur_func_ver(ctx, cursor);
    muprintf("Version: %d: %s\n", fvid, NAME(fvid));
    MuID iid  = ctx->cur_inst(ctx, cursor);
    muprintf("Instruction: %d: %s\n", iid, NAME(iid));

    MuID trap1_id = ID("@trapper.v1.entry.trap1");
    MuID trap2_id = ID("@trapper.v1.entry.trap2");

    MU_ASSERT_EQUALS_TRAP(fid , ID("@trapper"), "d");
    MU_ASSERT_EQUALS_TRAP(fvid, ID("@trapper.v1"), "d");

    muprintf("Selecting branch according to the instruction ID: %d\n", iid);

    if (iid == trap1_id) {
        muprintf("It is %%trap1\n");
        MuValue kas[1];
        muprintf("Dumping keep-alives...\n");
        ctx->dump_keepalives(ctx, cursor, kas);
        muprintf("Dumped\n");
        ctx->close_cursor(ctx, cursor);

        int64_t oldKa = ctx->handle_to_sint64(ctx, kas[0]);
        muprintf("The keep-alive variable is %" PRId64 "\n", oldKa);
        MU_ASSERT_EQUALS_TRAP(oldKa, 42LL, PRId64);

        int64_t newKa = oldKa + 1;

        muprintf("Prepare to return\n");
        muprintf("Writing result at %p\n", result);
        *result = MU_REBIND_PASS_VALUES;
        muprintf("Writing new_stack at %p\n", new_stack);
        *new_stack = stack;
        muprintf("Allocating values array, writing at %p\n", values);
        *values = (MuValue*)malloc(8);
        muprintf("Setting the only value. Addr: %p\n", &(*values)[0]);
        (*values)[0] = ctx->handle_from_sint64(ctx, newKa, 64);
        muprintf("Writing nvalues at %p\n", nvalues);
        *nvalues = 1;
        muprintf("Writing freer at %p\n", freer);
        *freer = malloc_freer;
        muprintf("Writing freerdata at %p\n", freerdata);
        *freerdata = NULL;
        muprintf("Bye!\n");
        return;
    } else if (iid == trap2_id) {
        muprintf("It is %%trap2\n");
        MuValue kas[1];
        ctx->dump_keepalives(ctx, cursor, kas);
        ctx->close_cursor(ctx, cursor);

        muprintf("Introspect the KAs...\n");
        int64_t v1 = ctx->handle_to_sint64(ctx, kas[0]);
        muprintf("KA value %%v1 is %" PRId64 "\n", v1);
        MU_ASSERT_EQUALS_TRAP(v1, 43LL, PRId64);

        muprintf("Prepare to return from trap handler...\n");
        *result = MU_REBIND_PASS_VALUES;
        *new_stack = stack;
        *values = NULL;
        *nvalues = 0;
        *freer = NULL;
        *freerdata = NULL;
        muprintf("Bye!\n");
        return;
    } else {
        muprintf("Unknown trap. ID: %d\n", iid);
        MuName trapName = NAME(iid);
        muprintf("name: %s\n", trapName);
        exit(1);
    }

    return; // Unreachable, but the C compiler may not be smart enough.
}

bool test_traps(MuVM *mvm) {
    struct simple_context userctx = { 42 };
    mvm->set_trap_handler(mvm, simple_trap_handler, &userctx);

    MuCtx *ctx = mvm->new_context(mvm);

    MuValue args[1];
    args[0] = ctx->handle_from_sint64(ctx, 42LL, 64);

    MuFuncRefValue func = ctx->handle_from_func(ctx, ID("@trapper"));
    MuStackRefValue stack = ctx->new_stack(ctx, func);
    MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, args, 1);

    mvm->execute(mvm);

    ctx->close_context(ctx);

    return true;
}

void load_bundle_trap_handler(MuCtx *ctx, MuThreadRefValue thread,
        MuStackRefValue stack, MuWPID wpid, MuTrapHandlerResult *result,
        MuStackRefValue *new_stack, MuValue **values, int *nvalues,
        MuValuesFreer *freer, MuCPtr *freerdata, MuRefValue *exception,
        MuCPtr userdata) {

    muprintf("Hi! I am the native trap handler!\n");

    MuFCRefValue *cursor = ctx->new_cursor(ctx, stack);
    MuID fid  = ctx->cur_func(ctx, cursor);
    MuID fvid = ctx->cur_func_ver(ctx, cursor);
    MuID iid  = ctx->cur_inst(ctx, cursor);

    MuID trap_id = ID("@made_in_c.v1.entry.trap");

    MU_ASSERT_EQUALS_TRAP(fid , ID("@made_in_c"), "d");
    MU_ASSERT_EQUALS_TRAP(fvid, ID("@made_in_c.v1"), "d");
    MU_ASSERT_EQUALS_TRAP(iid , ID("@made_in_c.v1.entry.trap"), "d");

    MuValue kas[1];
    ctx->dump_keepalives(ctx, cursor, kas);
    ctx->close_cursor(ctx, cursor);

    int64_t n = ctx->handle_to_sint64(ctx, kas[0]);
    muprintf("KA value %%n is %" PRId64 "\n", n);
    MU_ASSERT_EQUALS_TRAP(n, 53LL, PRId64);

    MuIRefValue hg_cmagic_iref = ctx->handle_from_global(ctx, ID("@g_cmagic"));
    MuIntValue  hg_cmagic_value = ctx->load(ctx, MU_ORD_NOT_ATOMIC, hg_cmagic_iref);
    int64_t g_cmagic = ctx->handle_to_sint64(ctx, hg_cmagic_value);
    muprintf("Global value @g_cmagic is %" PRId64 "\n", g_cmagic);
    MU_ASSERT_EQUALS_TRAP(g_cmagic, 52LL, PRId64);

    muprintf("Prepare to return from trap handler...\n");
    *result = MU_REBIND_PASS_VALUES;
    *new_stack = stack;
    *values = NULL;
    *nvalues = 0;
    *freer = NULL;
    *freerdata = NULL;
    muprintf("Bye!\n");

    return;
}

bool test_load_bundle(MuVM *mvm) {
    mvm->set_trap_handler(mvm, load_bundle_trap_handler, NULL);

    MuCtx *ctx = mvm->new_context(mvm);

    char ir[] = ".funcdef @made_in_c VERSION %v1 <@i_i> {\n"
        "  %entry(<@i64> %n):\n"
        "    [%trap] TRAP <> KEEPALIVE (%n)\n"
        "    COMMINST @uvm.thread_exit\n"
        "}\n"
        ".global @g_cmagic <@i64>\n"
        ;

    char hail[] = ".init @g_cmagic = 52\n";

    muprintf("Loading bundle...\n");
    ctx->load_bundle(ctx, ir, strlen(ir));
    muprintf("Loading HAIL...\n");
    ctx->load_hail(ctx, hail, strlen(hail));
    muprintf("All loaded.\n");

    MuValue args[1];
    args[0] = ctx->handle_from_sint64(ctx, 53LL, 64);

    MuFuncRefValue func = ctx->handle_from_func(ctx, ID("@made_in_c"));
    MuStackRefValue stack = ctx->new_stack(ctx, func);
    MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, args, 1);

    mvm->execute(mvm);

    ctx->close_context(ctx);

    return true;
}

bool test_comp_types(MuVM *mvm) {
    MuCtx *ctx = mvm->new_context(mvm);

    MuStructValue s1  = ctx->handle_from_const(ctx, ID("@S1"));
    MuIntValue    s10 = ctx->extract_value(ctx, s1, 0);
    int64_t s10_v = ctx->handle_to_sint64(ctx, s10);
    MU_ASSERT_EQUALS(s10_v, 6LL, PRId64);

    MuDoubleValue s11 = ctx->extract_value(ctx, s1, 1);
    double s11_v = ctx->handle_to_double(ctx, s11);
    MU_ASSERT_EQUALS(s11_v, 7.0, "lf");

    MuIntValue    I64_20 = ctx->handle_from_const(ctx, ID("@I64_20"));
    MuStructValue s1m    = ctx->insert_value(ctx, s1, 0, I64_20);
    MuIntValue    s10m   = ctx->extract_value(ctx, s1m, 0);
    int64_t       s10m_v = ctx->handle_to_sint64(ctx, s10m);
    MU_ASSERT_EQUALS(s10m_v, 20LL, PRId64);

    MuArrayValue a1 = ctx->handle_from_const(ctx, ID("@A1"));

    MuID i32 = ID("@i32");

    for (int i=0; i<3; i++) {
        MuIntValue hi  = ctx->handle_from_sint32(ctx, i, 32);
        MuIntValue a1i = ctx->extract_element(ctx, a1, hi);
        int32_t a1i_v  = ctx->handle_to_sint32(ctx, a1i);
        MU_ASSERT_EQUALS(a1i_v, i+2, PRId32);
        ctx->delete_value(ctx, hi);
        ctx->delete_value(ctx, a1i);
    }

    MuIntValue I32_2  = ctx->handle_from_const(ctx, ID("@I32_2"));
    MuIntValue I32_30 = ctx->handle_from_const(ctx, ID("@I32_30"));

    MuArrayValue a1m   = ctx->insert_element (ctx, a1,  I32_2, I32_30);
    MuIntValue   a1m2  = ctx->extract_element(ctx, a1m, I32_2);
    int32_t      a1m2v = ctx->handle_to_sint32(ctx, a1m2);
    MU_ASSERT_EQUALS(a1m2v, 30, PRId32);

    ctx->close_context(ctx);

    return true;
}

bool test_memory_ops(MuVM *mvm) {
    MuCtx *ctx = mvm->new_context(mvm);

    MuRefValue r1 = ctx->new_fixed(ctx, ID("@i64"));
    MuRefValue r2 = ctx->new_fixed(ctx, ID("@i64"));

    MuIntValue I64_3  = ctx->handle_from_const(ctx, ID("@I64_3"));
    MuIntValue I64_4  = ctx->handle_from_const(ctx, ID("@I64_4"));
    MuIntValue I64_10 = ctx->handle_from_const(ctx, ID("@I64_10"));

    MuRefValue rh1 = ctx->new_hybrid(ctx, ID("@hyb"), I64_10);
    MuRefValue rh2 = ctx->new_hybrid(ctx, ID("@hyb"), I64_10);

    int r1r1 = ctx->ref_eq(ctx, r1, r1);
    int r1r2 = ctx->ref_eq(ctx, r1, r2);

    MU_ASSERT_EQUALS(r1r1, 1, "d");
    MU_ASSERT_EQUALS(r1r2, 0, "d");

    MuIRefValue rh1i  = ctx->get_iref(ctx, rh1);
    MuIRefValue rh1f  = ctx->get_field_iref(ctx, rh1i, 0);
    MuIRefValue rh1v  = ctx->get_var_part_iref(ctx, rh1i);
    MuIRefValue rh1v3 = ctx->shift_iref(ctx, rh1v, I64_3);
    MuIRefValue rh1v4 = ctx->shift_iref(ctx, rh1v, I64_4);

    int rh33 = ctx->ref_ult(ctx, rh1v3, rh1v3);
    int rh34 = ctx->ref_ult(ctx, rh1v3, rh1v4);

    MU_ASSERT_EQUALS(rh33, 0, "d");
    MU_ASSERT_EQUALS(rh34, 1, "d");

    MuRefValue  r3   = ctx->new_fixed(ctx, ID("@a1"));
    MuIRefValue r3i  = ctx->get_iref(ctx, r3);
    MuIRefValue r3i3 = ctx->get_elem_iref(ctx, r3i, I64_3);

    MuRefValue r1i = ctx->get_iref(ctx, r1);
    
    ctx->store(ctx, MU_ORD_SEQ_CST, r1i, I64_10);
    MuIntValue l = ctx->load(ctx, MU_ORD_SEQ_CST, r1i);
    int64_t lv   = ctx->handle_to_sint64(ctx, l);

    MU_ASSERT_EQUALS(lv, 10LL, PRId64);

    int succ1;
    int succ2;

    MuIntValue res1 = ctx->cmpxchg(ctx, MU_ORD_SEQ_CST, MU_ORD_SEQ_CST, 0,
            r1i, I64_10, I64_4, &succ1);
    MuIntValue res2 = ctx->cmpxchg(ctx, MU_ORD_SEQ_CST, MU_ORD_SEQ_CST, 0,
            r1i, I64_10, I64_3, &succ2);

    MU_ASSERT_EQUALS(succ1, 1, "d");
    MU_ASSERT_EQUALS(succ2, 0, "d");

    int64_t res1v = ctx->handle_to_sint64(ctx, res1);
    int64_t res2v = ctx->handle_to_sint64(ctx, res2);

    MU_ASSERT_EQUALS(res1v, 10LL, PRId64);
    MU_ASSERT_EQUALS(res2v, 4LL , PRId64);

    MuIntValue res3 = ctx->atomicrmw(ctx, MU_ORD_SEQ_CST, MU_ARMW_ADD, r1i, I64_10);
    MuIntValue res4 = ctx->load(ctx, MU_ORD_SEQ_CST, r1i);

    int64_t res3v = ctx->handle_to_sint64(ctx, res3);
    int64_t res4v = ctx->handle_to_sint64(ctx, res4);

    MU_ASSERT_EQUALS(res3v, 4LL , PRId64);
    MU_ASSERT_EQUALS(res4v, 14LL, PRId64);

    ctx->fence(ctx, MU_ORD_SEQ_CST);

    ctx->close_context(ctx);

    return true;
}

void osr_bundle_trap_handler(MuCtx *ctx, MuThreadRefValue thread,
        MuStackRefValue stack, MuWPID wpid, MuTrapHandlerResult *result,
        MuStackRefValue *new_stack, MuValue **values, int *nvalues,
        MuValuesFreer *freer, MuCPtr *freerdata, MuRefValue *exception,
        MuCPtr userdata) {

    muprintf("Hi! I am the native trap handler!\n");

    MuFCRefValue *cursor = ctx->new_cursor(ctx, stack);
    MuID iid  = ctx->cur_inst(ctx, cursor);

    if (iid == ID("@sum_v1.opt.trap_opt")) {
        MuID fid  = ctx->cur_func(ctx, cursor);
        MuID fvid = ctx->cur_func_ver(ctx, cursor);
        MU_ASSERT_EQUALS_TRAP(fid , ID("@sum"), "d");
        MU_ASSERT_EQUALS_TRAP(fvid, ID("@sum_v1"), "d");

        MuValue kas[3];
        ctx->dump_keepalives(ctx, cursor, kas);

        int64_t nv = ctx->handle_to_sint64(ctx, kas[0]);
        int64_t iv = ctx->handle_to_sint64(ctx, kas[1]);
        int64_t sv = ctx->handle_to_sint64(ctx, kas[2]);

        MU_ASSERT_EQUALS_TRAP(nv, 8LL, PRId64);
        MU_ASSERT_EQUALS_TRAP(iv, 5LL, PRId64);
        MU_ASSERT_EQUALS_TRAP(sv, 10LL, PRId64);

        muprintf("Prepare to load new bundle...\n");

        {
            char bundle[4096];

            muprintf("Opening file...\n");
            FILE *fp = fopen("tests/uvm-refimpl-test/osr-tests-part2.uir", "r");
            muprintf("File opened.\n");

            if (fp == NULL) {
                perror("fp is NULL");
                exit(1);
            }

            int already_read = 0;

            while(!feof(fp)) {
                already_read += fread(bundle+already_read, 1, 4096, fp);
            }

            fclose(fp);

            muprintf("Loading bundle...\n");
            ctx->load_bundle(ctx, bundle, already_read);
        }

        muprintf("Loaded. Perform OSR...\n");

        muprintf("Popping...\n");

        ctx->next_frame(ctx, cursor);
        ctx->pop_frames_to(ctx, cursor);
        ctx->close_cursor(ctx, cursor);

        muprintf("Pushing...\n");

        MuFuncRefValue one_shot_func = ctx->handle_from_func(ctx, ID("@sum_osr_oneshot"));
        ctx->push_frame(ctx, stack, one_shot_func);

        MuValue *rebind_values = (MuValue*)malloc(sizeof(MuValue)*3);
        rebind_values[0] = kas[2];
        rebind_values[1] = kas[1];
        rebind_values[2] = kas[0];

        muprintf("Prepare to return from trap handler...\n");
        *result = MU_REBIND_PASS_VALUES;
        *new_stack = stack;
        *values = rebind_values;
        *nvalues = 3;
        *freer = malloc_freer;
        *freerdata = NULL;
        muprintf("Bye!\n");
    } else if (iid == ID("@osr_test_base_v1.entry.trap_base_exit")) {
        MuValue kas[1];
        ctx->dump_keepalives(ctx, cursor, kas);
        ctx->close_cursor(ctx, cursor);

        int64_t rv = ctx->handle_to_sint64(ctx, kas[0]);
        MU_ASSERT_EQUALS_TRAP(rv, 28LL, PRId64);

        muprintf("Prepare to return from trap handler...\n");
        *result = MU_REBIND_PASS_VALUES;
        *new_stack = stack;
        *values = NULL;
        *nvalues = 0;
        *freer = NULL;
        *freerdata = NULL;
        muprintf("Bye!\n");
    } else {
        muprintf("Unknown trap. ID: %d\n", iid);
        MuName trapName = NAME(iid);
        muprintf("name: %s\n", trapName);
        exit(1);
    }
}

bool test_osr(MuVM *mvm) {
    mvm->set_trap_handler(mvm, osr_bundle_trap_handler, NULL);

    MuCtx *ctx = mvm->new_context(mvm);

    {
        char bundle[4096];

        FILE *fp = fopen("tests/uvm-refimpl-test/osr-tests.uir", "r");
        int already_read = 0;

        while(!feof(fp)) {
            already_read += fread(bundle+already_read, 1, 4096, fp);
        }

        fclose(fp);

        ctx->load_bundle(ctx, bundle, already_read);
    }

    MuValue args[1];
    args[0] = ctx->handle_from_sint64(ctx, 8LL, 64);

    MuFuncRefValue func = ctx->handle_from_func(ctx, ID("@osr_test_base"));
    MuStackRefValue stack = ctx->new_stack(ctx, func);
    MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, args, 1);

    mvm->execute(mvm);

    ctx->close_context(ctx);

    return true;
}

bool test_tr64(MuVM *mvm) {
    MuCtx *ctx = mvm->new_context(mvm);

    MuDoubleValue hf = ctx->handle_from_double(ctx, 3.14);
    MuIntValue    hi = ctx->handle_from_sint64(ctx, 0x123456789abcdLL, 52);
    MuRefValue    hr = ctx->new_fixed(ctx, ID("@i32"));
    MuIntValue    ht = ctx->handle_from_sint8(ctx, 15, 6);

    MuTagRef64Value tf = ctx->tr64_from_fp (ctx, hf);
    MuTagRef64Value ti = ctx->tr64_from_int(ctx, hi);
    MuTagRef64Value tr = ctx->tr64_from_ref(ctx, hr, ht);

    int fisf = ctx->tr64_is_fp (ctx, tf);
    int fisi = ctx->tr64_is_int(ctx, tf);
    int fisr = ctx->tr64_is_ref(ctx, tf);
    int iisf = ctx->tr64_is_fp (ctx, ti);
    int iisi = ctx->tr64_is_int(ctx, ti);
    int iisr = ctx->tr64_is_ref(ctx, ti);
    int risf = ctx->tr64_is_fp (ctx, tr);
    int risi = ctx->tr64_is_int(ctx, tr);
    int risr = ctx->tr64_is_ref(ctx, tr);

    MU_ASSERT_EQUALS(fisf, 1, "d");
    MU_ASSERT_EQUALS(fisi, 0, "d");
    MU_ASSERT_EQUALS(fisr, 0, "d");
    MU_ASSERT_EQUALS(iisf, 0, "d");
    MU_ASSERT_EQUALS(iisi, 1, "d");
    MU_ASSERT_EQUALS(iisr, 0, "d");
    MU_ASSERT_EQUALS(risf, 0, "d");
    MU_ASSERT_EQUALS(risi, 0, "d");
    MU_ASSERT_EQUALS(risr, 1, "d");

    MuDoubleValue hfb = ctx->tr64_to_fp (ctx, tf);
    MuIntValue    hib = ctx->tr64_to_int(ctx, ti);
    MuRefValue    hrb = ctx->tr64_to_ref(ctx, tr);
    MuIntValue    htb = ctx->tr64_to_tag(ctx, tr);

    double  fb = ctx->handle_to_double(ctx, hfb);
    int64_t ib = ctx->handle_to_sint64(ctx, hib);
    int8_t  tb = ctx->handle_to_sint8 (ctx, htb);

    MU_ASSERT_EQUALS(fb, 3.14, "lf");
    MU_ASSERT_EQUALS(ib, 0x123456789abcdLL, PRIx64);
    MU_ASSERT_EQUALS(tb, 15, PRId32);   // printf implcitly converts to int.

    if (!ctx->ref_eq(ctx, hrb, hr)) {
        muprintf("hrb is not equal to hr\n");
        return false;
    }

    ctx->close_context(ctx);

    return true;
}

struct wp_trap_data {
    int which;
};

void wp_trap_handler(MuCtx *ctx, MuThreadRefValue thread,
        MuStackRefValue stack, MuWPID wpid, MuTrapHandlerResult *result,
        MuStackRefValue *new_stack, MuValue **values, int *nvalues,
        MuValuesFreer *freer, MuCPtr *freerdata, MuRefValue *exception,
        MuCPtr userdata) {

    muprintf("Hi! I am the native trap handler!\n");

    struct wp_trap_data *data = (struct wp_trap_data*)userdata;
    int which = data->which;

    MuFCRefValue *cursor = ctx->new_cursor(ctx, stack);
    MuID iid  = ctx->cur_inst(ctx, cursor);
    ctx->close_cursor(ctx, cursor);

    if (iid == ID("@wptest.v1.entry.wp")) {
        MU_ASSERT_EQUALS_TRAP(wpid,  44, "d");
        MU_ASSERT_EQUALS_TRAP(which,  1, "d");

        muprintf("Prepare to stop thread...\n");
        *result = MU_THREAD_EXIT;
        muprintf("Omae wa mou shindeiru!\n");
    } else if (iid == ID("@wptest.v1.dis.trap")) {
        MU_ASSERT_EQUALS_TRAP(which,  0, "d");

        muprintf("Prepare to stop thread...\n");
        *result = MU_THREAD_EXIT;
        muprintf("Omae wa mou shindeiru!\n");
    } else {
        muprintf("Unknown trap. ID: %d\n", iid);
        MuName trapName = NAME(iid);
        muprintf("name: %s\n", trapName);
        exit(1);
    }
}


bool test_wp(MuVM *mvm) {
    struct wp_trap_data data = {0};

    mvm->set_trap_handler(mvm, wp_trap_handler, &data);

    MuCtx *ctx = mvm->new_context(mvm);

    MuFuncRefValue func = ctx->handle_from_func(ctx, ID("@wptest"));

    {
        MuStackRefValue stack = ctx->new_stack(ctx, func);
        MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, NULL, 0);

        mvm->execute(mvm);
    }

    ctx->enable_watchpoint(ctx, 44);
    data.which = 1;

    {
        MuStackRefValue stack = ctx->new_stack(ctx, func);
        MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, NULL, 0);

        mvm->execute(mvm);
    }

    ctx->disable_watchpoint(ctx, 44);
    data.which = 0;

    {
        MuStackRefValue stack = ctx->new_stack(ctx, func);
        MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, NULL, 0);

        mvm->execute(mvm);
    }

    ctx->close_context(ctx);

    return true;
}

int (*plus_two)(int n);

int native_callback(int v2) {
    muprintf("Mu called back. v2=%d\n", v2);
    MU_ASSERT_EQUALS_TRAP(v2, 43, "d");

    muprintf("Trying to call plus_two(9)\n");
    int result = plus_two(9);
    muprintf("plus_two returns %d\n", result);
    MU_ASSERT_EQUALS_TRAP(result, 11, "d");

    return v2+1;
}

void native_trap_handler(MuCtx *ctx, MuThreadRefValue thread,
        MuStackRefValue stack, MuWPID wpid, MuTrapHandlerResult *result,
        MuStackRefValue *new_stack, MuValue **values, int *nvalues,
        MuValuesFreer *freer, MuCPtr *freerdata, MuRefValue *exception,
        MuCPtr userdata) {

    muprintf("Hi! I am the native trap handler!\n");

    MuFCRefValue *cursor = ctx->new_cursor(ctx, stack);
    MuID iid = ctx->cur_inst(ctx, cursor);
    MU_ASSERT_EQUALS_TRAP(iid, ID("@native_test.v1.entry.trap"), "d");

    MuValue kas[1];
    ctx->dump_keepalives(ctx, cursor, kas);
    ctx->close_cursor(ctx, cursor);

    int32_t rv = ctx->handle_to_sint32(ctx, kas[0]);
    MU_ASSERT_EQUALS_TRAP(rv, 44, PRId32);

    muprintf("Prepare to return from trap handler...\n");
    *result = MU_REBIND_PASS_VALUES;
    *new_stack = stack;
    *values = NULL;
    *nvalues = 0;
    *freer = NULL;
    *freerdata = NULL;
    muprintf("Bye!\n");
}

bool test_native(MuVM *mvm) {
    mvm->set_trap_handler(mvm, native_trap_handler, NULL);

    MuCtx *ctx = mvm->new_context(mvm);

    // Expose the @plus_two Mu function

    MuFuncRefValue hplus_two = ctx->handle_from_func(ctx, ID("@plus_two"));
    MuIntValue     hcookie   = ctx->handle_from_sint64(ctx, 2LL, 64);
    MuUFPValue hplus_two_fp  = ctx->expose(ctx, hplus_two, MU_CC_DEFAULT, hcookie);

    MuCFP plus_two_fp = ctx->handle_to_fp(ctx, hplus_two_fp);
    plus_two = (int(*)(int))plus_two_fp;

    // Prepare the native_callback C function for Mu to call
    
    MuIRefValue hg_native_callback  = ctx->handle_from_global(ctx, ID("@g_native_callback"));
    MuUFPValue  hnative_callback_fp = ctx->handle_from_fp(ctx, ID("@native_callback.fp"), (MuCFP)native_callback);
    ctx->store(ctx, MU_ORD_NOT_ATOMIC, hg_native_callback, hnative_callback_fp);

    // Preapare an object for the @native_test function. Pin the object to write
    // to it in C assignment expression.

    MuRefValue  obj = ctx->new_fixed(ctx, ID("@i32"));
    MuUPtrValue ptr = ctx->pin(ctx, obj);
    
    int *p = (int*)ctx->handle_to_ptr(ctx, ptr);
    *p = 42;
    ctx->unpin(ctx, obj);

    MuValue args[1] = { obj };

    // Prepare to run the test.

    MuFuncRefValue func = ctx->handle_from_func(ctx, ID("@native_test"));

    MuStackRefValue stack = ctx->new_stack(ctx, func);
    MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, args, 1);

    mvm->execute(mvm);

    ctx->unexpose(ctx, MU_CC_DEFAULT, hplus_two_fp);

    ctx->close_context(ctx);

    return true;
}

