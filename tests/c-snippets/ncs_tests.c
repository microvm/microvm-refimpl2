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
        MuStackRefValue stack, int wpid, MuTrapHandlerResult *result,
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
    MuThreadRefValue thread = ctx->new_thread(ctx, stack, MU_REBIND_PASS_VALUES,
            args, 1, NULL);

    mvm->execute(mvm);

    ctx->close_context(ctx);

    return true;
}
