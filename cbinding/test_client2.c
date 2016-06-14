#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include <unistd.h>     // write

#include <refimpl2-start.h>
#include <muapi.h>

const char *hw_string = "Hello world!\n";

const char *gc_conf =
"sosSize=524288\n"
"losSize=524288\n"
"globalSize=1048576\n"
"stackSize=32768\n"
"vmLog=DEBUG\n"
;

int *muerrno;

#define MUCHECKERR() do { if(*muerrno) { \
        fprintf(stderr, "Line %d: Error thrown in Mu. Stop. **muerrno: %d\n", \
                __LINE__, *muerrno); \
        exit(1); \
    }} while(0)

int main(int argc, char** argv) {
    MuVM *mvm = mu_refimpl2_new_ex(gc_conf);

    muerrno = mvm->get_mu_error_ptr(mvm);

    MuCtx *ctx = mvm->new_context(mvm);
    
    MUCHECKERR();

    MuBundleNode b = ctx->new_bundle(ctx);

    // primitive types

    MuTypeNode i1   = ctx->new_type_int(ctx, b, 1);
    MuTypeNode i8   = ctx->new_type_int(ctx, b, 8);
    MuTypeNode i32  = ctx->new_type_int(ctx, b, 32);
    MuTypeNode i64  = ctx->new_type_int(ctx, b, 64);
    MuTypeNode pi8  = ctx->new_type_uptr(ctx, b);
    MuTypeNode ppi8 = ctx->new_type_uptr(ctx, b);
    ctx->set_type_uptr(ctx, pi8,  i8);
    ctx->set_type_uptr(ctx, ppi8, pi8);

    MuID i32_id  = ctx->get_id(ctx, b, i32);
    MuID ppi8_id = ctx->get_id(ctx, b, ppi8);

    // signature for the main function

    MuTypeNode    main_paramtys[] = { i32, ppi8 };
    MuFuncSigNode main_sig        = ctx->new_funcsig(ctx, b, main_paramtys, 2,
            NULL, 0);

    // signature and ufuncptr for the write function
    
    MuTypeNode hvoid = ctx->new_type_void(ctx, b);
    MuTypeNode pvoid = ctx->new_type_uptr(ctx, b);
    ctx->set_type_uptr(ctx, pvoid, hvoid);

    MuTypeNode    write_paramtys[] = { i32, pvoid, i64 };
    MuTypeNode    write_rettys[]   = { i64 };
    MuFuncSigNode write_sig        = ctx->new_funcsig(ctx, b, write_paramtys, 3,
            write_rettys, 1);
    MuTypeNode    write_fp         = ctx->new_type_ufuncptr(ctx, b);
    ctx->set_type_ufuncptr(ctx, write_fp, write_sig);

    MuConstNode const_fp_write = ctx->new_const_int(ctx, b, write_fp, (uintptr_t)write);

    // constant for "Hello world!\n"

    MuConstNode const_ptr_hw_string = ctx->new_const_int(ctx, b, pi8, (uintptr_t)hw_string);

    // the exit state of the Mu function. It cannot directly return to C.

    MuGlobalNode exit_status = ctx->new_global_cell(ctx, b, i32);

    // the main function

    MuFuncNode main_func    = ctx->new_func(ctx, b, main_sig);
    MuID       main_func_id = ctx->get_id(ctx, b, main_func);

    MuFuncVerNode main_fv = ctx->new_func_ver(ctx, b, main_func);

    MuBBNode entry = ctx->new_bb(ctx, main_fv);
    MuNorParamNode hargc = ctx->new_nor_param(ctx, entry, i32);
    MuNorParamNode hargv = ctx->new_nor_param(ctx, entry, ppi8);
    
    // args to write. fd=1, sz=size of hw_string
    MuConstNode const_i32_1    = ctx->new_const_int(ctx, b, i32, 1L);
    MuConstNode const_i64_hwsz = ctx->new_const_int(ctx, b, i64, strlen(hw_string)+1);

    // convert the string pointer to void*
    MuInstNode    ptrcast   = ctx->new_conv(ctx, entry, MU_CONV_PTRCAST,
            ppi8, pvoid, const_ptr_hw_string);
    MuInstResNode ptrcast_r = ctx->new_inst_res(ctx, ptrcast);

    // call the write function
    MuVarNode          write_args[] = { const_i32_1, ptrcast_r, const_i64_hwsz };
    MuInstNode         write        = ctx->new_ccall(ctx, entry, MU_CC_DEFAULT,
            write_fp, write_sig, const_fp_write, write_args, 3);
    MuInstResNode      write_r0     = ctx->new_inst_res(ctx, write);

    // return 0
    MuConstNode const_i32_0 = ctx->new_const_int(ctx, b, i32, 0L);
    MuInstNode store = ctx->new_store(ctx, entry, 0, MU_ORD_NOT_ATOMIC, i32,
            exit_status, const_i32_0);

    // thread_exit
    MuInstNode threadexit = ctx->new_comminst(ctx, entry, MU_CI_UVM_THREAD_EXIT,
            NULL, 0, NULL, 0, NULL, 0, NULL, 0);

    ctx->load_bundle_from_node(ctx, b);

    MUCHECKERR();

    MuIntValue  v_argc = ctx->handle_from_sint32(ctx, i32_id,  argc);
    MuUPtrValue v_argv = ctx->handle_from_ptr(ctx, ppi8_id, (MuCPtr)argv);

    MuValue v_main_args[] = {v_argc, v_argv};

    MuFuncRefValue   v_main_func   = ctx->handle_from_func(ctx, main_func_id);
    MuStackRefValue  v_main_stack  = ctx->new_stack(ctx, v_main_func);
    MuThreadRefValue v_main_thread = ctx->new_thread_nor(ctx, v_main_stack,
            NULL, v_main_args, 2);

    MUCHECKERR();

    mvm->execute(mvm);

    mu_refimpl2_close(mvm);

    return 0;
}
