#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>     // write

#include <refimpl2-start.h>
#include <muapi.h>

const char *hw_string = "Hello world!\n";

const char *gc_conf =
"sosSize=524288\n"
"losSize=524288\n"
"globalSize=1048576\n"
"stackSize=32768\n"
;


int main() {
    MuVM *mvm = mu_refimpl2_new_ex(gc_conf);

    MuCtx *ctx = mvm->new_context(mvm);

    char *bundle1 = 
        ".typedef @cint = int<32>\n"
        ".typedef @void = void\n"
        ".typedef @cvoidptr = uptr<@void>\n"
        ".typedef @csize_t = int<64>\n"
        ".funcsig @write.sig = (@cint @cvoidptr @csize_t) -> (@csize_t)\n"
        ".typedef @write.fp  = ufuncptr<@write.sig>\n"
        ".const @the_fd <@cint> = 1\n"
        ;

    char bundle2[256];
    sprintf(bundle2,
            ".const @the_write  <@write.fp> = 0x%lx\n"
            ".const @the_string <@cvoidptr> = 0x%lx\n"
            ".const @the_length <@csize_t>  = 0x%lx\n"
            , (uintptr_t)write, (uintptr_t)hw_string, (unsigned long)strlen(hw_string));
        
    char *bundle3 = 
        ".funcsig @v_v = ()->()\n"
        ".funcdef @hw VERSION %1 <@v_v> {\n"
        "  %entry():\n"
        "    %rv = CCALL #DEFAULT <@write.fp @write.sig> @the_write (@the_fd @the_string @the_length)\n"
        "    COMMINST @uvm.thread_exit\n"
        "}\n"
        ;

    printf("Loading bundles...\n");

    printf("Bundle1:\n%s\n", bundle1);
    ctx->load_bundle(ctx, bundle1, strlen(bundle1));
    printf("Bundle2:\n%s\n", bundle2);
    ctx->load_bundle(ctx, bundle2, strlen(bundle2));
    printf("Bundle3:\n%s\n", bundle3);
    ctx->load_bundle(ctx, bundle3, strlen(bundle3));

    printf("Bundles loaded. Execute...\n");

    MuFuncRefValue   func   = ctx->handle_from_func(ctx, ctx->id_of(ctx, "@hw"));
    MuStackRefValue  stack  = ctx->new_stack(ctx, func);
    MuThreadRefValue thread = ctx->new_thread(ctx, stack, MU_REBIND_PASS_VALUES,
            NULL, 0, NULL);

    mvm->execute(mvm);

    mu_refimpl2_close(mvm);

    return 0;
}
