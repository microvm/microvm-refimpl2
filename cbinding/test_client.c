#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>     // write

#include <refimpl2-start.h>
#include <muapi.h>

const char *hw_string = "Hello world!\n";
const char *hw2_string = "Goodbye world!\n";

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
            ".const @the_length2 <@csize_t>  = 0x%lx\n"
            , (uintptr_t)write, (uintptr_t)hw_string,
            (unsigned long)strlen(hw_string), (unsigned long)strlen(hw2_string));
        
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
    MuThreadRefValue thread = ctx->new_thread_nor(ctx, stack, NULL, NULL, 0);

    mvm->execute(mvm);

    char *bundle4 = 
        ".typedef @cchar = int<8>\n"
        ".typedef @charhybrid = hybrid<@cchar>\n"
        ".typedef @refvoid = ref<@void>\n"
        ".funcdef @hw2 VERSION %1 <@v_v> {\n"
        "  %entry():\n"
        "    %tl = COMMINST @uvm.get_threadlocal\n"
        "    %p  = COMMINST @uvm.native.pin <@refvoid> (%tl)\n"
        "    %rv = CCALL #DEFAULT <@write.fp @write.sig> @the_write (@the_fd %p @the_length2)\n"
        "    COMMINST @uvm.native.unpin <@refvoid> (%tl)\n"
        "    COMMINST @uvm.thread_exit\n"
        "}\n"
        ;

    printf("Loading additional bundle...\n");
    ctx->load_bundle(ctx, bundle4, strlen(bundle4));

    printf("Bundle loaded. Create a thread-local string object...\n");
    MuIntValue hlen = ctx->handle_from_sint32(ctx, 256, 32);
    MuRefValue hobj = ctx->new_hybrid(ctx, ctx->id_of(ctx, "@charhybrid"), hlen);
    MuUPtrValue hpobj = ctx->pin(ctx, hobj);
    char *mustrbuf = (char*)ctx->handle_to_ptr(ctx, hpobj);
    strcpy(mustrbuf, hw2_string);
    ctx->unpin(ctx, hobj);

    printf("Object populated. Create thread with threadlocal and execute...\n");
    MuFuncRefValue   func2   = ctx->handle_from_func(ctx, ctx->id_of(ctx, "@hw2"));
    MuStackRefValue  stack2  = ctx->new_stack(ctx, func2);
    MuThreadRefValue thread2 = ctx->new_thread_nor(ctx, stack2, hobj, NULL, 0);

    mvm->execute(mvm);


    mu_refimpl2_close(mvm);

    return 0;
}
