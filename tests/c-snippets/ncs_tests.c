#include <stdio.h>
#include <string.h>

#include <muapi.h>

int test_basic(MuVM *mvm, int theID, char *theName) {
    printf("[C] header: %p\n", mvm->header);
    printf("[C] new_context: %p\n", mvm->new_context);
    printf("[C] id_of: %p\n", mvm->id_of);
    printf("[C] name_of: %p\n", mvm->name_of);
    printf("[C] set_trap_handler: %p\n", mvm->set_trap_handler);

    int id = mvm->id_of(mvm, theName);
    printf("[C] id = %d\n", id);
    if (id != theID) {
        printf("[C] ID %d is not equal to %d\n", id, theID);
        return 0;
    }

    char *name = mvm->name_of(mvm, theID);
    printf("[C] name = %s\n", name);
    if (strcmp(name, theName) != 0) {
        printf("[C] name %s is not equal to %s\n", name, theName);
        return 0;
    }

    return 1;
}

int test_with_ctx(MuVM *mvm, int theID, char *theName) {
    MuCtx *ctx = mvm->new_context(mvm);

    int id = ctx->id_of(ctx, theName);
    printf("[C] id = %d\n", id);
    if (id != theID) {
        printf("[C] ID %d is not equal to %d\n", id, theID);
        return 0;
    }

    char *name = ctx->name_of(ctx, theID);
    printf("[C] name = %s\n", name);
    if (strcmp(name, theName) != 0) {
        printf("[C] name %s is not equal to %s\n", name, theName);
        return 0;
    }


    ctx->close_context(ctx);

    return 1;
}
